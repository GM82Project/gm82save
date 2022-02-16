#![feature(asm_sym, naked_functions)]

#[cfg(not(all(windows, target_arch = "x86")))]
compile_error!("this tool only works on windows 32-bit");

mod asset;
#[macro_use]
mod delphi;
mod events;
mod ide;
mod list;
mod load;
mod save;
mod stub;

use crate::delphi::UStr;
use std::{
    arch::asm,
    collections::{HashMap, HashSet},
    ffi::c_void,
    io::Write,
    path::PathBuf,
    ptr,
};

#[derive(Debug)]
pub enum Error {
    IoError(std::io::Error),
    FileIoError(std::io::Error, PathBuf),
    DirIoError(std::io::Error, PathBuf),
    PngDecodeError(PathBuf, png::DecodingError),
    UnicodeError(String),
    AssetNotFound(String),
    SyntaxError(PathBuf),
    UnknownKey(PathBuf, String),
    UnknownAction(u32, u32),
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError),
    InvalidVersion(String),
    DuplicateAsset(String),
    DuplicateIncludedFile(String),
    DuplicateTrigger(String),
    OldGM82,
    Other(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IoError(e) => write!(f, "io error: {}", e),
            Self::FileIoError(e, p) => write!(f, "io error in file {}: {}", p.to_string_lossy(), e),
            Self::DirIoError(e, p) => write!(f, "io error in directory {}: {}", p.to_string_lossy(), e),
            Self::PngDecodeError(p, e) => write!(f, "couldn't decode image {}: {}", p.to_string_lossy(), e),
            Self::UnicodeError(s) => write!(f, "couldn't encode {}", s),
            Self::AssetNotFound(s) => write!(f, "couldn't find asset {}", s),
            Self::SyntaxError(p) => write!(f, "syntax error in file {}", p.to_string_lossy()),
            Self::UnknownKey(p, k) => write!(f, "unknown key in {}: {}", p.to_string_lossy(), k),
            Self::UnknownAction(lib_id, act_id) => write!(f, "unknown action {} in lib with id {}", act_id, lib_id),
            Self::ParseIntError(e) => write!(f, "integer parse error: {}", e),
            Self::ParseFloatError(e) => write!(f, "float parse error: {}", e),
            Self::InvalidVersion(v) => write!(f, "invalid exe_version {}", v),
            Self::DuplicateAsset(n) => write!(f, "multiple assets named {}", n),
            Self::DuplicateIncludedFile(n) => write!(f, "multiple included files named {}", n),
            Self::DuplicateTrigger(n) => write!(f, "multiple triggers named {}", n),
            Self::OldGM82 => write!(f, "this project was made with a newer version of gm82save, please update"),
            Self::Other(s) => write!(f, "other error: {}", s),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Error::IoError(err)
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(err: std::num::ParseIntError) -> Self {
        Error::ParseIntError(err)
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(err: std::num::ParseFloatError) -> Self {
        Error::ParseFloatError(err)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

const ACTION_TOKEN: &str = "/*\"/*'/**//* YYD ACTION";

fn show_message(msg: &str) {
    unsafe {
        delphi::ShowMessage(&UStr::new(msg));
    }
}

#[cfg(not(feature = "smooth_progress_bar"))]
struct FakeSender;
#[cfg(not(feature = "smooth_progress_bar"))]
impl FakeSender {
    fn send(&self, _: ()) {}
}
#[cfg(not(feature = "smooth_progress_bar"))]
fn run_while_updating_bar<OP>(_bar_start: u32, _bar_end: u32, _count: u32, op: OP) -> Result<()>
where
    OP: Fn(FakeSender) -> Result<()> + Sync + Send,
{
    op(FakeSender)
}

#[cfg(feature = "smooth_progress_bar")]
fn run_while_updating_bar<OP>(bar_start: u32, bar_end: u32, count: u32, op: OP) -> Result<()>
where
    OP: Fn(crossbeam_channel::Sender<()>) -> Result<()> + Send + Sync,
{
    if count > 0 {
        let (tx, rx) = crossbeam_channel::unbounded();
        let handle = {
            // this is basically what crossbeam does
            // note that the handle is joined at the end of the function
            // but it should be done executing by the time we get there anyway
            let f: Box<dyn FnOnce() -> Result<()> + Send> = Box::new(|| op(tx));
            let f: Box<dyn FnOnce() -> Result<()> + Send + 'static> = unsafe { std::mem::transmute(f) };
            std::thread::spawn(f)
        };
        let mut progress = 0;
        'outer: loop {
            'inner: loop {
                match rx.try_recv() {
                    Ok(()) => progress += 1,
                    Err(crossbeam_channel::TryRecvError::Empty) => break 'inner,
                    Err(_) => break 'outer,
                }
            }
            delphi::advance_progress_form(progress * (bar_end - bar_start) / count + bar_start);
            // if this errors, it'll error next time too so no need to check
            if let Ok(()) = rx.recv_timeout(std::time::Duration::from_millis(20)) {
                progress += 1;
            }
        }
        handle.join().unwrap()
    } else {
        Ok(())
    }
}

static mut LAST_SAVE: f64 = 0.0;

fn update_timestamp() {
    unsafe {
        asm! {
        "call {call}",
        "fstp dword ptr [{output}]",
        call = in(reg) 0x4199b0,
        output = sym LAST_SAVE,
        lateout("eax") _,
        lateout("edx") _,
        lateout("ecx") _,
        }
    }
}

#[derive(Clone)]
struct InstanceExtra {
    pub xscale: f64,
    pub yscale: f64,
    pub blend: u32,
    pub angle: f64,
}

impl InstanceExtra {
    pub const DEFAULT: Self = Self { xscale: 1.0, yscale: 1.0, blend: u32::MAX, angle: 0.0 };
}

impl Default for InstanceExtra {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[derive(Clone)]
struct TileExtra {
    pub xscale: f64,
    pub yscale: f64,
    pub blend: u32,
}

impl TileExtra {
    pub const DEFAULT: Self = Self { xscale: 1.0, yscale: 1.0, blend: u32::MAX };
}

impl Default for TileExtra {
    fn default() -> Self {
        Self::DEFAULT
    }
}

unsafe extern "fastcall" fn reset_extra_data_inj() {
    EXTRA_DATA = None;
    let _: u32 = delphi_call!(0x6bc964); // reset triggers (what this overwrote)
}

static mut EXTRA_DATA: Option<(HashMap<usize, InstanceExtra>, HashMap<usize, TileExtra>)> = None;

unsafe extern "fastcall" fn about_inj(about_dialog: *const *const usize) {
    let info = UStr::new(concat!("gm82save: ", env!("BUILD_DATE")));
    let edition_label = *about_dialog.add(0xe5);
    asm! {
        "call {}",
        in(reg) 0x4ee6d8, // TControl.SetText
        inlateout("eax") edition_label => _,
        inlateout("edx") info.0 => _,
        lateout("ecx") _,
    }
}

#[naked]
unsafe extern "C" fn save_inj() {
    asm! {
        "mov ecx, ebp",
        "sub ecx, 4",
        "mov edx, ebp",
        "sub edx, 20",
        "jmp {}",
        sym save,
        options(noreturn),
    }
}

// set the high byte to nonzero if YYD save code was used
// set the low byte to nonzero on success
unsafe extern "fastcall" fn save(proj_path: &UStr, stream_ptr: *mut u32) -> u16 {
    const IS_YYD: u16 = 0x100;
    let path: PathBuf = proj_path.to_os_string().into();
    // filename ".gm82" works in the ui but rust doesn't get it so check for that specifically
    let is_gm82 = path.extension() == Some("gm82".as_ref()) || path.file_name() == Some(".gm82".as_ref());
    if !is_gm82 {
        // CStream.Create
        let buf = delphi_call!(0x405a4c, 0x52e8fc, 1);
        stream_ptr.write(buf);
        // save gmk
        let success: u32 = delphi_call!(0x705798, buf);
        return success as u16
    }

    if let Err(e) = save::save_gmk(path) {
        // display the error
        delphi::close_progress_form();
        show_message(&format!("Failed to save: {}", e));
        0 | IS_YYD
    } else {
        delphi::close_progress_form();
        1 | IS_YYD
    }
}

#[naked]
unsafe extern "C" fn load_inj() {
    asm! {
        "mov ecx, ebp",
        "sub ecx, 4",
        "mov edx, ebp",
        "sub edx, 12",
        "mov eax,ebp",
        "sub eax, 5",
        "push eax",
        "call {}",
        "ret",
        sym load,
        options(noreturn),
    };
}

unsafe extern "fastcall" fn load(proj_path: &UStr, stream_ptr: *mut u32, result_ptr: *mut bool) -> bool {
    SAW_APPLIES_TO_WARNING = false;
    let path: PathBuf = proj_path.to_os_string().into();
    // .gm82 works in the ui but rust doesn't get it so check for that specifically
    let is_gm82 = path.extension() == Some("gm82".as_ref()) || path.file_name() == Some(".gm82".as_ref());
    if !is_gm82 {
        let stream = delphi_call!(0x405a4c, 0x52e8fc, 1);
        stream_ptr.write(stream);
        return false
    }

    if let Err(e) = load::load_gmk(path) {
        // display the error and reload
        delphi::close_progress_form();
        show_message(&format!("Failed to load: {}", e));
        ide::initialize_project();
    } else {
        delphi::close_progress_form();
        result_ptr.write(true);
    }
    true
}

#[naked]
unsafe extern "C" fn gm81_or_gm82_inj() {
    asm! {
        "mov ecx, eax",
        "jmp {}",
        sym gm81_or_gm82,
        options(noreturn),
    }
}

unsafe extern "fastcall" fn gm81_or_gm82(s: *const u16) -> i32 {
    let s = UStr::from_ptr(&s);
    // test .gm81
    let out = delphi::CompareText(s, 0x6e0534 as _);
    // test .gm82
    if out != 0 { delphi::CompareText(s, 0x6dfbe4 as _) } else { out }
}

unsafe extern "fastcall" fn make_new_folder(_: u32, path_ptr: *const u16) {
    use load::UStrPtr;
    let path_delphi = UStr::from_ptr(&path_ptr);
    let mut path: PathBuf = path_delphi.to_os_string().into();
    // .gm82 works in the ui but rust doesn't get it so check for that specifically
    let is_gm82 = path.extension() == Some("gm82".as_ref()) || path.file_name() == Some(".gm82".as_ref());
    if is_gm82 && !path.is_file() {
        path.push(path.file_name().unwrap().to_owned());
    }
    ide::PROJECT_PATH.asg(path);
}

#[naked]
unsafe extern "C" fn fix_tile_null_pointer() {
    asm! {
        "mov edx, 0x64e048",
        "call edx",
        "mov edx, 0x68ef07",
        "mov ecx, 0x68ef6c",
        "test eax, eax",
        "cmovz edx, ecx",
        "jmp edx",
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn inflate_inj() {
    asm! {
        "mov ecx, eax",
        "jmp {}",
        sym inflate,
        options(noreturn),
    }
}

unsafe extern "fastcall" fn inflate(src: &delphi::TMemoryStream) -> &mut delphi::TMemoryStream {
    let dst = &mut *delphi::TMemoryStream::new();
    let mut size: usize = 0;
    src.read(&mut size as *mut usize as *mut u8, 4);
    let mut data = Vec::with_capacity(size);
    data.set_len(size);
    src.read(data.as_mut_ptr(), size as u32);
    let mut decoder = flate2::write::ZlibDecoder::new(dst);
    decoder.write_all(&data).unwrap();
    let dst = decoder.finish().unwrap();
    dst.set_pos(0);
    return dst
}

#[naked]
unsafe extern "C" fn deflate_inj() {
    asm! {
        "mov ecx, eax",
        "jmp {}",
        sym deflate,
        options(noreturn),
    }
}

unsafe extern "fastcall" fn deflate(dst: &mut delphi::TMemoryStream, src: &delphi::TMemoryStream) {
    let mut encoder = flate2::write::ZlibEncoder::new(Vec::new(), flate2::Compression::new(DEFLATE_LEVEL));
    encoder.write_all(src.get_slice()).unwrap();
    let data = encoder.finish().unwrap();
    let _ = dst.write(&data.len().to_le_bytes());
    let _ = dst.write(&data);
    src.set_pos(data.len() as _);
}

static mut DEFLATE_LEVEL: u32 = 6; // default

#[naked]
unsafe extern "C" fn build_small() {
    asm! {
        "mov ecx, 9",
        "mov {}, ecx",
        "mov ecx, 0x4cf2f4",
        "jmp ecx",
        sym DEFLATE_LEVEL,
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn build_fast() {
    asm! {
        "mov ecx, 1",
        "mov {}, ecx",
        "mov ecx, 0x41735c",
        "jmp ecx",
        sym DEFLATE_LEVEL,
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn reset_compression() {
    asm! {
        "mov ecx, 6",
        "mov {}, ecx",
        "mov ecx, 0x51cc64",
        "jmp ecx",
        sym DEFLATE_LEVEL,
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn save_82_if_exe() {
    // only saves settings version 825 when saving an exe with the creation code flag set
    asm! {
        "mov edx, 825",
        "mov ecx, 800",
        "test bl, bl", // if exe
        "cmovz edx, ecx",
        "bt word ptr [0x77f54e], 15", // if force cpu
        "cmovnc edx, ecx",
        "ret",
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn save_bool_if_exe() {
    asm! {
        "push esi",
        "mov esi, 0x52f240", // WriteBoolean
        "mov ecx, 0x52f12c", // WriteInteger
        "test bl, bl", // if exe
        "cmovnz ecx, esi",
        "call ecx",
        "pop esi",
        "ret",
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn save_creation_code_flag() {
    asm! {
        "mov ecx, 0x52f12c", // WriteInteger (for uninitialized args)
        "call ecx",
        "test bl, bl", // if exe
        "jz 1f",
        "bt word ptr [0x77f54e], 15", // if force cpu
        "jnc 1f",

        "mov eax, esi", // gmk stream
        "xor edx, edx", // 0 (webgl)
        "mov ecx, 0x52f12c", // WriteInteger
        "call ecx",
        "mov eax, esi", // gmk stream
        "mov dl, 1", // true (creation code)
        "mov ecx, 0x52f240", // WriteBoolean
        "call ecx",

        "1: ret",
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn save_room_version_inj() {
    asm! {
        "mov cl, byte ptr [esp]",
        "call {}",
        "mov edx, eax",
        "mov eax, 0x658372",
        "jmp eax",
        sym save_room_version,
        options(noreturn),
    }
}

unsafe extern "fastcall" fn save_room_version(exe: bool) -> u32 {
    if exe && EXTRA_DATA.is_some() { 811 } else { 541 }
}

#[naked]
unsafe extern "C" fn save_instance_extra_inj() {
    asm! {
        "mov ecx, ebx", // file
        "mov eax, dword ptr [edi + 0x2f4]", // instance list
        "mov edx, dword ptr [eax + ebp*0x8 + 0xc]", // instance id
        "xor eax, eax",
        "mov al, byte ptr [esp]", // are we exe?
        "push eax",
        "call {}",
        "inc esi",
        "mov eax, 0x658600", // jnz of loop
        "dec dword ptr [esp + 0x4]",
        "jmp eax",
        sym save_instance_extra,
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn save_tile_extra_inj() {
    asm! {
        "mov ecx, ebx", // file
        "mov eax, dword ptr [edi + 0x2fc]", // tile list
        "mov edx, dword ptr [eax + ebp*0x8 + 0x20]", // tile id
        "xor eax, eax",
        "mov al, byte ptr [esp]", // are we exe?
        "push eax",
        "call {}",
        "inc esi",
        "mov eax, 0x6586dd", // jnz of loop
        "dec dword ptr [esp + 0x4]",
        "jmp eax",
        sym save_tile_extra,
        options(noreturn),
    }
}

unsafe fn save_real(file: usize, real: &f64) {
    asm! {
        "push dword ptr [{real} + 0x4]",
        "push dword ptr [{real}]",
        "call {call}",
        call = in(reg) 0x52f140,
        real = in(reg) real,
        inlateout("eax") file => _,
        lateout("edx") _,
        lateout("ecx") _,
    }
}

unsafe extern "fastcall" fn save_instance_extra(file: usize, id: usize, exe: bool) {
    if exe {
        if let Some(data) = EXTRA_DATA.as_ref().map(|(insts, _)| insts.get(&id).unwrap_or(&InstanceExtra::DEFAULT)) {
            save_real(file, &data.xscale);
            save_real(file, &data.yscale);
            let _: u32 = delphi_call!(0x52f12c, file, data.blend);
            save_real(file, &data.angle);
        }
    }
}

unsafe extern "fastcall" fn save_tile_extra(file: usize, id: usize, exe: bool) {
    if exe {
        if let Some(data) = EXTRA_DATA.as_ref().map(|(_, tiles)| tiles.get(&id).unwrap_or(&TileExtra::DEFAULT)) {
            save_real(file, &data.xscale);
            save_real(file, &data.yscale);
            let _: u32 = delphi_call!(0x52f12c, file, data.blend);
        }
    }
}

unsafe extern "stdcall" fn freshen_room_ids(room: &mut asset::Room) {
    if let Some((instance_map, tile_map)) = EXTRA_DATA.as_mut() {
        let last_instance_id = *ide::LAST_INSTANCE_ID + 1;
        let instances = room.get_instances_mut();
        *ide::LAST_INSTANCE_ID += instances.len();
        for (i, instance) in instances.into_iter().enumerate() {
            let old_id = instance.id;
            instance.id = last_instance_id + i;
            if let Some(data) = instance_map.get(&old_id).cloned() {
                instance_map.insert(instance.id, data);
            }
        }
        let last_tile_id = *ide::LAST_TILE_ID + 1;
        let tiles = room.get_tiles_mut();
        *ide::LAST_TILE_ID += tiles.len();
        for (i, tile) in tiles.into_iter().enumerate() {
            let old_id = tile.id;
            tile.id = last_tile_id + i;
            if let Some(data) = tile_map.get(&old_id).cloned() {
                tile_map.insert(tile.id, data);
            }
        }
    }
}

#[naked]
unsafe extern "C" fn setup_unicode_parse_inj() {
    asm! {
        "mov ecx, edi",
        "call {}",
        "mov eax, 5",
        "ret",
        sym setup_unicode_parse,
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn teardown_unicode_parse_inj() {
    asm! {
        "mov ecx, 810",
        "call {}",
        "mov eax, 0x6ca2cc",
        "jmp eax",
        sym setup_unicode_parse,
        options(noreturn),
    }
}

unsafe extern "fastcall" fn setup_unicode_parse(version: i32) {
    // this just patches CStream.ReadString to read with the active code page instead of UTF-8
    // (and reverts that change after loading so nothing else breaks)
    let cp = if version < 810 { [0, 0] } else { [0xe9, 0xfd] };
    patch(0x52f0a2 as _, &cp);
    patch(0x52f0c5 as _, &cp);
}

#[naked]
unsafe extern "C" fn gm82_file_association_inj() {
    asm! {
        "mov ecx, eax",
        "jmp {}",
        sym gm82_file_association,
        options(noreturn),
    }
}

unsafe extern "fastcall" fn gm82_file_association(reg: u32) {
    let ext = UStr::new(r"\.gm82");
    let _: u32 = delphi_call!(0x6dd850, reg, ext.0, 0, UStr::new("gm82file").0);
    let _: u32 = delphi_call!(0x452568, reg, 0x80000001u32);
    let _: u32 = delphi_call!(0x6dd850, reg, ext.0, UStr::new(r"\Software\Classes").0, UStr::new("gm82file").0);
}

#[naked]
unsafe extern "C" fn room_form_inj() {
    asm! {
        "mov ecx, eax",
        "jmp {}",
        sym room_form,
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn room_size() {
    asm! {
        "mov eax, {width}",
        "mov dword ptr [ebx+0xc], eax",
        "mov eax, {height}",
        "mov dword ptr [ebx+0x10], eax",
        "mov eax, {speed}",
        "mov dword ptr [ebx+0x8], eax",
        // views
        "mov ecx, 7 * 0x38",
        "2:", // loop point
        "mov eax, dword ptr {width}", // width
        "mov dword ptr [ebx + 0x12c + ecx + 0xc], eax",  // view_w
        "mov dword ptr [ebx + 0x12c + ecx + 0x1c], eax", // port_w
        "mov eax, dword ptr {height}", // height
        "mov dword ptr [ebx + 0x12c + ecx + 0x10], eax", // view_h
        "mov dword ptr [ebx + 0x12c + ecx + 0x20], eax", // port_h
        "sub ecx, 0x38",
        "jge 2b",
        "ret",
        width = sym DEFAULT_ROOM_WIDTH,
        height = sym DEFAULT_ROOM_HEIGHT,
        speed = sym DEFAULT_ROOM_SPEED,
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn fix_broken_room_size() {
    asm! {
        // we already have speed in eax so do that one first
        "cmp eax, 0",
        "cmovz eax, {speed}",
        "mov {speed}, eax",
        // check width
        "mov eax, 800",
        "cmp dword ptr {width}, 0",
        "cmovnz eax, {width}",
        "mov {width}, eax",
        // check height
        "mov eax, 608",
        "cmp dword ptr {height}, 0",
        "cmovnz eax, {height}",
        "mov {height}, eax",
        "ret",
        width = sym DEFAULT_ROOM_WIDTH,
        height = sym DEFAULT_ROOM_HEIGHT,
        speed = sym DEFAULT_ROOM_SPEED,
        options(noreturn),
    }
}

static mut DEFAULT_ROOM_WIDTH: u32 = 800;
static mut DEFAULT_ROOM_HEIGHT: u32 = 608;
static mut DEFAULT_ROOM_SPEED: u32 = 50;

static mut SAW_APPLIES_TO_WARNING: bool = false;

static mut SAVING_FOR_ROOM_EDITOR: bool = false;

unsafe extern "fastcall" fn room_form(room_id: usize) -> u32 {
    if *(0x79a982 as *const bool) {
        return delphi_call!(0x6884c8, room_id)
    }
    let editor_path = match std::env::current_exe() {
        Ok(mut path) => {
            path.set_file_name("gm82room.exe");
            path
        },
        Err(_) => return delphi_call!(0x6884c8, room_id),
    };
    if editor_path.exists() {
        let mut room_path = PathBuf::from((&*ide::PROJECT_PATH).to_os_string());
        if room_path.extension() == Some("gm82".as_ref()) {
            // if we haven't loaded as gm82 before, sanitize instance and tile ids, as there may be duplicates
            if EXTRA_DATA.is_none() {
                EXTRA_DATA = Some(Default::default());
                let mut instance_ids = HashSet::new();
                let mut tile_ids = HashSet::new();
                // making rooms mutable would lead to weirdness so i guess i'm not
                for room in ide::get_rooms_mut()
                    .iter_mut()
                    .map(|r| std::mem::transmute::<Option<&asset::Room>, Option<&mut asset::Room>>(*r))
                    .flatten()
                {
                    // check if all ids are unique, and if not, update *all* the ids for consistency
                    let instances_ok = room.get_instances().iter().all(|inst| instance_ids.insert(inst.id));
                    let tiles_ok = room.get_tiles().iter().all(|tile| tile_ids.insert(tile.id));
                    if !instances_ok || !tiles_ok {
                        freshen_room_ids(room);
                    }
                }
                // force-save all open rooms just in case
                for form in ide::get_room_forms().iter().map(|&f| f as *mut *const u8) {
                    if !form.is_null() {
                        let room = *form.add(0x61c / 4);
                        let saveroom = *form.add(0x620 / 4);
                        let _: u32 = delphi_call!(0x657994, saveroom, room); // copy room to saveroom
                        let undoroom_ptr = form.add(0x62c / 4);
                        if !(*undoroom_ptr).is_null() {
                            let _: u32 = delphi_call!(0x405a7c, *undoroom_ptr); // free undo room
                            *undoroom_ptr = ptr::null();
                        }
                        form.cast::<bool>().add(0x628).write(false); // clear ischanged flag
                    }
                }
            }
            SAVING_FOR_ROOM_EDITOR = true;
            let _: u32 = delphi_call!(0x6e0540, *(0x790100 as *const u32)); // save
            SAVING_FOR_ROOM_EDITOR = false;
            if room_path.exists() {
                room_path.pop();
                if let Ok(asset_maps) = load::load_asset_maps(&mut room_path) {
                    room_path.push("rooms");
                    room_path.push(ide::get_room_names()[room_id].to_os_string());
                    let _: u32 = delphi_call!(0x51acd0, *(0x790100 as *const u32)); // hide main form
                    let _ = std::process::Command::new(editor_path).arg(&room_path).spawn().and_then(|mut c| c.wait());
                    let _: u32 = delphi_call!(0x51acd8, *(0x790100 as *const u32)); // show main form
                    {
                        let room = ide::get_rooms()[room_id].unwrap();
                        // remove this room's ids from the global thing
                        if let Some((extra_inst, extra_tile)) = EXTRA_DATA.as_mut() {
                            for inst in room.get_instances() {
                                extra_inst.remove(&inst.id);
                            }
                            for tile in room.get_tiles() {
                                extra_tile.remove(&tile.id);
                            }
                        }
                        let _: u32 = delphi_call!(0x657820, room); // clear room
                    }
                    ide::get_rooms_mut()[room_id] =
                        load::load_room(&mut room_path, &asset_maps).expect("loading the updated room failed").as_ref();
                    return 0
                }
            }
        }
    }
    delphi_call!(0x6884c8, room_id) // the default
}

unsafe fn patch(dest: *mut u8, source: &[u8]) {
    // the only winapi imports in the whole project, no need for crates
    #[allow(non_camel_case_types)]
    type PAGE_TYPE = u32;
    const PAGE_READWRITE: PAGE_TYPE = 0x04;
    type BOOL = i32;
    type HANDLE = isize;
    #[link(name = "kernel32")]
    extern "system" {
        fn VirtualProtect(
            lpaddress: *mut c_void,
            dwsize: usize,
            flnewprotect: PAGE_TYPE,
            lpfloldprotect: *mut PAGE_TYPE,
        ) -> BOOL;
        fn GetCurrentProcess() -> HANDLE;
        fn FlushInstructionCache<'a>(hprocess: HANDLE, lpbaseaddress: *const c_void, dwsize: usize) -> BOOL;
    }

    let mut old_protect = 0;
    VirtualProtect(dest.cast(), source.len(), PAGE_READWRITE, &mut old_protect);
    dest.copy_from(source.as_ptr(), source.len());
    VirtualProtect(dest.cast(), source.len(), old_protect, &mut old_protect);
    FlushInstructionCache(GetCurrentProcess(), dest.cast(), source.len());
}

unsafe fn patch_call(instr: *mut u8, proc: usize) {
    patch(instr.add(1), &(proc - (instr as usize + 5)).to_le_bytes());
}

#[cfg_attr(not(test), ctor::ctor)]
#[cfg_attr(test, allow(dead_code))]
unsafe fn injector() {
    std::panic::set_hook(Box::new(|info| {
        #[link(name = "user32")]
        extern "system" {
            fn MessageBoxW(hWnd: usize, lpText: *const u16, lpCaption: *const u16, uType: u32) -> i32;
        }
        let msg = info.to_string().encode_utf16().chain(std::iter::once(0)).collect::<Vec<_>>();
        MessageBoxW(0, msg.as_ptr(), std::ptr::null(), 0);

        std::process::exit(-1);
    }));

    // about dialog
    #[rustfmt::skip]
    patch(0x71be58 as _, &[
        0x8b, 0xc8, // mov ecx, eax
        0xe9, // jmp [nothing yet]
    ]);
    patch_call(0x71be5a as _, about_inj as _);

    // call save() instead of CStream.Create and the "save gmk" function
    let save_dest = 0x705cbd as *mut u8;
    #[rustfmt::skip]
    let mut save_patch = [
        0xe8, 0x00, 0x00, 0x00, 0x00, // call save (my save)
        0x84, 0xe4, // test ah,ah
        0x74, 0x0e, // je 0x705cd4 (after this patch)
        0x84, 0xc0, // test al,al
        0x74, 0x25, // je 0x705cef (after save fail)
        0xe9, 0x7e, 0x01, 0x00, 0x00, // jmp 0x705e4d (after save success)
    ];
    save_patch[1..5].copy_from_slice(&(save_inj as u32 - (save_dest as u32 + 5)).to_le_bytes());
    patch(save_dest, &save_patch);

    // call load() instead of CStream.Create
    // and insert a JZ to the post-load code (0x705af3)
    let load_dest = 0x705a42 as *mut u8;
    #[rustfmt::skip]
    let mut load_patch = [
        0xe8, 0x00, 0x00, 0x00, 0x00, // call load (my load)
        0x84, 0xc0, // test al,al
        0x0f, 0x85, 0xa4, 0x00, 0x00, 0x00, // jne 0x705af3 (after load)
    ];
    load_patch[1..5].copy_from_slice(&(load_inj as u32 - (load_dest as u32 + 5)).to_le_bytes());
    patch(load_dest, &load_patch);

    // check for .gm82 as well as .gm81 when dragging file onto game maker
    patch_call(0x6df7e2 as _, gm81_or_gm82_inj as _);
    // check for .gm82 as well as .gm81 in open file dialog
    patch_call(0x6e02ed as _, gm81_or_gm82_inj as _);
    // check for .gm82 as well as .gm81 in "rename if using an old file extension" code
    patch_call(0x6e0574 as _, gm81_or_gm82_inj as _);
    // replace now-unused .gm81 with .gm82
    patch(0x6dfbec as _, &[b'2']);
    // save new .gm82 projects to subfolder when using "save as" dialog
    patch_call(0x6e06b3 as _, make_new_folder as _);

    // fix stupid null pointer error
    patch(0x68ef02 as _, &[0xe9]);
    patch_call(0x68ef02 as _, fix_tile_null_pointer as _);

    // use zlib-ng for compression
    patch(0x52f34c as _, &[0xe9]);
    patch_call(0x52f34c as _, deflate_inj as _);
    patch(0x52f2e4 as _, &[0xe9]);
    patch_call(0x52f2e4 as _, inflate_inj as _);
    // build fast when making test build
    patch_call(0x6ce8a2 as _, build_fast as _);
    patch_call(0x6ce8cb as _, reset_compression as _);
    // build small when making release
    patch_call(0x6ce775 as _, build_small as _);
    patch_call(0x6ce78f as _, reset_compression as _);

    // save creation code flag (reusing the software vertex processing flag)
    // write 825 instead of 800 for settings version if saving exe
    patch(0x70997c as _, &[0xe8]);
    patch_call(0x70997c as _, save_82_if_exe as _);
    // call WriteBoolean instead of WriteInteger if saving exe
    patch_call(0x709a4f as _, save_bool_if_exe as _);
    // save extra info if saving exe
    patch_call(0x709c99 as _, save_creation_code_flag as _);

    // save extra data on instances and tiles
    // write 811 instead of 541 for room version if saving exe
    patch(0x65836d as _, &[0xe9]);
    patch_call(0x65836d as _, save_room_version_inj as _);
    // instance stuff
    patch(0x6585fb as _, &[0xe9]);
    patch_call(0x6585fb as _, save_instance_extra_inj as _);
    // tile stuff
    patch(0x6586d8 as _, &[0xe9]);
    patch_call(0x6586d8 as _, save_tile_extra_inj as _);
    // and reset that extra data when loading a new project
    patch_call(0x705982 as _, reset_extra_data_inj as _);

    // read text as ANSI on pre-8.1
    patch(0x70537b as _, &[0xe8]);
    patch_call(0x70537b as _, setup_unicode_parse_inj as _);
    // reset above
    patch_call(0x705acc as _, teardown_unicode_parse_inj as _);

    // .gm82 file associations
    patch_call(0x6ddacd as _, gm82_file_association_inj as _);

    // default room editor settings
    patch(0x657852 as _, &[0xe8, 0, 0, 0, 0, 0x90, 0x90]);
    patch_call(0x657852 as _, room_size as _);

    // nop out room view size stuff
    patch(0x657904 as _, &[0x90; 14]);
    patch(0x65791c as _, &[0x90; 14]);

    // replace ids in new room when duplicating
    #[rustfmt::skip]
    patch(0x692e72 as _, &[
        0x8b, 0x14, 0x98, // mov edx, [eax+ebx*4]
        0x8b, 0x04, 0xb0, // mov eax, [eax+esi*4]
        0x50, // push eax
        0xe8, 0x16, 0x4b, 0xfc, 0xff, // call CRoom.Assign
        0xe8, 0x00, 0x00, 0x00, 0x00, // call freshen_room_ids
        0x90, 0x90, 0x90, 0x90, 0x90, // nop padding
    ]);
    patch_call(0x692e7e as _, freshen_room_ids as _);

    // funky room editor shit
    patch_call(0x69319c as _, room_form_inj as _);
    // disable news (replace function with a ret)
    patch(0x62c224 as _, &[0xc3]);

    // configs for default room editor settings
    // force progress bar (replace check with nops)
    patch(0x6ca266 as _, &[0x90, 0x90]);
    // replace ShowProgress with DefRoomW
    patch(0x717cbc as _, b"\x08\0\0\0D\0e\0f\0R\0o\0o\0m\0W\0\0\0");
    patch(0x719350 as _, b"\x08\0\0\0D\0e\0f\0R\0o\0o\0m\0W\0\0\0");
    // replace NoWebsite with DefRoomH
    patch(0x7189e8 as _, b"\x08\0\0\0D\0e\0f\0R\0o\0o\0m\0H\0\0\0");
    patch(0x719e0c as _, b"\x08\0\0\0D\0e\0f\0R\0o\0o\0m\0H\0\0\0");
    // replace NewsBrowser with DefRoomS
    patch(0x718a24 as _, b"\x08\0\0\0D\0e\0f\0R\0o\0o\0m\0S\0\0\0");
    patch(0x719e48 as _, b"\x08\0\0\0D\0e\0f\0R\0o\0o\0m\0S\0\0\0");
    // read ShowProgress from reg as int
    patch(0x7170f7 as _, &[0x33, 0xd2]);
    patch(0x7170fe as _, &[0xe8, 0xcd, 0xfc, 0xff, 0xff, 0xa3]);
    patch(0x717104 as _, &(&DEFAULT_ROOM_WIDTH as *const u32 as usize as u32).to_le_bytes());
    // read NoWebsite from reg as int
    patch(0x71799d as _, &[0xe8, 0x2e, 0xf4, 0xff, 0xff, 0xa3]);
    patch(0x7179a3 as _, &(&DEFAULT_ROOM_HEIGHT as *const u32 as usize as u32).to_le_bytes());
    // read NewsBrowser from reg as int
    patch(0x7179bf as _, &[0xe8, 0x0c, 0xf4, 0xff, 0xff, 0xe8]);
    patch_call(0x7179c4 as _, fix_broken_room_size as _);
    // write ShowProgress to reg as int
    patch(0x718bac as _, &[0x8b, 0x15, 0x2c, 0xa8, 0x79, 0x00, 0x90]);
    patch(0x718bb9 as _, &[0x43, 0xe0]);
    patch(0x718bae as _, &(&DEFAULT_ROOM_WIDTH as *const u32 as usize as u32).to_le_bytes());
    // write NoWebsite to reg as int
    patch(0x71908e as _, &[0x8b, 0x15, 0x81, 0xa9, 0x79, 0x00, 0x90]);
    patch(0x71909b as _, &[0x61, 0xdb]);
    patch(0x719090 as _, &(&DEFAULT_ROOM_HEIGHT as *const u32 as usize as u32).to_le_bytes());
    // write NewsBrowser to reg as int
    patch(0x7190b0 as _, &[0x8b, 0x15, 0x83, 0xa9, 0x79, 0x00, 0x90]);
    patch(0x7190bd as _, &[0x3f, 0xdb]);
    patch(0x7190b2 as _, &(&DEFAULT_ROOM_SPEED as *const u32 as usize as u32).to_le_bytes());
    // write ShowProgress to form as ValueEdit
    patch(0x71a272 as _, &[0x8b, 0x15, 0x2c, 0xa8, 0x79, 0x00, 0xe8, 0xe3, 0x64, 0xe1, 0xff, 0x90, 0x90, 0x90, 0x90]);
    patch(0x71a274 as _, &(&DEFAULT_ROOM_WIDTH as *const u32 as usize as u32).to_le_bytes());
    // write NoWebsite to form as ValueEdit
    patch(0x71a4ec as _, &[0x8b, 0x15, 0x81, 0xa9, 0x79, 0x00, 0xe8, 0x69, 0x62, 0xe1, 0xff, 0x90, 0x90, 0x90, 0x90]);
    patch(0x71a4ee as _, &(&DEFAULT_ROOM_HEIGHT as *const u32 as usize as u32).to_le_bytes());
    // write NewsBrowser to form as ValueEdit
    patch(0x71a51d as _, &[0x8b, 0x15, 0x83, 0xa9, 0x79, 0x00, 0xe8, 0x38, 0x62, 0xe1, 0xff, 0x90, 0x90, 0x90, 0x90]);
    patch(0x71a51f as _, &(&DEFAULT_ROOM_SPEED as *const u32 as usize as u32).to_le_bytes());
    // read ShowProgress from form as ValueEdit
    patch(0x71a777 as _, &[0x8b, 0x80, 0xa0, 0x02, 0x00, 0x00, 0xa3, 0x2c, 0xa8, 0x79, 0x00, 0x90, 0x90]);
    patch(0x71a77e as _, &(&DEFAULT_ROOM_WIDTH as *const u32 as usize as u32).to_le_bytes());
    // read NoWebsite from form as ValueEdit
    patch(0x71a93f as _, &[0x8b, 0x80, 0xa0, 0x02, 0x00, 0x00, 0xa3, 0x81, 0xa9, 0x79, 0x00, 0x90, 0x90]);
    patch(0x71a946 as _, &(&DEFAULT_ROOM_HEIGHT as *const u32 as usize as u32).to_le_bytes());
    // read NewsBrowser from form as ValueEdit
    patch(0x71a96b as _, &[0x8b, 0x80, 0xa0, 0x02, 0x00, 0x00, 0xa3, 0x83, 0xa9, 0x79, 0x00, 0x90, 0x90]);
    patch(0x71a972 as _, &(&DEFAULT_ROOM_SPEED as *const u32 as usize as u32).to_le_bytes());

    // update timestamps when setting name
    unsafe fn patch_timestamps(dest: *mut u8) {
        patch(dest, &[0x8b, 0xc3, 0xe8, 0xe0, 0x00, 0x00, 0x00]);
    }
    patch(0x62cbe9 as _, &[0x8b, 0xc3, 0xe8, 0x3c, 0x01, 0x00, 0x00]); // objects
    patch_timestamps(0x6f59e1 as _); // sprites
    patch_timestamps(0x652381 as _); // sounds
    patch_timestamps(0x692fe5 as _); // rooms
    patch_timestamps(0x64def9 as _); // backgrounds
    patch_timestamps(0x655c01 as _); // scripts
    patch_timestamps(0x722901 as _); // paths
    patch_timestamps(0x6fcd19 as _); // fonts
    patch_timestamps(0x6fa6c9 as _); // timelines
}
