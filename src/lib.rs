#![feature(naked_functions)]

#[cfg(not(all(windows, target_arch = "x86")))]
compile_error!("this tool only works on windows 32-bit");

mod asset;
#[macro_use]
mod delphi;
mod compiler;
mod events;
mod font_render;
mod ide;
mod list;
mod load;
mod regular;
mod save;
mod save_exe;
mod stub;

use crate::{
    delphi::{TMenuItem, TTreeNode, UStr},
    ide::get_triggers,
    regular::{extension_watcher::update_extensions, project_watcher},
    save::GetAsset,
};
use ide::AssetListTrait;
use lazy_static::lazy_static;
use regex::Regex;
use std::{
    arch::asm,
    collections::{HashMap, HashSet},
    ffi::{c_void, OsStr},
    io::Write,
    os::windows::process::CommandExt,
    path::PathBuf,
    ptr,
    time::SystemTime,
};

#[derive(Debug)]
pub enum Error {
    IoError(std::io::Error),
    FileIoError(std::io::Error, PathBuf),
    DirIoError(std::io::Error, PathBuf),
    PngDecodeError(PathBuf, png::DecodingError),
    UnicodeError(String),
    AssetNotFound(String, &'static str, String),
    SyntaxError(PathBuf),
    UnknownKey(PathBuf, String),
    UnknownAction(u32, u32),
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError),
    InvalidVersion(String),
    DuplicateAsset(String),
    DuplicateIncludedFile(String),
    DuplicateTrigger(String),
    BadAssetName(String, char),
    BadIncludedFileName(String, char),
    BadTriggerName(String, char),
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
            Self::AssetNotFound(s, t, src) => write!(f, "couldn't find {} {} (from {})", t, s, src),
            Self::SyntaxError(p) => write!(f, "syntax error in file {}", p.to_string_lossy()),
            Self::UnknownKey(p, k) => write!(f, "unknown key in {}: {:?}", p.to_string_lossy(), k),
            Self::UnknownAction(lib_id, act_id) => write!(f, "unknown action {} in lib with id {}", act_id, lib_id),
            Self::ParseIntError(e) => write!(f, "integer parse error: {}", e),
            Self::ParseFloatError(e) => write!(f, "float parse error: {}", e),
            Self::InvalidVersion(v) => write!(f, "invalid exe_version {}", v),
            Self::DuplicateAsset(n) => write!(f, "multiple assets named {}", n),
            Self::DuplicateIncludedFile(n) => write!(f, "multiple included files named {}", n),
            Self::DuplicateTrigger(n) => write!(f, "multiple triggers named {}", n),
            Self::BadAssetName(n, c) => write!(f, "asset name {n} may not contain character {c}"),
            Self::BadIncludedFileName(n, c) => write!(f, "included file name {n} may not contain character {c}"),
            Self::BadTriggerName(n, c) => write!(f, "trigger file name {n} may not contain character {c}"),
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

// line iterator that strips right end but only if not in a string
pub struct GMLLines<'a>(std::str::Lines<'a>, u8);

impl<'a> GMLLines<'a> {
    fn new(lines: std::str::Lines<'a>) -> Self {
        Self(lines, 0)
    }
}

impl<'a> Iterator for GMLLines<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        // trim line only if EOL is not in a string
        let line = self.0.next()?;
        let trimmed = line.trim_end();
        for c in trimmed.bytes() {
            if self.1 == 0 && (c == b'"' || c == b'\'') {
                self.1 = c;
            } else if c == self.1 {
                self.1 = 0;
            }
        }
        Some(if self.1 == 0 { trimmed } else { line })
    }
}

pub type Result<T> = std::result::Result<T, Error>;

const ACTION_TOKEN: &str = "/*\"/*'/**//* YYD ACTION";

fn show_message(msg: impl AsRef<OsStr>) {
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

static mut SAVE_END: SystemTime = SystemTime::UNIX_EPOCH;
static mut LAST_SAVE: f64 = 0.0;

fn update_timestamp() {
    unsafe {
        SAVE_END = SystemTime::now();
        delphi::Now(&mut LAST_SAVE);
    }
}

#[naked]
unsafe extern "C" fn reset_if_time_went_backwards() {
    asm!(
        "movsd xmm0, qword ptr [{last_save}]", // load last save
        "ucomisd xmm0, qword ptr [esp]",       // compare to now
        "jb 2f", // jump if now > last save (i.e. no change needed)
        "mov dword ptr [{last_save}], 0",      // null out last_save
        "mov dword ptr [{last_save}+4], 0",
        "2: add esp, 0x20", // return
        "ret",
        last_save = sym LAST_SAVE,
        options(noreturn),
    );
}

#[derive(Clone)]
struct InstanceExtra {
    pub name: u32,
    pub xscale: f64,
    pub yscale: f64,
    pub blend: u32,
    pub angle: f64,
}

impl InstanceExtra {
    pub const DEFAULT: Self = Self { name: 0, xscale: 1.0, yscale: 1.0, blend: u32::MAX, angle: 0.0 };
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

unsafe extern "fastcall" fn stuff_to_do_on_project_init() {
    EXTRA_DATA = None;
    SEEN_ERROR = false;
    project_watcher::unwatch();
    let _: u32 = delphi_call!(0x62c554); // reset objects (what this overwrote)
    // insert a blank object
    ide::OBJECTS.alloc(1);
    // refresh the gm82room checkbox
    refresh_gm82room_checkbox();
}

unsafe extern "fastcall" fn close_preferences_form() {
    refresh_gm82room_checkbox();
    // save to registry
    let _: u32 = delphi_call!(0x718ac0);
}

unsafe extern "fastcall" fn toggle_gm82room_checkbox() {
    let setting = 0x79a982 as *mut bool;
    *setting = !*setting;
    refresh_gm82room_checkbox();
    // save to registry
    let _: u32 = delphi_call!(0x718ac0);
}

unsafe fn refresh_gm82room_checkbox() {
    let main_form = *(0x790100 as *const *const *mut bool);
    let room_item = *main_form.add(0x3b8 / 4);
    let _: u32 = delphi_call!(0x4c0238, room_item, !*(0x79a982 as *const bool) as u32);
}

static mut EXTRA_DATA: Option<(HashMap<usize, InstanceExtra>, HashMap<usize, TileExtra>)> = None;

unsafe extern "fastcall" fn about_inj(about_dialog: *const *const usize) {
    let info = UStr::new(concat!("gm82save: ", env!("ABOUT_BUILD_DATE")));
    let edition_label = *about_dialog.add(0xe5);
    asm!(
        "call {}",
        in(reg) 0x4ee6d8, // TControl.SetText
        in("eax") edition_label,
        in("edx") info.0,
        clobber_abi("C"),
    );
}

#[naked]
unsafe extern "C" fn save_inj() {
    asm!(
        "mov ecx, ebp",
        "sub ecx, 4",
        "mov edx, ebp",
        "sub edx, 20",
        "jmp {}",
        sym save,
        options(noreturn),
    );
}

// set the high byte to nonzero if YYD save code was used
// set the low byte to nonzero on success
unsafe extern "fastcall" fn save(proj_path: &UStr, stream_ptr: *mut u32) -> u16 {
    SEEN_ERROR = false;
    const IS_YYD: u16 = 0x100;
    let mut path: PathBuf = proj_path.to_os_string().into();
    // filename ".gm82" works in the ui but rust doesn't get it so check for that specifically
    let is_gm82 = path.extension() == Some("gm82".as_ref()) || path.file_name() == Some(".gm82".as_ref());
    if !is_gm82 {
        project_watcher::unwatch();
        // CStream.Create
        let buf = delphi_call!(0x405a4c, 0x52e8fc, 1);
        stream_ptr.write(buf);
        // save gmk
        let success: u32 = delphi_call!(0x705798, buf);
        return success as u16
    }

    if let Err(e) = save::save_gmk(&mut path) {
        // display the error
        project_watcher::unwatch();
        delphi::close_progress_form();
        show_message(format!("Failed to save: {}", e));
        0 | IS_YYD
    } else {
        if !SAVING_FOR_ROOM_EDITOR {
            project_watcher::setup_watcher(&mut path);
        }
        delphi::close_progress_form();
        1 | IS_YYD
    }
}

#[naked]
unsafe extern "C" fn load_inj() {
    asm!(
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
    );
}

unsafe extern "fastcall" fn load(proj_path: &UStr, stream_ptr: *mut u32, result_ptr: *mut bool) -> bool {
    SEEN_ERROR = false;
    SAW_APPLIES_TO_WARNING = false;
    project_watcher::unwatch();
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
        show_message(format!("Failed to load: {}", e));
        ide::initialize_project();
    } else {
        delphi::close_progress_form();
        result_ptr.write(true);
    }
    true
}

#[naked]
unsafe extern "C" fn stuff_to_do_on_ide_start() {
    unsafe extern "C" fn inj() {
        regular::init();
        // overwrite HelpBtn OnClick
        (0x790100 as *const *const *mut usize)
            .read()
            .add(0x3fc / 4)
            .read()
            .add(0x110 / 4)
            .write(start_shader_compiler as usize);
    }
    asm!(
        "mov eax, 0x77f464",
        "mov byte ptr [eax], 0",
        "jmp {}",
        sym inj,
        options(noreturn),
    )
}

unsafe extern "fastcall" fn start_shader_compiler() {
    if let Err(e) = std::process::Command::new("anvil.exe").spawn() {
        show_message(format!("Couldn't start anvil.exe: {e}"));
    }
}

#[naked]
unsafe extern "C" fn load_recent_project_and_maybe_compile() {
    unsafe extern "C" fn inj() {
        let mut args = std::env::args().peekable();
        while let Some(arg) = args.next() {
            if arg == "--build" {
                if let Some(path) = args.peek() {
                    // we found a build arg, build project and close
                    let path = UStr::new(path);
                    let _: u32 = delphi_call!(0x6ce300, path.0, 0, 0, 0);
                    std::process::exit(0);
                }
            }
        }
    }
    asm!(
        // load project
        "mov ecx, 0x7059d8",
        "call ecx",
        "jmp {}",
        sym inj,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn gm81_or_gm82_inj() {
    asm!(
        "mov ecx, eax",
        "jmp {}",
        sym gm81_or_gm82,
        options(noreturn),
    );
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
    // throw this in here as well
    project_watcher::unwatch();
}

#[naked]
unsafe extern "C" fn fix_tile_null_pointer() {
    asm!(
        "mov edx, 0x64e048",
        "call edx",
        "mov edx, 0x68ef07",
        "mov ecx, 0x68ef6c",
        "test eax, eax",
        "cmovz edx, ecx",
        "jmp edx",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn inflate_inj() {
    asm!(
        "mov ecx, eax",
        "jmp {}",
        sym inflate,
        options(noreturn),
    );
}

unsafe extern "fastcall" fn inflate(src: &delphi::TMemoryStream) -> delphi::DelphiBox<delphi::TMemoryStream> {
    let dst = delphi::TMemoryStream::new();
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
    asm!(
        "mov ecx, eax",
        "jmp {}",
        sym deflate,
        options(noreturn),
    );
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
    asm!(
        "mov ecx, 9",
        "mov {}, ecx",
        "mov ecx, 0x4cf2f4",
        "jmp ecx",
        sym DEFLATE_LEVEL,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn build_fast() {
    asm!(
        "mov ecx, 0x79a998",
        "movzx ecx, byte ptr [ecx]",
        "mov {}, ecx",
        "mov ecx, 0x41735c",
        "jmp ecx",
        sym DEFLATE_LEVEL,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn reset_compression() {
    asm!(
        "mov ecx, 6",
        "mov {}, ecx",
        "mov ecx, 0x51cc64",
        "jmp ecx",
        sym DEFLATE_LEVEL,
        options(noreturn),
    );
}

unsafe extern "stdcall" fn duplicate_room(room: &mut asset::Room, old_id: usize, new_id: usize) {
    let room_names = ide::ROOMS.names();
    fix_instances_when_renaming_room(
        room,
        room_names[old_id].to_os_string().to_str().unwrap(),
        room_names[new_id].to_os_string().to_str().unwrap(),
    );
    freshen_room_ids(room);
}

unsafe fn freshen_room_ids(room: &mut asset::Room) {
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
    asm!(
        "mov ecx, edi",
        "call {}",
        "mov eax, 5",
        "ret",
        sym setup_unicode_parse,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn teardown_unicode_parse_inj() {
    asm!(
        "mov ecx, 810",
        "call {}",
        "mov eax, 0x6ca2cc",
        "jmp eax",
        sym setup_unicode_parse,
        options(noreturn),
    );
}

unsafe extern "fastcall" fn setup_unicode_parse(version: i32) {
    // this just patches CStream.ReadString to read with the active code page instead of UTF-8
    // (and reverts that change after loading so nothing else breaks)
    let cp = if version < 810 { [0, 0] } else { [0xe9, 0xfd] };
    patch(0x52f0a2, &cp);
    patch(0x52f0c5, &cp);
}

#[naked]
unsafe extern "C" fn properly_update_object_timestamp_drag_drop() {
    asm!(
        "mov eax, [esi + 0x46c]", // TObjectForm.index
        "mov ecx, 0x62cd2c",      // update object timestamp
        "jmp ecx",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn properly_update_timeline_timestamp_drag_drop() {
    asm!(
        "mov eax, [esi + 0x430]", // TTimeLineForm.index
        "mov ecx, 0x6fa7b0",      // update timeline timestamp
        "jmp ecx",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn properly_update_object_timestamp_right_click() {
    asm!(
        // show action modal
        "mov ecx, 0x6ff4dc",
        "call ecx",
        // if it returned false, return false
        "test al, al",
        "jz 2f",
        // update timestamp and return 1
        "mov eax, [esi + 0x46c]",
        "mov ecx, 0x62cd2c",
        "call ecx",
        "mov al, 1",
        "2: ret",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn properly_update_timeline_timestamp_right_click() {
    asm!(
        // show action modal
        "mov ecx, 0x6ff4dc",
        "call ecx",
        // if it returned false, return false
        "test al, al",
        "jz 2f",
        // update timestamp and return 1
        "mov eax, [esi + 0x430]",
        "mov ecx, 0x6fa7b0",
        "call ecx",
        "mov al, 1",
        "2: ret",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn update_sprite_mask_timestamp() {
    asm!(
        "mov eax, [ebx+0x42c]", // TMaskForm.theindex
        "mov ecx, 0x6f5ac8",    // update sprite timestamp
        "jmp ecx",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn gm82_file_association_inj() {
    asm!(
        "mov ecx, eax",
        "jmp {}",
        sym gm82_file_association,
        options(noreturn),
    );
}

unsafe extern "fastcall" fn gm82_file_association(reg: u32) {
    let ext = UStr::new(r"\.gm82");
    let _: u32 = delphi_call!(0x6dd850, reg, ext.0, 0, UStr::new("gm82file").0);
    let _: u32 = delphi_call!(0x452568, reg, 0x80000001u32);
    let _: u32 = delphi_call!(0x6dd850, reg, ext.0, UStr::new(r"\Software\Classes").0, UStr::new("gm82file").0);
}

unsafe extern "fastcall" fn check_gm_processes(name: usize, value: u32) {
    use sysinfo::{ProcessExt, SystemExt};
    let system = sysinfo::System::new_with_specifics(
        sysinfo::RefreshKind::new().with_processes(sysinfo::ProcessRefreshKind::new()),
    );
    let path = std::env::current_exe().unwrap();
    if system.processes().iter().filter(|(_, p)| p.exe() == path).count() <= 1 {
        let _: u32 = delphi_call!(0x716b78, name, value);
    }
}

#[naked]
unsafe extern "C" fn image_editor_dont_error_when_switching_tool() {
    asm!(
        // set mouse down global to -1
        "mov eax, 0x77f108",
        "mov dword ptr [eax], -1",
        // what this overwrote
        "mov eax, [ebx + 0x768]",
        "ret",
        options(noreturn),
    )
}

#[naked]
unsafe extern "C" fn free_image_editor_bitmap() {
    asm!(
        // call free on TheBitmap
        "mov edx, 0x405a7c",
        "call edx",
        // get TransBitmap
        "mov eax, dword ptr [esi]",
        "mov eax, dword ptr [eax + 0x708]",
        // check if it exists
        "test eax, eax",
        "jz 1f",
        // free it
        "mov edx, 0x405a7c",
        "call edx",
        // null it
        "mov eax, dword ptr [esi]",
        "xor edx, edx",
        "mov dword ptr [eax + 0x708], edx",
        "1: ret",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn copy_origin_on_new() {
    asm!(
        "mov ecx, [eax+0xc]",
        "mov [esi+0xc], ecx",
        "mov ecx, [eax+0x10]",
        "mov [esi+0x10], ecx",
        "mov ecx, 0x405a7c",
        "jmp ecx",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn floor_st0() {
    asm!(
        // move the return address and put st0 on the stack before it so it's like an argument
        "mov eax, [esp]",
        "sub esp, 8",
        // can't fucking say "fstp tword ptr [esp]" apparently so i guess i'm doing this
        ".byte 0xdb, 0x3c, 0x24",
        "push eax",
        "mov eax, 0x410538",
        "jmp eax",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn update_sprite_icon_on_revert() {
    asm!(
        "mov ecx, 0x6f5980", // set sprite name (original function)
        "call ecx",
        "mov eax, [ebx + 0x40c]", // TSpriteForm.index
        "mov ecx, 0x6f5bb4",      // update sprite icon
        "jmp ecx",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn update_background_icon_on_revert() {
    asm!(
        "mov ecx, 0x64de98", // set background name (original function)
        "call ecx",
        "mov eax, [ebx + 0x400]", // TBackgroundForm.index
        "mov ecx, 0x64e0cc",      // update background icon
        "jmp ecx",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn dont_show_action_tooltip_if_event_is_null() {
    asm!(
        // if eax is not null, call CEvent.GetAction
        "mov ecx, 0x5a502c",
        "test eax, eax",
        "jz 2f",
        "jmp ecx",
        // otherwise, skip to the end of that code block
        // there's a function that updates the tooltip or something in there
        // so skip it, so that the tooltip doesn't just stick around
        "2: add dword ptr [esp], 0x25",
        "ret",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn first_object_updates_room_forms() {
    asm!(
        // get number of objects in resource tree
        "mov eax, [0x79a9b8]",
        "mov edx, 0x4ad490",
        "call edx",
        // skip if there are objects
        "test eax, eax",
        "jnz 2f",
        // go over all room forms
        "push ebx",
        "xor ebx, ebx",
        "4: cmp ebx, [0x77f3b8]",
        "jge 3f",
        // initialize objects tab
        "mov eax, [0x77f3ac]",
        "mov eax, [eax+4*ebx]",
        "test eax, eax",
        "je 5f",
        "mov edx, 0x68a1e0",
        "call edx",
        "5: inc ebx",
        "jmp 4b",
        "3: pop ebx",
        "2:",
        // call overwritten tree node write function
        "mov eax, [ebp-0xc]",
        "mov ecx, [ebp-0x8]",
        "mov edx, edi",
        "push 0x71cb48",
        "ret",
        options(noreturn),
    )
}

#[naked]
unsafe extern "C" fn timeline_form_add_events() {
    asm!(
        // call original function
        "mov ecx, 0x6f7fac",
        "call ecx",

        // get event list
        "mov eax, [ebx + 0x3d0]",
        // set its OnDblClick to open action
        "mov edx, 0x6f9860",
        "mov [eax + 0x118], edx",
        "mov [eax + 0x11c], ebx",

        "ret",
        options(noreturn),
    )
}

#[naked]
unsafe extern "C" fn object_form_add_events() {
    asm!(
        // call original function
        "mov ecx, 0x6c60f8",
        "call ecx",

        // get parent label/button
        "mov eax, [ebx + 0x418]",
        // set its OnClick
        "lea edx, {parent}",
        "mov [eax + 0x110], edx",
        "mov [eax + 0x114], ebx",

        // get mask label/button
        "mov eax, [ebx + 0x410]",
        // set its OnClick
        "lea edx, {mask}",
        "mov [eax + 0x110], edx",
        "mov [eax + 0x114], ebx",

        // get "depth label" / children button
        "mov eax, [ebx + 0x414]",
        // set its OnClick
        "lea edx, {children}",
        "mov [eax + 0x110], edx",
        "mov [eax + 0x114], ebx",

        // get event list
        "mov eax, [ebx + 0x3d4]",
        // set its OnDblClick to open action
        "mov edx, 0x6c77d0",
        "mov [eax + 0x118], edx",
        "mov [eax + 0x11c], ebx",

        "ret",
        parent = sym object_open_parent,
        mask = sym object_open_mask,
        children = sym object_show_children_inj,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn object_open_parent() {
    asm!(
        // get theobject from form
        "mov eax, [eax + 0x45c]",
        // get parent_index from object
        "mov eax, [eax + 0x14]",
        // open the form
        "mov ecx, 0x62cde0",
        "jmp ecx",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn object_open_mask() {
    asm!(
        // get theobject from form
        "mov eax, [eax + 0x45c]",
        // get mask_index from object
        "mov eax, [eax + 0x18]",
        // open the form
        "mov ecx, 0x6f5b74",
        "jmp ecx",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn object_show_children_inj() {
    asm!(
        "mov ecx, eax",
        "jmp {}",
        sym object_show_children,
        options(noreturn),
    );
}

unsafe extern "fastcall" fn object_show_children(object_form: *const i32) {
    const RESULT_PTR: *mut i32 = 0x79a9f0 as _;
    *RESULT_PTR = -100;
    let object_index = object_form.add(0x46c / 4).read();
    let mut popup = delphi::TPopupMenu::new(0);
    popup.SetAutoHotkeys(1);
    popup.SetImages();

    let mut children = ide::OBJECTS
        .assets()
        .iter()
        .enumerate()
        .filter_map(|(i, o)| Some((i, o.as_ref()?)))
        .filter(|(_, o)| o.parent_index == object_index)
        .map(|(i, _)| i)
        .peekable();

    if children.peek().is_some() {
        for obj in children {
            popup.Items.add_with_fake_tree_node(&ide::OBJECTS.names()[obj], 3, 1, obj, None);
        }
    } else {
        let mut menu_item = TMenuItem::new(0);
        menu_item.set_caption(&UStr::new("<no children>"));
        menu_item.set_image_index(-1);
        popup.Items.add(menu_item);
    }
    popup.popup_at_cursor_pos();
    let _: u32 = delphi_call!(0x62cde0, *RESULT_PTR);
}

#[naked]
unsafe extern "C" fn object_clean_collide_events_inj() {
    asm!(
        "mov ecx, eax",
        "mov edx, ebx",
        "jmp {}",
        sym object_clean_collide_events,
        options(noreturn),
    );
}

unsafe extern "fastcall" fn object_clean_collide_events(obj: &mut asset::Object, object_id: usize) {
    let mut updated = false;
    for (i, event) in obj.events[events::EV_COLLISION].iter_mut().enumerate() {
        if event.action_count != 0 && ide::OBJECTS.assets().get_asset(i as i32).is_none() {
            let _: u32 = delphi_call!(0x5a5090, event.as_ptr());
            updated = true;
        }
    }
    if updated {
        let _: u32 = delphi_call!(0x62cd2c, object_id);
    }
}

#[naked]
unsafe extern "C" fn object_clean_triggers_inj() {
    asm!(
        "mov ecx, ebx",
        "jmp {}",
        sym object_clean_triggers,
        options(noreturn),
    );
}

unsafe extern "fastcall" fn object_clean_triggers(trigger_id: usize) {
    for (obj_id, obj_opt) in ide::OBJECTS.assets_mut().iter_mut().enumerate() {
        let mut updated = false;
        if let Some(obj) = obj_opt {
            if let Some(event) = obj.events[events::EV_TRIGGER].get(trigger_id) {
                if event.action_count != 0 && get_triggers()[trigger_id].is_none() {
                    let _: u32 = delphi_call!(0x5a5090, event.as_ptr());
                    updated = true;
                }
            }
        }
        if updated {
            let _: u32 = delphi_call!(0x62cd2c, obj_id);
        }
    }
}

#[naked]
unsafe extern "C" fn path_form_mouse_wheel_inj() {
    asm!(
        // call TPathForm.Create
        "mov ebx, 0x514e78",
        "call ebx",
        // set OnMouseWheel
        "mov dword ptr [eax + 0x144], eax",
        "lea edx, {}",
        "mov dword ptr [eax + 0x140], edx",
        "ret",
        sym path_form_mouse_wheel,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn path_form_mouse_wheel() {
    asm!(
        // check handled flag
        "mov edx, [esp + 0x4]",
        "cmp byte ptr [edx], 0",
        "jnz 4f",
        // set handled flag
        "mov byte ptr [edx], 1",
        "push eax",
        // check if shift is being held
        "mov edx, dword ptr [esp + 0x10]",
        "test cx, 1",
        "jnz 2f",
        // no shift, so scroll vertically
        "sub dword ptr [eax + 0x464], edx",
        "jmp 3f",
        // yes shift, so scroll horizontally
        "2: sub dword ptr [eax + 0x460], edx",
        "3:",
        // update room background
        "mov ecx, 0x7203ec",
        "call ecx",
        // draw path image
        "mov eax, dword ptr [esp]",
        "mov ecx, 0x720560",
        "call ecx",
        // update status bar
        "pop eax",
        "mov ecx, 0x720a68",
        "call ecx",
        "4:",
        "ret 0xc",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn path_room_change_forces_room_editor_save() {
    asm!(
        "mov ecx, 0x720560", // draw form
        "call ecx",
        "mov byte ptr {}, 1",
        "ret",
        sym PATH_FORM_UPDATED,
        options(noreturn),
    );
}

static mut PATH_FORM_UPDATED: bool = false;

#[naked]
unsafe extern "C" fn maybe_reload_extensions_when_typing() {
    asm!(
        "mov [ebp-8], eax",
        "call {}",
        "mov eax, 0x6ab1bb",
        "jmp eax",
        sym update_extensions,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn code_editor_dont_resize_if_maximized() {
    asm!(
        // are we maximized?
        "cmp byte ptr [eax + 0x29a], 0x2",
        "je 2f",
        // if not, call actual function
        "jmp [ebx + 0x98]",
        // otherwise quit
        "2: ret 0x8",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn code_editor_better_resize() {
    asm!(
        // are we maximized?
        "cmp byte ptr [eax + 0x29a], 0x2",
        "jne 2f",
        // if maximized, return from outer function
        "add esp, 4",
        // otherwise continue regular operation
        "2: mov edx, [0x781dd0]",
        "ret",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn code_editor_middle_click() {
    asm!(
        // push return address
        "mov ecx, 0x6b734e",
        "push ecx",
        // abort if not middle click
        "cmp byte ptr [ebp - 1], 2",
        "jnz 2f",
        // save cursor position
        "push dword ptr [ebx + 0x2ec]",
        "push dword ptr [ebx + 0x2f0]",
        // move cursor
        "mov dword ptr [ebx + 0x2ec], edi",
        "mov dword ptr [ebx + 0x2f0], esi",
        // show resource on cursor position
        "mov ecx, 0x6b2000",
        "mov eax, ebx",
        "call ecx",
        // reset cursor
        "pop dword ptr [ebx + 0x2f0]",
        "pop dword ptr [ebx + 0x2ec]",
        "2: ret",
        options(noreturn),
    );
}

unsafe extern "fastcall" fn code_editor_script_hint(name: *const u16, out: &mut UStr) {
    // get script from name
    let script_id: i32 = delphi_call!(0x655c2c, name);
    if script_id >= 0 {
        if let Some(Some(script)) = ide::SCRIPTS.assets().get(script_id as usize) {
            if script.source.as_slice().get(..3) == Some(&[b'/' as u16; 3]) {
                let count =
                    script.source.as_slice().iter().position(|&c| c == b'\r' as u16).unwrap_or(script.source.len()) - 3;
                let mut untrimmed = UStr::default();
                // @UStrCopy
                let _: u32 = delphi_call!(0x4086a8, script.source.0, 4, count, &mut untrimmed.0);
                // Trim
                let _: u32 = delphi_call!(0x415dd0, untrimmed.0, &mut out.0);
            }
        }
    }
}

#[naked]
unsafe extern "C" fn completion_script_args_inj() {
    asm!(
        "mov ecx, eax",
        "call {}",
        // copy output of this to the argument on the stack
        "mov eax, dword ptr [ebp-0x5c]",
        "mov dword ptr [esp+8], eax",
        "ret",
        sym completion_script_args,
        options(noreturn),
    );
}

unsafe extern "fastcall" fn add_space_before_trigger_name(trigger_id: i32, out: &mut UStr) {
    *out = UStr::new(" ");
    let mut tmp = UStr::default();
    let _: u32 = delphi_call!(0x6bcce4, trigger_id, &mut tmp);
    out.push_ustr(&tmp);
}

unsafe extern "fastcall" fn completion_script_args(script_id: usize, out: &mut UStr) {
    let script = ide::SCRIPTS.assets().get_unchecked(script_id).as_deref().unwrap_unchecked();
    if script.source.as_slice().get(..3) == Some(&[b'/' as u16; 3]) {
        if let Some(paren_pos) = script.source.as_slice().iter().position(|&c| c == b'(' as u16) {
            let count = script.source.as_slice().iter().position(|&c| c == b'\r' as u16).unwrap_or(script.source.len())
                - paren_pos;
            let mut untrimmed = UStr::default();
            // @UStrCopy
            let _: u32 = delphi_call!(0x4086a8, script.source.0, paren_pos + 1, count, &mut untrimmed.0);
            // Trim
            let _: u32 = delphi_call!(0x415dd0, untrimmed.0, &mut out.0);
            return
        }
    }
    *out = UStr(0x6baf10 as _);
}

#[naked]
unsafe extern "C" fn write_number_on_actions() {
    asm!(
        // call original function
        "mov ecx, 0x45b498",
        "call ecx",
        // move existing string to top of stack
        "mov eax, [ebp-4]",
        "push eax",
        "mov dword ptr [ebp-4], 0",
        // get action index from outer function (same for object and form)
        "mov eax, [ebp]",
        "mov eax, [eax-4]",
        "inc eax",
        // convert to int and put in original string
        "lea edx, [ebp-4]",
        "mov ecx, 0x41666c",
        "call ecx",
        // append ". "
        "lea eax, [ebp-4]",
        "lea edx, 2f",
        "mov ecx, 0x4082dc",
        "call ecx",
        // append original string
        "lea eax, [ebp-4]",
        "mov edx, [esp]",
        "mov ecx, 0x4082dc",
        "call ecx",
        // free original string
        "lea eax, [esp]",
        "mov ecx, 0x407ea8",
        "call ecx",
        // cleanup and return
        "add esp, 4",
        "ret",
        // define the ". "
        ".align 4",
        ".short 1200, 2",
        ".long -1, 2",
        "2:",
        ".short '.', ' ', 0",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn regen_temp_folder_when_making_file() {
    asm!(
        "mov ecx, 0x407660",
        "call ecx",
        // ForceDirectories the temp directory
        "mov eax, [0x788974]",
        "mov ecx, 0x416eac",
        "jmp ecx",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn get_temp_folder_but_also_regen_it() {
    asm!(
        //UStrAsg temp_directory to the output
        "mov edx, [0x788974]",
        "mov ecx, 0x407eb8",
        "call ecx",
        // ForceDirectories the temp directory
        "mov eax, [0x788974]",
        "mov ecx, 0x416eac",
        "jmp ecx",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn trace_date_inj() {
    asm!(
        // keep first two args
        "push ecx",
        "push eax",
        // get date string
        "call {}",
        // take first two args back
        "pop edx",
        "pop ecx",
        // store date string
        "push eax",
        // put args in right place
        "xchg edx, eax",
        // call UStrCat3
        "push ebx",
        "mov ebx, 0x40839c",
        "call ebx",
        "pop ebx",
        // free date string
        "mov eax, esp",
        "mov edx, 0x407ea8",
        "call edx",
        // we done here
        "pop eax",
        "ret",
        sym trace_date,
        options(noreturn),
    );
}

unsafe extern "fastcall" fn trace_date() -> UStr {
    let now = time::OffsetDateTime::now_utc();
    UStr::new(format!(
        "[{}-{:02}-{:02} {:02}:{:02}:{:02} UTC | gm82save {}] Unhandled Exception - ",
        now.year(),
        u8::from(now.month()),
        now.day(),
        now.hour(),
        now.minute(),
        now.second(),
        env!("ERROR_BUILD_DATE"),
    ))
}

static mut SEEN_ERROR: bool = false;

unsafe extern "fastcall" fn patch_error_box(caption: *const u16, text: *const u16, _flags: u32) {
    if SEEN_ERROR {
        return
    }
    SEEN_ERROR = true;
    let mut message = UStr::from_ptr(&text).clone();
    let extra_text = UStr::new(
        "\r\nGame Maker will continue to run, but may be unstable. Enjoy!\r\n\r\n\
        In the meantime, please send Floogle your TraceIDE.log file. It should be next to \
        GameMaker.exe. Would you like to open the relevant folder now?",
    );
    // UStrCat
    let _: u32 = delphi_call!(0x4082dc, &mut message, extra_text.0);
    // TApplication.MessageBox
    let answer: u32 = delphi_call!(0x51fbdc, *(0x7882ec as *const u32), message.0, caption, 0x14);
    if answer == 6 {
        let path = std::env::current_exe()
            .ok()
            .and_then(|mut p| {
                p.pop();
                p.push("TraceIDE.log");
                p.into_os_string().into_string().ok()
            })
            .unwrap_or_default();
        let _ = std::process::Command::new("explorer.exe").raw_arg(format!("/select,\"{path}\"")).spawn();
    }
}

#[naked]
unsafe extern "C" fn get_treenode_count_and_preserve_resource_type() {
    asm!("mov ecx, 0x4ad490", "call ecx", "mov [esp], edi", "mov ecx, 0x71c6a5", "jmp ecx", options(noreturn),)
}

#[naked]
unsafe extern "C" fn add_three_newest_inj() {
    asm!(
        // add a line
        "mov eax, [esi + 0x38]",
        "mov ecx, 0x4dd244",
        "call ecx",
        // add the three items
        "pop edx", // resource type
        "push ebp",
        "mov ecx, [esi + 0x38]", // menu items
        "call {}",
        // cleanup
        "xor eax, eax",
        "pop edx",
        "pop ecx",
        "pop ecx",
        "mov ecx, 0x71c6e2",
        "jmp ecx",
        sym add_three_newest,
        options(noreturn),
    );
}

unsafe extern "fastcall" fn add_three_newest(items: &TMenuItem, ty: u32, ebp: *const u8) {
    unsafe fn inner<T: 'static, AL: AssetListTrait<T>>(items: &TMenuItem, ty: u32, ebp: *const u8, asset_list: &AL) {
        let ids = asset_list
            .assets()
            .iter()
            .enumerate()
            .rev()
            .filter(|(_, o)| o.is_some())
            .map(|(i, _)| i)
            .take(3)
            .collect::<Vec<_>>();
        for &id in ids.iter().rev() {
            items.add_with_fake_tree_node(
                &asset_list.names()[id],
                3,
                ty,
                id,
                ebp.sub(5).cast::<bool>().read().then(|| &*ebp.cast::<[u32; 6]>()),
            );
        }
    }
    match ty {
        1 => inner(items, ty, ebp, &ide::OBJECTS),
        2 => inner(items, ty, ebp, &ide::SPRITES),
        3 => inner(items, ty, ebp, &ide::SOUNDS),
        4 => inner(items, ty, ebp, &ide::ROOMS),
        6 => inner(items, ty, ebp, &ide::BACKGROUNDS),
        7 => inner(items, ty, ebp, &ide::SCRIPTS),
        8 => inner(items, ty, ebp, &ide::PATHS),
        9 => inner(items, ty, ebp, &ide::FONTS),
        12 => inner(items, ty, ebp, &ide::TIMELINES),
        _ => return,
    };
}

#[naked]
unsafe extern "C" fn room_form_inj() {
    asm!(
        "mov ecx, eax",
        "jmp {}",
        sym room_form,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn room_size() {
    asm!(
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
    );
}

#[naked]
unsafe extern "C" fn fix_broken_room_size() {
    asm!(
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
    );
}

static mut DEFAULT_ROOM_WIDTH: u32 = 800;
static mut DEFAULT_ROOM_HEIGHT: u32 = 608;
static mut DEFAULT_ROOM_SPEED: u32 = 50;

#[naked]
unsafe extern "C" fn rename_room_inj() {
    asm!(
        "mov ecx, ebx",
        "mov edx, [ebp - 4]",
        "call {}",
        "test eax, eax", // for the jump afterwards
        "ret",
        sym rename_room,
        options(noreturn),
    );
}

unsafe extern "fastcall" fn rename_room(room_id: usize, new_name: *const u16) -> *const UStr {
    let room_names = ide::ROOMS.names();
    if let Some(room) = ide::ROOMS.assets_mut()[room_id].as_deref_mut() {
        let new_name = UStr::from_ptr(&new_name);
        let new_name_slice = new_name.as_slice();
        if new_name_slice.is_empty() {
            show_message("Can't give room an empty name.");
            return ptr::null()
        }
        if new_name_slice.contains(&(b'=' as u16)) {
            show_message("Can't use illegal character '=' in asset name.");
            return ptr::null()
        }
        let old_name = room_names[room_id].to_os_string().into_string().unwrap();
        let new_name = new_name.to_os_string().into_string().unwrap();
        fix_instances_when_renaming_room(room, &old_name, &new_name);
    }
    &room_names[room_id]
}

lazy_static! {
    static ref ROOM_RENAME_REGEX: Regex = Regex::new(r"=[ \t\r\n]*([^=]*?)_[0-9A-F]{8}").unwrap();
}

fn fix_instances_when_renaming_room(room: &mut asset::Room, old_name: &str, new_name: &str) {
    let re: &Regex = &ROOM_RENAME_REGEX;
    for inst in room.get_instances_mut() {
        let code = inst.creation_code.to_os_string().into_string().unwrap();
        let mut it = re.captures_iter(&code).filter_map(|c| c.get(1)).filter(|m| m.as_str() == old_name).peekable();
        if it.peek().is_none() {
            continue
        }
        let mut new_code = String::with_capacity(code.len());
        let mut last_match = 0;
        for m in it {
            new_code.push_str(&code[last_match..m.start()]);
            new_code.push_str(&new_name);
            last_match = m.end();
        }
        new_code.push_str(&code[last_match..]);
        inst.creation_code = UStr::new(new_code);
    }
}

#[naked]
unsafe extern "C" fn dont_make_room_form_inj() {
    asm!(
        "mov ecx, eax",
        "jmp {}",
        sym dont_make_room_form,
        options(noreturn)
    );
}

unsafe extern "fastcall" fn dont_make_room_form(node: &TTreeNode) {
    // always open if gm82room is disabled, not opening a room, or not using gm82 format
    if *(0x79a982 as *const bool)
        || (*node.data).kind != 4
        || (*ide::PROJECT_PATH).as_slice().last().copied() != Some(b'2' as u16)
    {
        let _: u32 = delphi_call!(0x71d608, node);
    }
}

#[naked]
unsafe extern "C" fn show_instance_id_inj() {
    asm!(
        "mov ecx, eax",
        "mov eax, [ebx + 0x630]",
        "push eax",
        "call {}",
        "ret",
        sym show_instance_id,
        options(noreturn),
    );
}

unsafe extern "fastcall" fn show_instance_id(id: usize, out: &mut UStr, room_id: usize) {
    if let Some((insts, _)) = EXTRA_DATA.as_mut() {
        let suffix = {
            let mut name = insts.entry(id).or_default().name;
            if name == 0 {
                loop {
                    name = delphi::Random();
                    if !insts.values().any(|ex| ex.name == name) {
                        insts.get_mut(&id).unwrap().name = name;
                        break
                    }
                }
            }
            UStr::new(format!("_{:08X}", name))
        };
        let _: u32 = delphi_call!(0x40839c, out, ide::ROOMS.names()[room_id].0, suffix.0);
    } else {
        let _: u32 = delphi_call!(0x41666c, id, out);
    }
}

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
                for room in ide::ROOMS.assets_mut().iter_mut().map(|r| r.as_deref_mut()).flatten() {
                    // check if all ids are unique, and if not, update *all* the ids for consistency
                    let instances_ok = room.get_instances().iter().all(|inst| instance_ids.insert(inst.id));
                    let tiles_ok = room.get_tiles().iter().all(|tile| tile_ids.insert(tile.id));
                    if !instances_ok || !tiles_ok {
                        freshen_room_ids(room);
                    }
                }
                // force-save all open rooms just in case
                for form in ide::ROOMS.forms().iter().map(|&f| f as *mut *const u8) {
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
            // save game first, if needed
            let project_modified = PATH_FORM_UPDATED || {
                let out: u32 = delphi_call!(0x7060e8);
                out != 0
            };
            if project_modified {
                SAVING_FOR_ROOM_EDITOR = true;
                let _: u32 = delphi_call!(0x51cc64, *(0x7882f0 as *const u32), 0xfff5); // set cursor
                let success: u32 = delphi_call!(0x705c84, (*ide::PROJECT_PATH).0); // save
                let _: u32 = delphi_call!(0x51cc64, *(0x7882f0 as *const u32), 0); // reset cursor
                SAVING_FOR_ROOM_EDITOR = false;
                if success == 0 {
                    return 0
                }
            } else {
                project_watcher::unwatch();
            }
            // sort out running gm82room
            room_path.pop();
            let mut asset_maps_path = room_path.clone();
            room_path.push("rooms");
            room_path.push(ide::ROOMS.names()[room_id].to_os_string());
            let _: u32 = delphi_call!(0x51acd0, *(0x790100 as *const u32)); // hide main form
            let _ = std::process::Command::new(editor_path).arg(&room_path).spawn().and_then(|mut c| c.wait());
            let _: u32 = delphi_call!(0x51acd8, *(0x790100 as *const u32)); // show main form
            {
                let room_opt = &mut ide::ROOMS.assets_mut()[room_id];
                let room = room_opt.as_mut().unwrap();
                // remove this room's ids from the global thing
                if let Some((extra_inst, extra_tile)) = EXTRA_DATA.as_mut() {
                    for inst in room.get_instances() {
                        extra_inst.remove(&inst.id);
                    }
                    for tile in room.get_tiles() {
                        extra_tile.remove(&tile.id);
                    }
                }

                *room_opt = None; // delete room
            }
            // reload whether assets exist
            let asset_maps = {
                let mut has_backgrounds = true;
                let mut has_datafiles = true;
                let mut has_fonts = true;
                let mut has_objects = true;
                let mut has_paths = true;
                let mut has_scripts = true;
                let mut has_sounds = true;
                let mut has_sprites = true;
                let mut has_timelines = true;
                let mut has_triggers = true;
                let project_path = PathBuf::from((&*ide::PROJECT_PATH).to_os_string());
                load::read_txt(&project_path, |k, v| {
                    Ok(match k {
                        "has_backgrounds" => has_backgrounds = v.parse::<u8>()? != 0,
                        "has_datafiles" => has_datafiles = v.parse::<u8>()? != 0,
                        "has_fonts" => has_fonts = v.parse::<u8>()? != 0,
                        "has_objects" => has_objects = v.parse::<u8>()? != 0,
                        "has_paths" => has_paths = v.parse::<u8>()? != 0,
                        "has_scripts" => has_scripts = v.parse::<u8>()? != 0,
                        "has_sounds" => has_sounds = v.parse::<u8>()? != 0,
                        "has_sprites" => has_sprites = v.parse::<u8>()? != 0,
                        "has_timelines" => has_timelines = v.parse::<u8>()? != 0,
                        "has_triggers" => has_triggers = v.parse::<u8>()? != 0,
                        _ => (),
                    })
                })
                .expect("reloading project failed");
                load::load_asset_maps(
                    &mut asset_maps_path,
                    has_triggers,
                    has_sprites,
                    has_sounds,
                    has_backgrounds,
                    has_paths,
                    has_scripts,
                    has_objects,
                    has_fonts,
                    has_timelines,
                )
                .expect("loading updated indexes failed")
            };
            // reload paths
            asset_maps_path.push("paths");
            let path_names = &asset_maps.paths.index;
            ide::PATHS.alloc(path_names.len());
            path_names
                .iter()
                .zip(ide::PATHS.assets_mut())
                .zip(ide::PATHS.names_mut())
                .try_for_each(|((name, asset), name_p)| -> Result<()> {
                    if !name.is_empty() {
                        *name_p = UStr::new(name);
                        *asset = Some(load::load_path(&mut asset_maps_path.join(name), &asset_maps)?);
                    }
                    Ok(())
                })
                .expect("loading updated paths failed");
            for (&form, path, name) in ide::PATHS
                .forms()
                .iter()
                .zip(ide::PATHS.assets())
                .zip(ide::PATHS.names())
                .filter(|((&f, _), _)| f != 0)
                .filter_map(|((f, o), n)| o.as_ref().map(|p| (f, p, n)))
            {
                (*((form + 0x444) as *const *mut asset::Path)).as_mut().map(|p| p.assign(path));
                (*((form + 0x448) as *const *mut asset::Path)).as_mut().map(|p| p.assign(path));
                (*((form + 0x454) as *const *mut asset::Path)).as_mut().map(|p| p.assign(path));
                *((form + 0x44c) as *mut UStr) = name.clone();
                *((form + 0x450) as *mut bool) = false;
                // update path form
                let _: u32 = delphi_call!(0x71ffe4, form);
            }
            asset_maps_path.pop();
            (**ide::RT_PATHS).DeleteChildren();
            load::read_resource_tree(ide::RT_PATHS, 8, "paths", &asset_maps.paths.map, true, &mut asset_maps_path)
                .expect("loading updated path tree failed");
            // reload room
            ide::ROOMS.assets_mut()[room_id] = Some(
                load::load_room(&mut room_path, &asset_maps)
                    .map_err(|e| e.to_string())
                    .expect("loading the updated room failed"),
            );
            room_path.pop();
            room_path.pop();
            update_timestamp();
            project_watcher::setup_watcher(&mut room_path);
            return 0
        }
    }
    delphi_call!(0x6884c8, room_id) // the default
}

unsafe fn patch(dest: usize, source: &[u8]) {
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
    let dest = dest as *mut u8;
    VirtualProtect(dest.cast(), source.len(), PAGE_READWRITE, &mut old_protect);
    dest.copy_from(source.as_ptr(), source.len());
    VirtualProtect(dest.cast(), source.len(), old_protect, &mut old_protect);
    FlushInstructionCache(GetCurrentProcess(), dest.cast(), source.len());
}

unsafe fn patch_call(instr: usize, proc: usize) {
    patch(instr + 1, &(proc - (instr as usize + 5)).to_le_bytes());
}

#[cfg_attr(not(test), ctor::ctor)]
#[cfg_attr(test, allow(dead_code))]
unsafe fn injector() {
    std::panic::set_hook(Box::new(|info| {
        let msg = UStr::new(info.to_string() + "\r\n\r\nPlease send a screenshot of this error message to Floogle.");
        let _: u32 = delphi_call!(0x51fbdc, *(0x7882ec as *const usize), msg.0, 0, 0x10);
        std::process::exit(-1);
    }));

    // do whatever needs doing when the IDE starts up
    patch(0x6deb83, &[0x90, 0x90, 0xe8]);
    patch_call(0x6deb83 + 2, stuff_to_do_on_ide_start as _);

    // accept only the first commandline argument as a project path
    patch(0x6dead7, &[0xeb]);

    patch_call(0x6deb0f, load_recent_project_and_maybe_compile as _);

    // about dialog
    #[rustfmt::skip]
    patch(0x71be58, &[
        0x8b, 0xc8, // mov ecx, eax
        0xe9, // jmp [nothing yet]
    ]);
    patch_call(0x71be5a, about_inj as _);

    // call save() instead of CStream.Create and the "save gmk" function
    let save_dest = 0x705cbd;
    #[rustfmt::skip]
    let mut save_patch = [
        0xe8, 0x00, 0x00, 0x00, 0x00, // call save (my save)
        0x84, 0xe4, // test ah,ah
        0x74, 0x0e, // je 0x705cd4 (after this patch)
        0x84, 0xc0, // test al,al
        0x74, 0x25, // je 0x705cef (after save fail)
        0xe9, 0x7e, 0x01, 0x00, 0x00, // jmp 0x705e4d (after save success)
    ];
    save_patch[1..5].copy_from_slice(&(save_inj as usize - (save_dest + 5)).to_le_bytes());
    patch(save_dest, &save_patch);

    // call load() instead of CStream.Create
    // and insert a JZ to the post-load code (0x705af3)
    let load_dest = 0x705a42;
    #[rustfmt::skip]
    let mut load_patch = [
        0xe8, 0x00, 0x00, 0x00, 0x00, // call load (my load)
        0x84, 0xc0, // test al,al
        0x0f, 0x85, 0xa4, 0x00, 0x00, 0x00, // jne 0x705af3 (after load)
    ];
    load_patch[1..5].copy_from_slice(&(load_inj as usize - (load_dest + 5)).to_le_bytes());
    patch(load_dest, &load_patch);

    // check for .gm82 as well as .gm81 when dragging file onto game maker
    patch_call(0x6df7e2, gm81_or_gm82_inj as _);
    // check for .gm82 as well as .gm81 in open file dialog
    patch_call(0x6e02ed, gm81_or_gm82_inj as _);
    // check for .gm82 as well as .gm81 in "rename if using an old file extension" code
    patch_call(0x6e0574, gm81_or_gm82_inj as _);
    // replace now-unused .gm81 with .gm82
    patch(0x6dfbec, &[b'2']);
    // save new .gm82 projects to subfolder when using "save as" dialog
    patch_call(0x6e06b3, make_new_folder as _);

    // fix stupid null pointer error
    patch(0x68ef02, &[0xe9]);
    patch_call(0x68ef02, fix_tile_null_pointer as _);

    // no need to refresh icon or redraw tree when closing resource forms
    patch(0x64e133, &[0x90; 10]);
    patch(0x6f5c1b, &[0x90; 10]);
    patch(0x722af3, &[0x90; 5]);
    patch(0x62ce43, &[0x90; 5]);
    patch(0x6525ab, &[0x90; 5]);
    patch(0x655f1b, &[0x90; 5]);
    patch(0x6931d7, &[0x90; 5]);
    patch(0x6fa8bb, &[0x90; 5]);
    patch(0x6fcf0b, &[0x90; 5]);

    // but do refresh icon when changes aren't saved
    patch_call(0x6f5152, update_sprite_icon_on_revert as _);
    patch_call(0x64d66a, update_background_icon_on_revert as _);

    // fix memory leak in image editor
    patch_call(0x643bd0, free_image_editor_bitmap as _);

    // don't dereference null pointer when changing image editor tool while mouse is down
    patch(0x643eb6, &[0x90, 0xe8]);
    patch_call(0x643eb7, image_editor_dont_error_when_switching_tool as _);

    // get default blend mode from form in image editor
    patch(0x64654d, &[
        0x8b, 0xc3, // mov eax, ebx
        0xe8, 0x58, 0xf6, 0xff, 0xff, // call TImageEditorForm.DrawModeGroupClick
        0x90, 0x90, 0x90, // nops
    ]);

    // copy origin when New
    patch_call(0x6ee2f8, copy_origin_on_new as _);

    // fix grid snap
    patch_call(0x64612b, floor_st0 as _);
    patch_call(0x646164, floor_st0 as _);
    patch_call(0x64639e, floor_st0 as _);
    patch_call(0x6463d7, floor_st0 as _);

    // don't skip font dwType 2 (otf)
    patch(0x6fb501, &[0x90, 0x90]);

    // don't skip font dwType 1 (bitmap fonts)
    patch(0x6fb504, &[0x90, 0x90]);

    // fix access violation when closing object/timeline window while mousing over action
    patch_call(0x6c6f6f, dont_show_action_tooltip_if_event_is_null as _);
    patch_call(0x6f9043, dont_show_action_tooltip_if_event_is_null as _);

    // update room forms if first object is created
    patch_call(0x71cfa2, first_object_updates_room_forms as _);

    // double clicking a timeline moment opens the first action
    patch_call(0x6f7f42, timeline_form_add_events as _);

    // go to parent by clicking on parent button
    patch_call(0x6c515e, object_form_add_events as _);

    // clean collision events and mark as modified when deleting objects
    patch_call(0x62ca82, object_clean_collide_events_inj as _);

    // clean trigger events when deleting triggers
    patch(0x6bcc94, &[0xe8, 0x00, 0x00, 0x00, 0x00, 0x90, 0x90]);
    patch_call(0x6bcc94, object_clean_triggers_inj as _);

    // add scrolling to path form
    patch_call(0x71fdcb, path_form_mouse_wheel_inj as _);

    // changing room in path form counts as a change
    patch_call(0x7211ff, path_room_change_forces_room_editor_save as _);

    // don't show Trace.log after debug run
    patch(0x6d83f1, &[0xe9, 0x45, 0x05, 0x00, 0x00]);
    // load DebugTraceCheckBox as TValueEdit
    patch(0x71a6bb, &[
        0xe8, 0xa0, 0x60, 0xe1, 0xff, // call TValueEdit.SetValue
        0x90, 0x90, 0x90, // nop slide
    ]);
    // save DebugTraceCheckBox as TValueEdit
    patch(0x71aad5, &[
        0x8b, 0x80, 0xa0, 0x02, 0x00, 0x00, // mov eax, DebugTraceCheckBox.i_value
        0xa2, 0x98, 0xa9, 0x79, 0x00, // mov [compression_value], al
    ]);
    // set default to 1
    patch(0x71792e, &[0xb2, 0x01]);
    // load from registry as int
    patch(0x717936, &[0x96, 0xf4]);
    // save to registry as int
    patch(0x719068, &[0x94]);
    // ShowDebugTrace -> TestComprLevel
    #[rustfmt::skip]
    patch(0x7187dc, &[
        b'T', 0, b'e', 0, b's', 0, b't',
        b'C', 0, b'o', 0, b'm', 0, b'p', 0, b'r', 0,
        b'L', 0, b'e', 0, b'v', 0, b'e', 0, b'l'
    ]);
    #[rustfmt::skip]
    patch(0x719da0, &[
        b'T', 0, b'e', 0, b's', 0, b't', 0,
        b'C', 0, b'o', 0, b'm', 0, b'p', 0, b'r', 0,
        b'L', 0, b'e', 0, b'v', 0, b'e', 0, b'l', 0,
    ]);

    // use zlib-ng for compression
    patch(0x52f34c, &[0xe9]);
    patch_call(0x52f34c, deflate_inj as _);
    patch(0x52f2e4, &[0xe9]);
    patch_call(0x52f2e4, inflate_inj as _);
    // build fast when making test build
    patch_call(0x6ce8a2, build_fast as _);
    patch_call(0x6ce8cb, reset_compression as _);
    // build small when making release
    patch_call(0x6ce775, build_small as _);
    patch_call(0x6ce78f, reset_compression as _);

    // compiler injections
    compiler::inject();
    // reset extra data, unwatch project folder, and add a blank object when loading a new project
    patch_call(0x70598c, stuff_to_do_on_project_init as _);

    // read text as ANSI on pre-8.1
    patch(0x70537b, &[0xe8]);
    patch_call(0x70537b, setup_unicode_parse_inj as _);
    // reset above
    patch_call(0x705acc, teardown_unicode_parse_inj as _);

    // .gm82 file associations
    patch_call(0x6ddacd, gm82_file_association_inj as _);

    // fix access violation when pasting empty clipboard
    patch(0x6b8a7f + 1, &[0x0d]);

    // check if extensions need updating when drawing code
    patch(0x6ab18c, &[0xe9]);
    patch_call(0x6ab18c, maybe_reload_extensions_when_typing as _);

    // code editor don't resize on maximize
    // script resize
    patch(0x65508c, &[0xe8, 0, 0, 0, 0, 0x90]);
    patch_call(0x65508c, code_editor_better_resize as _);
    // codeaction resize
    patch(0x682bf0, &[0xe8, 0, 0, 0, 0, 0x90]);
    patch_call(0x682bf0, code_editor_better_resize as _);

    // but also don't resize on create *if* maximized
    patch(0x653d83, &[0xe8, 0, 0, 0, 0, 0x90]);
    patch_call(0x653d83, code_editor_dont_resize_if_maximized as usize);

    // middle click in code editor shows resource
    // remove first check
    patch(0x6b7182, &[0x90, 0x90, 0x90, 0x90, 0x90, 0x90]);
    // inject second check
    patch_call(0x6b721b, code_editor_middle_click as _);

    // code hint: faster extension function search and add script hints
    patch(0x71364e, &[0xae, 0x22]);
    #[rustfmt::skip]
    patch(0x6bb12e, &[
        // prior line: mov eax, [ebp-4]
        0x8b, 0x55, 0xf8, // mov edx, [ebp-8]
        0xe8, 0xde, 0x84, 0x05, 0x00, // call extension_get_helpline_from_function_name
        0x8b, 0x55, 0xf8, // mov edx, [ebp-8]
        0x8b, 0x02, // mov eax, [edx]
        0x85, 0xc0, // test eax, eax
        0x75, 0x10, // jnz to function end
        0x8b, 0x4d, 0xfc, // mov ecx, [ebp-4]
        0xe8, 0, 0, 0, 0, // call code_editor_script_hint
        0xeb, 0x06, // jmp to function end
    ]);
    patch_call(0x6bb142, code_editor_script_hint as _);

    // script args in code completion
    patch_call(0x6baa91, completion_script_args_inj as _);
    patch(0x6baa98, &[0xa8]); // get previous script name result

    // fix triggers in code completion
    patch(0x6baa1c, &[0x98, 0x23]); // get trigger const instead of name
    patch(0x6baa2e, &[
        0x6a, 0x00, // push 0
        0x6a, 0x04, // push 4
        0x8d, 0x54, 0x24, 0x4, // lea edx, [esp+4]
        0x8b, 0xcb, // mov ecx, ebx (the mov to eax afterwards is useless but that's fine)
    ]);
    patch_call(0x6baa3a, add_space_before_trigger_name as usize);
    patch(0x6baa41, &[0xb0]);

    // show number on code actions
    patch_call(0x7002fe, write_number_on_actions as _);

    // default room editor settings
    patch(0x657852, &[0xe8, 0, 0, 0, 0, 0x90, 0x90]);
    patch_call(0x657852, room_size as _);

    // nop out room view size stuff
    patch(0x657904, &[0x90; 14]);
    patch(0x65791c, &[0x90; 14]);

    // fix instance references in creation code when renaming room
    #[rustfmt::skip]
    patch(0x692fbb, &[
        0xe8, 0, 0, 0, 0, // call rename_room_inj
        0x74, 0x2a, // jz to end of function
        0x90, // nop
    ]);
    patch_call(0x692fbb, rename_room_inj as _);

    // replace ids in new room when duplicating
    #[rustfmt::skip]
    patch(0x692e72, &[
        0x8b, 0x14, 0x98, // mov edx, [eax+ebx*4]
        0x8b, 0x04, 0xb0, // mov eax, [eax+esi*4]
        0x56, // push esi (new id)
        0x53, // push ebx (old id)
        0x50, // push eax (room ptr)
        0xe8, 0x14, 0x4b, 0xfc, 0xff, // call CRoom.Assign
        0xe8, 0x00, 0x00, 0x00, 0x00, // call freshen_room_ids
        0x90, 0x90, 0x90, // nop padding
    ]);
    patch_call(0x692e80, duplicate_room as _);

    // show instance id in old room editor
    patch_call(0x68fbc9, show_instance_id_inj as _);

    // funky room editor shit
    patch_call(0x69319c, room_form_inj as _);
    // disable news (replace function with a ret)
    patch(0x62c224, &[0xc3]);
    // don't open gm82room when creating/duplicating a new room
    patch_call(0x6e2f86, dont_make_room_form_inj as _);
    patch_call(0x6e2f5c, dont_make_room_form_inj as _);
    patch_call(0x6e169e, dont_make_room_form_inj as _);

    // configs for default room editor settings
    // force progress bar (replace check with nops)
    patch(0x6ca266, &[0x90, 0x90]);
    // replace ShowProgress with DefRoomW
    patch(0x717cbc, b"\x08\0\0\0D\0e\0f\0R\0o\0o\0m\0W\0\0\0");
    patch(0x719350, b"\x08\0\0\0D\0e\0f\0R\0o\0o\0m\0W\0\0\0");
    // replace NoWebsite with DefRoomH
    patch(0x7189e8, b"\x08\0\0\0D\0e\0f\0R\0o\0o\0m\0H\0\0\0");
    patch(0x719e0c, b"\x08\0\0\0D\0e\0f\0R\0o\0o\0m\0H\0\0\0");
    // replace NewsBrowser with DefRoomS
    patch(0x718a24, b"\x08\0\0\0D\0e\0f\0R\0o\0o\0m\0S\0\0\0");
    patch(0x719e48, b"\x08\0\0\0D\0e\0f\0R\0o\0o\0m\0S\0\0\0");
    // read ShowProgress from reg as int
    patch(0x7170f7, &[0x33, 0xd2]);
    patch(0x7170fe, &[0xe8, 0xcd, 0xfc, 0xff, 0xff, 0xa3]);
    patch(0x717104, &(&DEFAULT_ROOM_WIDTH as *const u32 as usize as u32).to_le_bytes());
    // read NoWebsite from reg as int
    patch(0x71799d, &[0xe8, 0x2e, 0xf4, 0xff, 0xff, 0xa3]);
    patch(0x7179a3, &(&DEFAULT_ROOM_HEIGHT as *const u32 as usize as u32).to_le_bytes());
    // read NewsBrowser from reg as int
    patch(0x7179bf, &[0xe8, 0x0c, 0xf4, 0xff, 0xff, 0xe8]);
    patch_call(0x7179c4, fix_broken_room_size as _);
    // write ShowProgress to reg as int
    patch(0x718bac, &[0x8b, 0x15, 0x2c, 0xa8, 0x79, 0x00, 0x90]);
    patch(0x718bb9, &[0x43, 0xe0]);
    patch(0x718bae, &(&DEFAULT_ROOM_WIDTH as *const u32 as usize as u32).to_le_bytes());
    // write NoWebsite to reg as int
    patch(0x71908e, &[0x8b, 0x15, 0x81, 0xa9, 0x79, 0x00, 0x90]);
    patch(0x71909b, &[0x61, 0xdb]);
    patch(0x719090, &(&DEFAULT_ROOM_HEIGHT as *const u32 as usize as u32).to_le_bytes());
    // write NewsBrowser to reg as int
    patch(0x7190b0, &[0x8b, 0x15, 0x83, 0xa9, 0x79, 0x00, 0x90]);
    patch(0x7190bd, &[0x3f, 0xdb]);
    patch(0x7190b2, &(&DEFAULT_ROOM_SPEED as *const u32 as usize as u32).to_le_bytes());
    // write ShowProgress to form as ValueEdit
    patch(0x71a272, &[0x8b, 0x15, 0x2c, 0xa8, 0x79, 0x00, 0xe8, 0xe3, 0x64, 0xe1, 0xff, 0x90, 0x90, 0x90, 0x90]);
    patch(0x71a274, &(&DEFAULT_ROOM_WIDTH as *const u32 as usize as u32).to_le_bytes());
    // write NoWebsite to form as ValueEdit
    patch(0x71a4ec, &[0x8b, 0x15, 0x81, 0xa9, 0x79, 0x00, 0xe8, 0x69, 0x62, 0xe1, 0xff, 0x90, 0x90, 0x90, 0x90]);
    patch(0x71a4ee, &(&DEFAULT_ROOM_HEIGHT as *const u32 as usize as u32).to_le_bytes());
    // write NewsBrowser to form as ValueEdit
    patch(0x71a51d, &[0x8b, 0x15, 0x83, 0xa9, 0x79, 0x00, 0xe8, 0x38, 0x62, 0xe1, 0xff, 0x90, 0x90, 0x90, 0x90]);
    patch(0x71a51f, &(&DEFAULT_ROOM_SPEED as *const u32 as usize as u32).to_le_bytes());
    // read ShowProgress from form as ValueEdit
    patch(0x71a777, &[0x8b, 0x80, 0xa0, 0x02, 0x00, 0x00, 0xa3, 0x2c, 0xa8, 0x79, 0x00, 0x90, 0x90]);
    patch(0x71a77e, &(&DEFAULT_ROOM_WIDTH as *const u32 as usize as u32).to_le_bytes());
    // read NoWebsite from form as ValueEdit
    patch(0x71a93f, &[0x8b, 0x80, 0xa0, 0x02, 0x00, 0x00, 0xa3, 0x81, 0xa9, 0x79, 0x00, 0x90, 0x90]);
    patch(0x71a946, &(&DEFAULT_ROOM_HEIGHT as *const u32 as usize as u32).to_le_bytes());
    // read NewsBrowser from form as ValueEdit
    patch(0x71a96b, &[0x8b, 0x80, 0xa0, 0x02, 0x00, 0x00, 0xa3, 0x83, 0xa9, 0x79, 0x00, 0x90, 0x90]);
    patch(0x71a972, &(&DEFAULT_ROOM_SPEED as *const u32 as usize as u32).to_le_bytes());
    // update menu box
    patch_call(0x71aaf7, close_preferences_form as usize);

    // toggle gm82room instead of opening news
    patch_call(0x6e2002, toggle_gm82room_checkbox as _);

    // check for other processes before setting MakerRunning to false
    patch(0x71af15, &[0xb9]);
    patch_call(0x71af15, check_gm_processes as _);

    patch_call(0x75e88c, trace_date_inj as _);

    // error box only shows once and has custom message
    patch_call(0x51fe33, patch_error_box as _);

    // regenerate temp directory if it doesn't exist
    patch_call(0x5342e8, regen_temp_folder_when_making_file as _);
    patch_call(0x6ce82b, get_temp_folder_but_also_regen_it as _);

    // add three newest resources to popup menus
    patch_call(0x71c6a0, get_treenode_count_and_preserve_resource_type as _);
    patch(0x71c6dd, &[0xe9]);
    patch_call(0x71c6dd, add_three_newest_inj as _);

    // update timestamps when setting name
    unsafe fn patch_timestamps(dest: usize) {
        patch(dest, &[0x8b, 0xc3, 0xe8, 0xe0, 0x00, 0x00, 0x00]);
    }
    patch(0x62cbe9, &[0x8b, 0xc3, 0xe8, 0x3c, 0x01, 0x00, 0x00]); // objects
    patch_timestamps(0x6f59e1); // sprites
    patch_timestamps(0x652381); // sounds
    patch_timestamps(0x692fe5); // rooms
    patch_timestamps(0x64def9); // backgrounds
    patch_timestamps(0x655c01); // scripts
    patch_timestamps(0x722901); // paths
    patch_timestamps(0x6fcd19); // fonts
    patch_timestamps(0x6fa6c9); // timelines

    // fix objects/timelines updating the wrong timestamp
    patch_call(0x6c73ef, properly_update_object_timestamp_drag_drop as _);
    patch_call(0x6f94c3, properly_update_timeline_timestamp_drag_drop as _);
    patch_call(0x6c7512, properly_update_object_timestamp_right_click as _);
    patch_call(0x6f95e6, properly_update_timeline_timestamp_right_click as _);

    // update timestamp properly in mask form
    unsafe fn patch_timestamp_mask(dest: usize) {
        patch(dest, &[0xe8, 0, 0, 0, 0, 0x90, 0x90, 0x90]);
        patch_call(dest, update_sprite_mask_timestamp as _);
    }
    patch_timestamp_mask(0x6f3208);
    patch_timestamp_mask(0x6f33fa);
    patch_timestamp_mask(0x6f34e8);
    patch_timestamp_mask(0x6f3555);

    // check for time going backwards
    patch(0x4199fb, &[0xe9]);
    patch_call(0x4199fb, reset_if_time_went_backwards as _);

    patch_call(0x6cd943, save_exe::save_assets_inj::<asset::Sprite> as usize);
    patch_call(0x6cd95e, save_exe::save_assets_inj::<asset::Background> as usize);
    patch_call(0x06cd979, save_exe::save_assets_inj::<asset::Path> as usize);
    patch_call(0x6cd994, save_exe::save_assets_inj::<asset::Script> as usize);
    patch_call(0x6cd9af, save_exe::save_assets_inj::<asset::Font> as usize);
    patch_call(0x6cd9f8, save_exe::save_assets_inj::<asset::Room> as usize);

    patch_call(0x6ce104, save_exe::write_encrypted_gamedata_inj as usize);
}
