#![feature(asm, naked_functions)]

#[cfg(not(all(windows, target_arch = "x86")))]
compile_error!("this tool only works on windows 32-bit");

mod asset;
#[macro_use]
mod delphi;
mod events;
mod ide;
mod load;
mod save;
mod stub;

use crate::delphi::UStr;
use std::{collections::HashMap, ffi::c_void, path::PathBuf};

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

static mut EXTRA_DATA: Option<(HashMap<u32, InstanceExtra>, HashMap<u32, TileExtra>)> = None;

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
    EXTRA_DATA = Some(Default::default());
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
unsafe extern "C" fn save_82_if_exe() {
    asm! {
        "mov ecx, 825",
        "mov edx, 800",
        "test bl, bl",
        "cmovnz edx, ecx",
        "ret",
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn save_bool_if_exe() {
    asm! {
        "push esi",
        "mov esi, 0x52f240",
        "mov ecx, 0x52f12c",
        "test bl, bl",
        "cmovnz ecx, esi",
        "call ecx",
        "pop esi",
        "ret",
        options(noreturn),
    }
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
unsafe extern "C" fn save_creation_code_flag_inj() {
    asm! {
        "mov ecx, 0x52f12c",
        "call ecx",
        "test bl, bl",
        "jnz {}",
        "ret",
        sym save_creation_code_flag,
        options(noreturn),
    }
}

#[naked]
unsafe extern "C" fn save_creation_code_flag() {
    asm! {
        "mov eax, esi",
        "xor edx, edx",
        "mov ecx, 0x52f12c",
        "call ecx",
        "mov eax, esi",
        "mov edx, [0x77f54c]",
        "shr edx, 31",
        "mov ecx, 0x52f240",
        "call ecx",
        "ret",
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

unsafe extern "fastcall" fn save_instance_extra(file: usize, id: u32) {
    if let Some(data) = EXTRA_DATA.as_ref().map(|(insts, _)| insts.get(&id).unwrap_or(&InstanceExtra::DEFAULT)) {
        save_real(file, &data.xscale);
        save_real(file, &data.yscale);
        let _: u32 = delphi_call!(0x52f12c, file, data.blend);
        save_real(file, &data.angle);
    }
}

unsafe extern "fastcall" fn save_tile_extra(file: usize, id: u32) {
    if let Some(data) = EXTRA_DATA.as_ref().map(|(_, tiles)| tiles.get(&id).unwrap_or(&TileExtra::DEFAULT)) {
        save_real(file, &data.xscale);
        save_real(file, &data.yscale);
        let _: u32 = delphi_call!(0x52f12c, file, data.blend);
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
unsafe extern "C" fn room_form_inj() {
    asm! {
        "mov ecx, eax",
        "jmp {}",
        sym room_form,
        options(noreturn),
    }
}

static mut SAVING_FOR_ROOM_EDITOR: bool = false;

unsafe extern "fastcall" fn room_form(room_id: usize) -> u32 {
    if *(0x79a982 as *const bool) {
        return delphi_call!(0x6884c8, room_id)
    }
    let editor_path = match std::env::current_exe() {
        Ok(mut path) => {
            path.set_file_name("room_editor.exe");
            path
        },
        Err(_) => return delphi_call!(0x6884c8, room_id),
    };
    if editor_path.exists() {
        let mut room_path = PathBuf::from((&*ide::PROJECT_PATH).to_os_string());
        if room_path.extension() == Some("gm82".as_ref()) {
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
                    ide::get_rooms_mut()[room_id] = load::load_room(&mut room_path, &asset_maps)
                        .ok()
                        .expect("loading the updated room failed")
                        .as_ref();
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
        show_message(&info.to_string());
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

    // save creation code flag (reusing the software vertex processing flag)
    // write 825 instead of 800 for settings version if saving exe
    patch(0x70997c as _, &[0xe8]);
    patch_call(0x70997c as _, save_82_if_exe as _);
    // call WriteBoolean instead of WriteInteger if saving exe
    patch_call(0x709a4f as _, save_bool_if_exe as _);
    // save extra info if saving exe
    patch_call(0x709c99 as _, save_creation_code_flag_inj as _);

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

    // read text as ANSI on pre-8.1
    patch(0x70537b as _, &[0xe8]);
    patch_call(0x70537b as _, setup_unicode_parse_inj as _);
    // reset above
    patch_call(0x705acc as _, teardown_unicode_parse_inj as _);

    // funky room editor shit
    patch_call(0x69319c as _, room_form_inj as _);
    // disable news (replace function with a ret)
    patch(0x62c224 as _, &[0xc3]);

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
