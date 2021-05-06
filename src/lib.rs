#![feature(asm)]

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
use ctor::ctor;
use std::path::PathBuf;
use winapi::um::{
    memoryapi::VirtualProtect,
    processthreadsapi::{FlushInstructionCache, GetCurrentProcess},
    winnt::PAGE_READWRITE,
};

pub enum Error {
    IoError(std::io::Error),
    ImageError(image::ImageError),
    UnicodeError(String),
    AssetNotFound(String),
    SyntaxError(PathBuf),
    UnknownKey(PathBuf, String),
    UnknownAction(u32, u32),
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError),
    InvalidVersion(String),
    Other(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IoError(e) => write!(f, "io error: {}", e),
            Self::ImageError(e) => write!(f, "image error: {}", e),
            Self::UnicodeError(s) => write!(f, "couldn't encode {}", s),
            Self::AssetNotFound(s) => write!(f, "couldn't find asset {}", s),
            Self::SyntaxError(p) => write!(f, "syntax error in file {}", p.to_string_lossy()),
            Self::UnknownKey(p, k) => write!(f, "unknown key in {}: {}", p.to_string_lossy(), k),
            Self::UnknownAction(lib_id, act_id) => write!(f, "unknown action {} in lib with id {}", act_id, lib_id),
            Self::ParseIntError(e) => write!(f, "integer parse error: {}", e),
            Self::ParseFloatError(e) => write!(f, "float parse error: {}", e),
            Self::InvalidVersion(v) => write!(f, "invalid exe_version {}", v),
            Self::Other(s) => write!(f, "other error: {}", s),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Error::IoError(err)
    }
}

impl From<image::ImageError> for Error {
    fn from(err: image::ImageError) -> Self {
        Error::ImageError(err)
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
        delphi::ShowMessage(&UStr::new(msg.as_ref()));
    }
}

// set the high byte to nonzero if YYD save code was used
// set the low byte to nonzero on success
unsafe extern "C" fn save() -> u16 {
    const IS_YYD: u16 = 0x100;
    // get the path to the project file
    let ebp: *mut UStr;
    asm!("mov {}, [ebp]", out(reg) ebp);
    let real_string = &*ebp.sub(1);
    let path: PathBuf = real_string.to_os_string().into();
    // filename ".gm82" works in the ui but rust doesn't get it so check for that specifically
    let is_gm82 = path.extension() == Some("gm82".as_ref()) || path.file_name() == Some(".gm82".as_ref());
    if !is_gm82 {
        // CStream.Create
        let buf = delphi_call!(0x405a4c, 0x52e8fc, 1);
        ebp.sub(5).cast::<u32>().write(buf);
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

unsafe extern "C" fn load() -> bool {
    let ebp: *mut UStr;
    asm!("mov {}, [ebp]", out(reg) ebp);
    let real_string = &*ebp.sub(1);
    let path: PathBuf = real_string.to_os_string().into();
    // .gm82 works in the ui but rust doesn't get it so check for that specifically
    let is_gm82 = path.extension() == Some("gm82".as_ref()) || path.file_name() == Some(".gm82".as_ref());
    if !is_gm82 {
        let obj = delphi_call!(0x405a4c, 0x52e8fc, 1);
        ebp.sub(3).cast::<u32>().write(obj);
        return false
    }

    if let Err(e) = load::load_gmk(path) {
        // display the error and reload
        delphi::close_progress_form();
        show_message(&format!("Failed to load: {}", e));
        ide::initialize_project();
    } else {
        delphi::close_progress_form();
        ebp.cast::<bool>().sub(5).write(true);
    }
    true
}

unsafe extern "C" fn gm81_or_gm82() -> i32 {
    let ebp: *const UStr;
    asm!("mov {}, ebp", out(reg) ebp); // no [] because this function doesn't push ebp when compiled
    let real_string = &*ebp.sub(8);
    // original .gm81 compare
    let out = delphi::CompareText(real_string, &UStr::from_static_pointer(0x6dfbe4 as _));
    if out != 0 {
        // new .gm82 compare
        delphi::CompareText(real_string, &UStr::from_static_pointer(0x6e05e4 as _))
    } else {
        out
    }
}

unsafe fn patch(dest: *mut u8, source: &[u8]) {
    let mut old_protect = 0;
    VirtualProtect(dest.cast(), source.len(), PAGE_READWRITE, &mut old_protect);
    dest.copy_from(source.as_ptr(), source.len());
    VirtualProtect(dest.cast(), source.len(), old_protect, &mut old_protect);
    FlushInstructionCache(GetCurrentProcess(), dest.cast(), source.len());
}

#[ctor]
unsafe fn injector() {
    std::panic::set_hook(Box::new(|info| {
        show_message(&info.to_string());
    }));

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
    save_patch[1..5].copy_from_slice(&(save as u32 - (save_dest as u32 + 5)).to_le_bytes());
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
    load_patch[1..5].copy_from_slice(&(load as u32 - (load_dest as u32 + 5)).to_le_bytes());
    patch(load_dest, &load_patch);

    // check for .gm82 as well as .gm81 when dragging file onto game maker
    patch(0x6df7e3 as *mut u8, &(gm81_or_gm82 as u32 - 0x6df7e7).to_le_bytes());
    // check for .gm82 as well as .gm81 in open file dialog
    patch(0x6e02ee as *mut u8, &(gm81_or_gm82 as u32 - 0x6e02f2).to_le_bytes());
    // replace .gm81 with .gm82 in "rename if using an old file extension" code
    patch(0x6e05ec as *mut u8, &[b'2']);
    // replace .gm81 with .gm82 in "generate a default filename to save to" code
    patch(0x6e0734 as *mut u8, &[b'2']);
    // patch out file extension associations
    patch(0x6de76b as *mut u8, &[0x90, 0x90, 0x90, 0x90, 0x90]);
}
