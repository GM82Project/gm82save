#![feature(asm)]

#[cfg(not(all(windows, target_arch = "x86")))]
compile_error!("this tool only works on windows 32-bit");

mod asset;
mod delphi;
mod ide;
mod save;
mod stub;

use crate::delphi::UStr;
use ctor::ctor;
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
    Other(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IoError(e) => write!(f, "io error: {}", e),
            Self::ImageError(e) => write!(f, "image error: {}", e),
            Self::UnicodeError(s) => write!(f, "couldn't encode {}", s),
            Self::AssetNotFound(s) => write!(f, "couldn't find asset {}", s),
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

pub type Result<T> = std::result::Result<T, Error>;

fn show_message(msg: &str) {
    unsafe {
        delphi::ShowMessage(&UStr::new(msg.as_ref()));
    }
}

unsafe extern "C" fn save() -> u32 {
    // get the path to the yyd file
    let ebp: *const UStr;
    asm!("mov {}, [ebp]", out(reg) ebp);
    let real_string = &*ebp.sub(1);
    let path = real_string.to_os_string();

    if let Err(e) = save::save_gmk(path.into()) {
        // display the error
        show_message(&format!("Failed to save: {}", e));
    }
    0
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

    // call save() instead of the "generate gm81" function
    // and insert a jump call to the post-save code
    let save_dest = 0x705cd0 as *mut u8;
    let mut save_patch = [0, 0, 0, 0, 0xe9, 0x67, 1, 0, 0];
    save_patch[..4].copy_from_slice(&(save as u32 - (save_dest as u32 + 4)).to_le_bytes());
    patch(save_dest, &save_patch);
    // replace .gm81 with .yyd
    patch(0x6e05e0 as *mut u8, &[0x04, 0, 0, 0, b'.', 0, b'y', 0, b'y', 0, b'd', 0, 0, 0]);
    patch(0x6e0728 as *mut u8, &[0x04, 0, 0, 0, b'.', 0, b'y', 0, b'y', 0, b'd', 0, 0, 0]);
    // patch out file extension associations
    patch(0x6de76b as *mut u8, &[0x90, 0x90, 0x90, 0x90, 0x90]);
}
