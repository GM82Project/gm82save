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

fn show_message(msg: &str) {
    unsafe {
        delphi::ShowMessage(&UStr::new(msg.as_ref()));
    }
}

unsafe extern "C" fn injected() -> u32 {
    // get the path to the gm81 file
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

    // call injected() instead of the "generate gm81" function
    // and insert a jump call after that
    let func_dest = 0x705cd0 as *mut u8;
    let mut func_patch = [0, 0, 0, 0, 0xe9, 0x67, 1, 0, 0];
    func_patch[..4].copy_from_slice(&(injected as u32 - (func_dest as u32 + 4)).to_le_bytes());
    patch(func_dest, &func_patch);
    // replace .gm81 with .yyd
    patch(0x6e05e0 as *mut u8, &[0x04, 0, 0, 0, b'.', 0, b'y', 0, b'y', 0, b'd', 0, 0, 0]);
    patch(0x6e0728 as *mut u8, &[0x04, 0, 0, 0, b'.', 0, b'y', 0, b'y', 0, b'd', 0, 0, 0]);
    // patch out file extension associations
    patch(0x6de76b as *mut u8, &[0x90, 0x90, 0x90, 0x90, 0x90]);
}
