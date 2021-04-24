// This DLL is emulating winspool.drv, and these are stubs for the functions that GM8 imports.

#[no_mangle]
pub extern "system" fn ClosePrinter(_a: u32) {}
#[no_mangle]
pub extern "system" fn DocumentPropertiesW(_a: u32, _b: u32, _c: u32, _d: u32, _e: u32, _f: u32) {}
#[no_mangle]
pub extern "system" fn EnumPrintersW(_a: u32, _b: u32, _c: u32, _d: u32, _e: u32, _f: u32, _g: u32) {}
#[no_mangle]
pub extern "system" fn GetDefaultPrinterW(_a: u32, _b: u32) {}
#[no_mangle]
pub extern "system" fn OpenPrinterW(_a: u32, _b: u32, _c: u32) {}
