#![allow(non_snake_case, dead_code)]

use std::{
    ffi::{OsStr, OsString},
    os::windows::ffi::{OsStrExt, OsStringExt},
    slice,
};

macro_rules! delphi_call {
    ($call: literal) => {{
        let out;
        asm! {
            "mov ecx, {call}",
            "call ecx",
            call = const $call,
            lateout("eax") out,
            lateout("edx") _,
            lateout("ecx") _,
        };
        out
    }};
    ($call: literal, $a: expr) => {{
        let out;
        asm! {
            "mov ecx, {call}",
            "call ecx",
            call = const $call,
            inlateout("eax") $a => out,
            lateout("edx") _,
            lateout("ecx") _,
        };
        out
    }};
    ($call: literal, $a: expr, $b: expr) => {{
        let out;
        asm! {
            "mov ecx, {call}",
            "call ecx",
            call = const $call,
            inlateout("eax") $a => out,
            inlateout("edx") $b => _,
            lateout("ecx") _,
        };
        out
    }};
    ($call: literal, $a: expr, $b: expr, $c: expr) => {{
        let out;
        asm! {
            "mov ebx, {call}",
            "call ebx",
            call = const $call,
            inlateout("eax") $a => out,
            inlateout("edx") $b => _,
            inlateout("ecx") $c => _,
            lateout("ebx") _,
        };
        out
    }};
}

// this is really game maker specific but i left it here for simplicity
#[repr(C)]
pub struct TreeNodeData {
    unknown: u32,
    pub rtype: u32, // 0 for toplevel, 1 for group, 2 for folder i think?
    pub kind: u32,  // what resource type i.e. sprite, sound, etc
    pub index: u32, // resource index
}

#[repr(C)]
pub struct TTreeNode {
    unknown: u64,
    pub name: UStr,
    pub data: *const TreeNodeData,
}

impl TTreeNode {
    pub unsafe fn GetCount(&self) -> u32 {
        delphi_call!(0x4ad490, self)
    }

    pub unsafe fn GetItem(&self, index: u32) -> *const Self {
        delphi_call!(0x4ad3b4, self, index)
    }

    pub unsafe fn SetData(&self, data: *const TreeNodeData) {
        let _: u32 = delphi_call!(0x4ac9b4, self, data);
    }

    pub unsafe fn SetImageIndex(&self, index: i32) {
        let _: u32 = delphi_call!(0x4acb64, self, index);
    }
}

#[repr(C)]
pub struct TTreeNodes {}

impl TTreeNodes {
    pub unsafe fn AddChild(&self, parent: *const TTreeNode, s: &UStr) {
        let _: u32 = delphi_call!(0x4ae1e8, self, parent, s.0);
    }

    pub unsafe fn Clear(&self) {
        let _: u32 = delphi_call!(0x4ae12c, self);
    }
}

pub struct TGraphic {}

impl TGraphic {
    pub unsafe fn SaveToFile(&self, filename: &UStr) {
        // NOTE: FILENAME MUST BE FULLY FORMED
        let _: u32 = delphi_call!(0x45e6d8, self, filename.0);
    }

    pub unsafe fn LoadFromFile(&self, filename: &UStr) {
        // NOTE: FILENAME MUST BE FULLY FORMED
        let _: u32 = delphi_call!(0x45e64c, self, filename.0);
    }
}

pub struct TStream {}

impl TStream {
    pub unsafe fn get_pos(&self) -> u32 {
        delphi_call!(0x43f234, self)
    }

    pub unsafe fn set_pos(&self, pos: u32) {
        asm! {
            "push edx",
            "push 0",
            "mov ecx,{call}",
            "call ecx",
            call = const 0x43f254,
            inlateout("eax") self => _,
            inlateout("edx") pos => _,
            lateout("ecx") _,
        };
    }

    pub unsafe fn get_size(&self) -> u32 {
        let out;
        asm! {
            "mov ecx,[eax]",
            "call [ecx]",
            inlateout("eax") self => out,
            lateout("edx") _,
            lateout("ecx") _,
        };
        out
    }

    pub unsafe fn read(&self, buf: *mut u8, count: u32) {
        let _: u32 = delphi_call!(0x43f488, self, buf, count);
    }
}

pub struct TStrings {}

impl TStrings {
    pub unsafe fn SaveToFile(&self, fname: &UStr) {
        let _: u32 = delphi_call!(0x43e204, self, fname.0);
    }
}

// weird name for an allocator function
pub unsafe fn GetMem<T>(size: u32) -> *const T {
    delphi_call!(0x40431c, size)
}

pub unsafe fn FreeMem<T>(mem: *const T) {
    let _: u32 = delphi_call!(0x404338, mem);
}

pub unsafe fn UStrFromPCharLen(dest: &mut UStr, source: *const u8, length: usize) {
    let _: u32 = delphi_call!(0x407fe4, dest, source, length);
}

pub unsafe fn UStrFromPWCharLen(dest: &mut UStr, source: *const u16, length: usize) {
    let _: u32 = delphi_call!(0x407ff4, dest, source, length);
}

pub unsafe fn UStrClr(str: &mut UStr) {
    let _: u32 = delphi_call!(0x407ea8, str.0);
}

#[repr(transparent)]
pub struct UStr(pub(self) *const u16);

impl UStr {
    pub fn new(s: &OsStr) -> Self {
        let mut out = UStr(std::ptr::null());
        let s = s.encode_wide().collect::<Vec<_>>();
        unsafe {
            UStrFromPWCharLen(&mut out, s.as_ptr(), s.len());
        }
        out
    }

    pub fn to_os_string(&self) -> OsString {
        if self.0.is_null() {
            OsString::new()
        } else {
            unsafe {
                let len = self.0.cast::<u32>().sub(1).read();
                OsString::from_wide(slice::from_raw_parts(self.0, len as usize))
            }
        }
    }
}

impl Drop for UStr {
    fn drop(&mut self) {
        unsafe { UStrClr(self) }
    }
}

pub unsafe fn ShowMessage(msg: &UStr) {
    let _: u32 = delphi_call!(0x4d43f8, msg.0);
}

pub fn advance_progress_form(progress: u32) {
    unsafe {
        let _: u32 = delphi_call!(0x6ca2ac, progress);
    }
}

pub unsafe fn Now() -> f64 {
    let out: f64;
    asm! {
        "mov ecx, {call}",
        "call ecx",
        "sub esp,8",
        "fstp qword ptr [esp]",
        "movsd {out}, [esp]",
        "add esp,8",
        call = const 0x4199b0,
        out = lateout(xmm_reg) out,
        lateout("eax") _,
        lateout("edx") _,
        lateout("ecx") _,
    };
    out
}
