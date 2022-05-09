#![allow(non_snake_case, dead_code)]

use std::{
    arch::asm,
    ffi::{OsStr, OsString},
    os::windows::ffi::{OsStrExt, OsStringExt},
    ptr, slice,
};

#[macro_export]
macro_rules! delphi_call {
    ($call: literal) => {{
        let out;
        std::arch::asm! {
            "call {call}",
            call = in(reg) $call,
            lateout("eax") out,
            lateout("edx") _,
            lateout("ecx") _,
        };
        out
    }};
    ($call: literal, $a: expr) => {{
        let out;
        std::arch::asm! {
            "call {call}",
            call = in(reg) $call,
            inlateout("eax") $a => out,
            lateout("edx") _,
            lateout("ecx") _,
        };
        out
    }};
    ($call: literal, $a: expr, $b: expr) => {{
        let out;
        std::arch::asm! {
            "call {call}",
            call = in(reg) $call,
            inlateout("eax") $a => out,
            inlateout("edx") $b => _,
            lateout("ecx") _,
        };
        out
    }};
    ($call: literal, $a: expr, $b: expr, $c: expr) => {{
        let out;
        std::arch::asm! {
            "call {call}",
            call = in(reg) $call,
            inlateout("eax") $a => out,
            inlateout("edx") $b => _,
            inlateout("ecx") $c => _,
        };
        out
    }};
    ($call: literal, $a: expr, $b: expr, $c: expr, $d: expr) => {{
        let out;
        std::arch::asm! {
            "push {arg4}",
            "call {call}",
            call = in(reg) $call,
            arg4 = in(reg) $d,
            inlateout("eax") $a => out,
            inlateout("edx") $b => _,
            inlateout("ecx") $c => _,
        };
        out
    }};
}

// this is really game maker specific but i left it here for simplicity
#[repr(C)]
pub struct TreeNodeData {
    unknown: u32,
    pub rtype: u32,   // 0 for toplevel, 1 for group, 2 for folder i think?
    pub kind: u32,    // what resource type i.e. sprite, sound, etc
    pub index: usize, // resource index
}

impl TreeNodeData {
    pub fn new(rtype: u32, kind: u32, index: usize) -> *const TreeNodeData {
        let data = unsafe {
            let data: *mut TreeNodeData = delphi_call!(0x405a4c, 0x71c368, 1);
            &mut *data
        };
        data.rtype = rtype;
        data.kind = kind;
        data.index = index;
        data
    }
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
    pub unsafe fn AddChild(&self, parent: *const TTreeNode, s: &UStr) -> *const TTreeNode {
        delphi_call!(0x4ae1e8, self, parent, s.0)
    }

    pub unsafe fn Clear(&self) {
        let _: u32 = delphi_call!(0x4ae12c, self);
    }
}

#[repr(C)]
pub struct TTreeView {
    padding: [u8; 0x6c],
    pub color: [u8; 3],
    padding2: [u8; 0x269],
    pub nodes: *const TTreeNodes,
}

pub struct TBitmap {}

impl TBitmap {
    pub unsafe fn new() -> *mut Self {
        delphi_call!(0x462144, 0x4587d4, 1)
    }

    pub unsafe fn SaveToFile(&self, filename: &UStr) {
        let _: u32 = delphi_call!(0x45e6d8, self, filename.0);
    }

    pub unsafe fn LoadFromFile(&self, filename: &UStr) {
        let _: u32 = delphi_call!(0x45e64c, self, filename.0);
    }

    pub unsafe fn SetWidth(&mut self, width: u32) {
        asm! {
            "mov ecx, [eax]",
            "call [ecx+0x44]",
            inlateout("eax") self => _,
            inlateout("edx") width => _,
            lateout("ecx") _,
        }
    }

    pub unsafe fn SetHeight(&mut self, height: u32) {
        asm! {
            "mov ecx, [eax]",
            "call [ecx+0x38]",
            inlateout("eax") self => _,
            inlateout("edx") height => _,
            lateout("ecx") _,
        }
    }
}

pub struct TIcon {}

impl TIcon {
    pub unsafe fn SaveToFile(&self, filename: &UStr) {
        let _: u32 = delphi_call!(0x45e6d8, self, filename.0);
    }

    pub unsafe fn LoadFromFile(&mut self, filename: &UStr) {
        let _: u32 = delphi_call!(0x45e64c, self, filename.0);
    }
}

pub struct TMemoryStream {
    // fields are dangerous to use
    vmt: u32,
    memory: *const u8,
    size: usize,
    position: usize,
    capacity: usize,
}

impl TMemoryStream {
    pub unsafe fn new() -> *mut Self {
        delphi_call!(0x405a4c, 0x433630, 1)
    }

    pub unsafe fn get_pos(&self) -> u32 {
        delphi_call!(0x43f234, self)
    }

    pub unsafe fn set_pos(&self, pos: u32) {
        asm! {
            "push 0",
            "push {pos_lo}",
            "call {call}",
            call = in(reg) 0x43f254,
            pos_lo = in(reg) pos,
            inlateout("eax") self => _,
            lateout("edx") _,
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

    pub unsafe fn get_slice(&self) -> &[u8] {
        std::slice::from_raw_parts(self.memory, self.size)
    }

    pub unsafe fn read(&self, buf: *mut u8, count: u32) {
        let _: u32 = delphi_call!(0x43f488, self, buf, count);
    }

    pub unsafe fn load(&self, fname: &UStr) {
        let s: *const u16 = fname.0;
        let _: u32 = delphi_call!(0x43ff44, self, s);
    }
}

// only usable for real TMemoryStreams
impl std::io::Write for TMemoryStream {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        unsafe {
            let result: usize = delphi_call!(0x44006c, self, buf.as_ptr(), buf.len());
            Ok(result)
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[repr(C)]
pub struct THelpForm {
    padding: [u8; 0x388],
    pub editor: *mut TRichEdit,
}

#[repr(C)]
pub struct TRichEdit {
    padding: [u8; 0x6c],
    pub colour: u32,
    padding2: [u8; 0x2c0 - 0x70],
    pub rich_edit_strings: *mut TStrings,
}

pub struct TStrings {}

impl TStrings {
    pub unsafe fn SaveToFile(&self, fname: &UStr) {
        let _: u32 = delphi_call!(0x43e204, self, fname.0);
    }

    pub unsafe fn LoadFromFile(&mut self, fname: &UStr) {
        let _: u32 = delphi_call!(0x43DEC0, self, fname.0);
    }
}

// weird name for an allocator function
pub unsafe fn GetMem<T>(size: usize) -> *mut T {
    delphi_call!(0x40431c, size)
}

pub unsafe fn FreeMem<T>(mem: *const T) {
    let _: u32 = delphi_call!(0x404338, mem);
}

pub unsafe fn UStrAsg(dest: &mut UStr, src: &UStr) {
    let _: u32 = delphi_call!(0x407eb8, dest, src.0);
}

pub unsafe fn UStrFromPCharLen(dest: &mut UStr, source: *const u8, length: usize) {
    let _: u32 = delphi_call!(0x407fe4, dest, source, length);
}

pub unsafe fn UStrFromPWCharLen(dest: &mut UStr, source: *const u16, length: usize) {
    let _: u32 = delphi_call!(0x407ff4, dest, source, length);
}

pub unsafe fn UStrSetLength(dest: &mut UStr, length: usize) {
    let _: u32 = delphi_call!(0x0408244, dest, length);
}

pub unsafe fn UStrAddRef(str: &mut UStr) {
    let _: u32 = delphi_call!(0x407ea0, str.0);
}

pub unsafe fn UStrClr(str: &mut UStr) {
    let _: u32 = delphi_call!(0x407ea8, str);
}

// terrible hack for the second operand i'm sorry
pub unsafe fn CompareText(a: &UStr, b: *const u16) -> i32 {
    delphi_call!(0x415924, a.0, b)
}

#[repr(transparent)]
pub struct UStr(pub *mut u16);

impl UStr {
    pub const EMPTY: Self = Self(ptr::null_mut());

    pub fn new(s: impl AsRef<OsStr>) -> Self {
        let mut out = UStr(std::ptr::null_mut());
        let s = s.as_ref();
        // if it takes more than one WTF-16 u16, it will DEFINITELY take more than one WTF-8 u8
        let guess_len = s.len();
        unsafe {
            UStrSetLength(&mut out, guess_len);
            let mut real_len = 0;
            for (dst, src) in slice::from_raw_parts_mut(out.0, guess_len).iter_mut().zip(s.encode_wide()) {
                *dst = src;
                real_len += 1;
            }
            UStrSetLength(&mut out, real_len);
        }
        out
    }

    pub fn len(&self) -> usize {
        if self.0.is_null() { 0 } else { unsafe { self.0.cast::<usize>().sub(1).read() } }
    }

    pub fn as_slice(&self) -> &[u16] {
        if self.0.is_null() { &[] } else { unsafe { slice::from_raw_parts(self.0, self.len()) } }
    }

    pub fn to_os_string(&self) -> OsString {
        OsString::from_wide(self.as_slice())
    }

    pub const unsafe fn from_ptr(s: &*const u16) -> &Self {
        std::mem::transmute(s)
    }
}

impl Default for UStr {
    fn default() -> Self {
        UStr(ptr::null_mut())
    }
}

impl Clone for UStr {
    fn clone(&self) -> Self {
        let mut new_str = Self(self.0);
        unsafe {
            UStrAddRef(&mut new_str);
        }
        new_str
    }
}

impl Drop for UStr {
    fn drop(&mut self) {
        unsafe { UStrClr(self) }
    }
}

unsafe impl Sync for UStr {}

unsafe impl Send for UStr {}

pub unsafe fn ShowMessage(msg: &UStr) {
    let _: u32 = delphi_call!(0x4d43f8, msg.0);
}

pub fn advance_progress_form(progress: u32) {
    unsafe {
        let _: u32 = delphi_call!(0x6ca2ac, progress);
    }
}

pub fn close_progress_form() {
    unsafe {
        let _: u32 = delphi_call!(0x6ca2cc);
    }
}

pub unsafe fn Free<T>(a: *const T) {
    let _: u32 = delphi_call!(0x405a7c, a);
}

pub unsafe fn DynArrayClear<T, U>(a: *mut T, type_info: *const U) {
    let _: u32 = delphi_call!(0x409ce0, a, type_info);
}

pub fn Random() -> u32 {
    unsafe { delphi_call!(0x4047b0) }
}

pub unsafe fn DynArraySetLength<T>(a: *mut *mut T, type_info: *const u8, dimensions: usize, size: usize) {
    // this has caller clean-up for some reason
    asm! {
        "push {d}",
        "call {call}",
        "add esp,4",
        call = in(reg) 0x409be0,
        d = in(reg) size,
        inlateout("eax") a => _,
        inlateout("edx") type_info => _,
        inlateout("ecx") dimensions => _,
    };
}

pub fn Now(out: &mut f64) {
    unsafe {
        asm! {
            "call {call}",
            "fstp qword ptr [{output}]",
            call = in(reg) 0x4199b0,
            output = in(reg) out,
            out("eax") _,
            out("edx") _,
            out("ecx") _,
        }
    }
}
