#![allow(non_snake_case, dead_code)]

use std::{
    arch::asm,
    ffi::{OsStr, OsString},
    os::windows::ffi::{OsStrExt, OsStringExt},
    ptr, slice,
};

// this gets optimized out in release mode so it's fine
#[macro_export]
macro_rules! check_call {
    ($call: literal) => {{
        if $call & 3 != 0 {
            crate::show_message(format!("can you let floogle know {:#x} isn't a valid function thanks", $call));
        }
    }};
}

#[macro_export]
macro_rules! delphi_call {
    ($call: literal) => {{
        crate::check_call!($call);
        let out;
        std::arch::asm! {
            "call {call}",
            call = in(reg) $call,
            lateout("eax") out,
            clobber_abi("C"),
        };
        out
    }};
    ($call: literal, $a: expr) => {{
        crate::check_call!($call);
        let out;
        std::arch::asm! {
            "call {call}",
            call = in(reg) $call,
            inlateout("eax") $a => out,
            clobber_abi("C"),
        };
        out
    }};
    ($call: literal, $a: expr, $b: expr) => {{
        crate::check_call!($call);
        let out;
        std::arch::asm! {
            "call {call}",
            call = in(reg) $call,
            inlateout("eax") $a => out,
            in("edx") $b,
            clobber_abi("C"),
        };
        out
    }};
    ($call: literal, $a: expr, $b: expr, $c: expr) => {{
        crate::check_call!($call);
        let out;
        std::arch::asm! {
            "call {call}",
            call = in(reg) $call,
            inlateout("eax") $a => out,
            in("edx") $b,
            in("ecx") $c,
            clobber_abi("C"),
        };
        out
    }};
    ($call: literal, $a: expr, $b: expr, $c: expr, $d: expr) => {{
        crate::check_call!($call);
        let out;
        std::arch::asm! {
            "push {arg4}",
            "call {call}",
            call = in(reg) $call,
            arg4 = in(reg) $d,
            inlateout("eax") $a => out,
            in("edx") $b,
            in("ecx") $c,
            clobber_abi("C"),
        };
        out
    }};
    ($call: literal, $a: expr, $b: expr, $c: expr, $d: expr, $e: expr) => {{
        crate::check_call!($call);
        let out;
        std::arch::asm! {
            "push {arg5}",
            "push {arg4}",
            "call {call}",
            call = in(reg) $call,
            arg4 = in(reg) $d,
            arg5 = in(reg) $e,
            inlateout("eax") $a => out,
            inlateout("edx") $b,
            inlateout("ecx") $c,
            clobber_abi("C"),
        };
        out
    }};
}

#[macro_export]
macro_rules! delphi_box {
    ($call: literal, $vmt: literal) => {{
        DelphiBox::from_ptr(delphi_call!($call, $vmt, 1))
    }};
    ($call: literal, $vmt: literal, $($x:expr),*) => {{
        DelphiBox::from_ptr(delphi_call!($call, $vmt, 1, $($x),*))
    }};
}

#[repr(transparent)]
pub struct DelphiBox<T>(ptr::NonNull<T>);

unsafe impl<T> Send for DelphiBox<T> {}
unsafe impl<T> Sync for DelphiBox<T> {}

impl<T> DelphiBox<T> {
    pub fn from_ptr(ptr: *mut T) -> Self {
        unsafe { Self(ptr::NonNull::new_unchecked(ptr)) }
    }

    pub fn as_ptr(&self) -> *const T {
        self.0.as_ptr()
    }
}

impl<T> Drop for DelphiBox<T> {
    fn drop(&mut self) {
        unsafe {
            let _: u32 = delphi_call!(0x405a7c, self.0.as_ref());
        }
    }
}

impl<T> std::ops::Deref for DelphiBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<T> std::ops::DerefMut for DelphiBox<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

impl<T: std::io::Write> std::io::Write for DelphiBox<T> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        (**self).write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        (**self).flush()
    }
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

    pub unsafe fn new(name: &UStr, rtype: u32, kind: u32, index: usize) -> *const TTreeNode {
        delphi_call!(0x71cb48, name.0, rtype, kind, index)
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

#[repr(C)]
pub struct TMenuItem {
    padding: [u8; 12],
    pub Tag: i32,
    padding2: [u8; 136 - 12],
    pub OnClick: usize,
}

impl TMenuItem {
    pub fn new(parent: usize) -> DelphiBox<Self> {
        unsafe { delphi_box!(0x4d89cc, 0x4d5e80, parent) }
    }

    pub fn set_caption(&mut self, caption: &UStr) {
        unsafe {
            let _: u32 = delphi_call!(0x4dbc6c, self, caption.0);
        }
    }

    pub fn set_image_index(&mut self, image_index: i32) {
        unsafe {
            let _: u32 = delphi_call!(0x4dbe60, self, image_index);
        }
    }

    pub fn add(&mut self, added: DelphiBox<Self>) {
        unsafe {
            let added: &Self = &added;
            let _: u32 = delphi_call!(0x4dc190, self, added);
        }
        std::mem::forget(added);
    }

    pub fn add_from_tree_node(&self, tree_node: &TTreeNode, custom_events: Option<&[u32; 6]>) {
        const BLANK_EVENTS: [u32; 6] = [0; 6];
        unsafe {
            asm! {
                "push dword ptr [{base} + 0x1c]",
                "push dword ptr [{base} + 0x18]",
                "push dword ptr [{base} + 0x14]",
                "push dword ptr [{base} + 0x10]",
                "push dword ptr [{base} + 0xc]",
                "push dword ptr [{base} + 0x8]",
                "call {func}",
                base = in(reg) custom_events.unwrap_or(&BLANK_EVENTS),
                func = in(reg) 0x71c3fc,
                in("eax") self,
                in("edx") tree_node,
                in("ecx") u32::from(custom_events.is_some()),
                clobber_abi("C"),
            }
        }
    }

    pub fn add_with_fake_tree_node(
        &self,
        name: &UStr,
        rtype: u32,
        kind: u32,
        index: usize,
        custom_events: Option<&[u32; 6]>,
    ) {
        // very bad and evil
        let data = TreeNodeData { unknown: 0, rtype, kind, index };
        let node = TTreeNode { unknown: 0x49614c, name: name.clone(), data: &data };
        self.add_from_tree_node(&node, custom_events);
    }
}

#[repr(C)]
pub struct TPopupMenu {
    padding: [u8; 0x38],
    pub Items: &'static mut TMenuItem,
}

impl TPopupMenu {
    pub fn new(parent: usize) -> DelphiBox<Self> {
        unsafe { delphi_box!(0x4dee2c, 0x4d7824, parent) }
    }

    pub fn SetAutoHotkeys(&mut self, hotkeys: u8) {
        unsafe {
            let _: u32 = delphi_call!(0x4de514, self, u32::from(hotkeys));
        }
    }

    pub fn SetImages(&mut self) {
        unsafe {
            let _: u32 = delphi_call!(0x4de0a0, self, *(0x789b38 as *const usize));
        }
    }

    pub fn popup_at_cursor_pos(&self) {
        let mouse_pos: [i32; 2] = [0, 0];
        unsafe {
            // TMouse.GetCursorPos
            let _: u32 = delphi_call!(0x4fd580, 0, mouse_pos.as_ptr());
            // TPopupMenu.PopUp
            let _: u32 = delphi_call!(0x4def94, self, mouse_pos[0], mouse_pos[1]);
            // TApplication.ProcessMessages
            let _: u32 = delphi_call!(0x51f71c, *(0x7882ec as *const usize));
        }
    }
}

#[repr(C)]
pub struct TBitmap {}

impl TBitmap {
    pub unsafe fn new() -> DelphiBox<Self> {
        delphi_box!(0x462144, 0x4587d4)
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
            in("eax") self,
            in("edx") width,
            clobber_abi("C"),
        }
    }

    pub unsafe fn SetHeight(&mut self, height: u32) {
        asm! {
            "mov ecx, [eax]",
            "call [ecx+0x38]",
            in("eax") self,
            in("edx") height,
            clobber_abi("C"),
        }
    }

    pub unsafe fn SetSize(&mut self, width: u32, height: u32) {
        asm! {
            "call {}",
            in(reg) (self as *const Self).cast::<*const u32>().read().add(0x6c / 4).read(),
            in("eax") self,
            in("edx") width,
            in("ecx") height,
        }
    }

    pub unsafe fn SetPixelFormat(&mut self, format: u8) {
        let _: u32 = delphi_call!(0x463dc0, self, u32::from(format));
    }

    pub unsafe fn GetCanvas(&self) -> &mut TCanvas {
        let ptr: *mut TCanvas = delphi_call!(0x462a64, self);
        &mut *ptr
    }

    pub unsafe fn GetScanline(&self, row: u32) -> *const u8 {
        delphi_call!(0x462be8, self, row)
    }
}

#[repr(C)]
pub struct TCanvas {
    _unused: [u8; 0x40],
    pub FFont: &'static mut TFont,
    _unused2: u32,
    pub FBrush: &'static mut TBrush,
}

impl TCanvas {
    pub unsafe fn GetHandle(&self) -> usize {
        delphi_call!(0x45ce2c, self)
    }

    pub unsafe fn TextWidth(&self, text: &UStr) -> u32 {
        delphi_call!(0x45bec4, self, text.0)
    }

    pub unsafe fn TextHeight(&self, text: &UStr) -> u32 {
        delphi_call!(0x45bea4, self, text.0)
    }

    pub unsafe fn GetClipRect(&self, rect: &mut [u32; 4]) {
        let _: u32 = delphi_call!(0x45ce04, self, rect.as_mut_ptr());
    }

    pub unsafe fn FillRect(&self, rect: &[u32; 4]) {
        let _: u32 = delphi_call!(0x45c664, self, rect.as_ptr());
    }

    pub unsafe fn TextOut(&self, x: i32, y: u32, text: &UStr) {
        let _: u32 = delphi_call!(0x45c9e8, self, x, y, text.0);
    }
}

#[repr(C)]
pub struct TFont {}

impl TFont {
    pub fn Assign(&mut self, other: &Self) {
        unsafe {
            let _: u32 = delphi_call!(0x45af94, self, other);
        }
    }

    pub unsafe fn SetColor(&self, col: u32) {
        let _: u32 = delphi_call!(0x45b0d0, self, col);
    }
}

#[repr(C)]
pub struct TBrush {}

impl TBrush {
    pub unsafe fn SetColor(&self, color: u32) {
        let _: u32 = delphi_call!(0x45bc24, self, color);
    }

    pub unsafe fn SetStyle(&self, style: u8) {
        let _: u32 = delphi_call!(0x45bda4, self, u32::from(style));
    }
}

#[repr(C)]
pub struct TIcon {}

impl TIcon {
    pub unsafe fn SaveToFile(&self, filename: &UStr) {
        let _: u32 = delphi_call!(0x45e6d8, self, filename.0);
    }

    pub unsafe fn LoadFromFile(&mut self, filename: &UStr) {
        let _: u32 = delphi_call!(0x45e64c, self, filename.0);
    }
}

#[repr(C)]
pub struct TMemoryStream {
    // fields are dangerous to use
    vmt: u32,
    memory: *const u8,
    size: usize,
    position: usize,
    capacity: usize,
}

impl TMemoryStream {
    pub unsafe fn new() -> DelphiBox<Self> {
        delphi_box!(0x405a4c, 0x433630)
    }

    pub fn get_pos(&self) -> u32 {
        unsafe { delphi_call!(0x43f234, self) }
    }

    pub fn set_pos(&self, pos: u32) {
        unsafe {
            asm! {
                "push 0",
                "push {pos_lo}",
                "call {call}",
                call = in(reg) 0x43f254,
                pos_lo = in(reg) pos,
                in("eax") self,
                clobber_abi("C"),
            };
        }
    }

    pub fn get_size(&self) -> u32 {
        let out;
        unsafe {
            asm! {
                "mov ecx,[eax]",
                "call [ecx]",
                inlateout("eax") self => out,
                clobber_abi("C"),
            };
        }
        out
    }

    pub fn get_slice(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.memory, self.size) }
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

pub const DPI: *mut u32 = 0x78810c as _;

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
        let mut out = UStr(ptr::null_mut());
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

    pub fn from_char(c: u16) -> Self {
        let mut out = UStr(ptr::null_mut());
        let _: u32 = unsafe { delphi_call!(0x408034, &mut out, u32::from(c)) };
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

    pub fn push_ustr(&mut self, other: &Self) {
        unsafe {
            let _: u32 = delphi_call!(0x4082dc, self, other.0);
        }
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
        in("eax") a,
        in("edx") type_info,
        in("ecx") dimensions,
        clobber_abi("C"),
    };
}

pub fn Now(out: &mut f64) {
    unsafe {
        asm! {
            "call {call}",
            "fstp qword ptr [{output}]",
            call = in(reg) 0x4199b0,
            output = in(reg) out,
            // manually exclude eax, edx, ecx from register selection
            out("eax") _,
            out("edx") _,
            out("ecx") _,
            clobber_abi("C"),
        }
    }
}
