#![allow(non_snake_case, dead_code)]

type WStr = *const u16;

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
            "push ebx",
            "mov ebx, {call}",
            "call ebx",
            "pop ebx",
            call = const $call,
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
    pub rtype: u32, // 0 for toplevel, 1 for group, 2 for folder i think?
    pub kind: u32,  // what resource type i.e. sprite, sound, etc
    pub index: u32, // resource index
}

#[repr(C)]
pub struct TTreeNode {
    unknown: u64,
    pub name: WStr,
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
    pub unsafe fn AddChild(&self, parent: *const TTreeNode, s: WStr) {
        let _: u32 = delphi_call!(0x4ae1e8, self, parent, s);
    }

    pub unsafe fn Clear(&self) {
        let _: u32 = delphi_call!(0x4ae12c, self);
    }
}

pub struct TGraphic {}

impl TGraphic {
    pub unsafe fn SaveToFile(&self, filename: WStr) {
        // NOTE: FILENAME MUST BE FULLY FORMED
        let _: u32 = delphi_call!(0x45e6d8, self, filename);
    }

    pub unsafe fn LoadFromFile(&self, filename: WStr) {
        // NOTE: FILENAME MUST BE FULLY FORMED
        let _: u32 = delphi_call!(0x45e64c, self, filename);
    }
}

// weird name for an allocator function
pub unsafe fn GetMem<T>(size: u32) -> *const T {
    delphi_call!(0x40431c, size)
}

pub unsafe fn FreeMem<T>(mem: *const T) {
    let _: u32 = delphi_call!(0x404338, mem);
}

pub unsafe fn UStrFromPCharLen(dest: *const WStr, source: *const u8, length: usize) {
    let _: u32 = delphi_call!(0x407fe4, dest, source, length);
}

pub unsafe fn UStrFromPWCharLen(dest: *const WStr, source: *const u16, length: usize) {
    let _: u32 = delphi_call!(0x407ff4, dest, source, length);
}

pub unsafe fn UStrClr(str: *mut WStr) {
    let _: u32 = delphi_call!(0x407ea8, str);
}

pub unsafe fn ShowMessage(msg: WStr) {
    let _: u32 = delphi_call!(0x4d43f8, msg);
}

pub unsafe fn advance_progress_form(progress: u32) {
    let _: u32 = delphi_call!(0x6ca2ac, progress);
}
