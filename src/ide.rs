#![allow(dead_code)]
use crate::{
    asset::*,
    delphi::{DelphiBox, TTreeNode, TTreeView, UStr},
    list::DelphiList,
};
use std::slice;

type IntPtr = *mut usize;
type GaplessList<T, const P: usize> = *mut DelphiList<DelphiBox<T>, P>;
type TypeInfoPtr = usize;

macro_rules! get_member {
    (pub $n:ident, $t:ty, $p:ident) => {
        pub fn $n(&self) -> &[$t] {
            unsafe { (*self.0).$p.get_unchecked(self.range()) }
        }
    };
    ($n:ident, $t:ty, $p:ident) => {
        fn $n(&self) -> &[$t] {
            unsafe { (*self.0).$p.get_unchecked(self.range()) }
        }
    };
}

macro_rules! get_member_mut {
    (pub $n:ident, $t:ty, $p:ident) => {
        pub fn $n(&self) -> &'static mut [$t] {
            unsafe { (*self.0).$p.get_unchecked_mut(self.range()) }
        }
    };
    ($n:ident, $t:ty, $p:ident) => {
        fn $n(&self) -> &'static mut [$t] {
            unsafe { (*self.0).$p.get_unchecked_mut(self.range()) }
        }
    };
}

pub trait AssetListTrait<T>: Sync {
    fn assets(&self) -> &[Option<DelphiBox<T>>];
    fn assets_mut(&self) -> &'static mut [Option<DelphiBox<T>>];
    fn names(&self) -> &[UStr];
    fn names_mut(&self) -> &'static mut [UStr];
    fn timestamps(&self) -> &[f64];
    fn alloc(&self, count: usize);
}

#[repr(C)]
struct AssetListInner<T: 'static, const P1: usize, const P2: usize, const P3: usize, const P4: usize> {
    pub assets: DelphiList<Option<DelphiBox<T>>, P1>,
    pub forms: DelphiList<Form, P2>,
    pub names: DelphiList<UStr, P3>,
    pub timestamps: DelphiList<f64, P4>,
    pub count: usize,
}

#[repr(C)]
struct GraphicAssetListInner<
    T: 'static,
    const P1: usize,
    const P2: usize,
    const P3: usize,
    const P4: usize,
    const P5: usize,
> {
    pub assets: DelphiList<Option<DelphiBox<T>>, P1>,
    pub forms: DelphiList<Form, P2>,
    pub names: DelphiList<UStr, P3>,
    pub timestamps: DelphiList<f64, P4>,
    pub thumbs: DelphiList<i32, P5>,
    pub count: usize,
}

pub struct AssetList<T: 'static, const P1: usize, const P2: usize, const P3: usize, const P4: usize>(
    *mut AssetListInner<T, P1, P2, P3, P4>,
);

pub struct GraphicAssetList<
    T: 'static,
    const P1: usize,
    const P2: usize,
    const P3: usize,
    const P4: usize,
    const P5: usize,
>(*mut GraphicAssetListInner<T, P1, P2, P3, P4, P5>);

unsafe impl<T: 'static, const P1: usize, const P2: usize, const P3: usize, const P4: usize> Sync
    for AssetList<T, P1, P2, P3, P4>
{
}

unsafe impl<T, const P1: usize, const P2: usize, const P3: usize, const P4: usize, const P5: usize> Sync
    for GraphicAssetList<T, P1, P2, P3, P4, P5>
{
}

impl<T, const P1: usize, const P2: usize, const P3: usize, const P4: usize> AssetList<T, P1, P2, P3, P4> {
    get_member!(pub forms, Form, forms);

    get_member_mut!(pub forms_mut, Form, forms);

    get_member_mut!(pub timestamps_mut, f64, timestamps);

    unsafe fn range(&self) -> std::ops::RangeTo<usize> {
        ..(*self.0).count
    }
}

impl<T, const P1: usize, const P2: usize, const P3: usize, const P4: usize, const P5: usize>
    GraphicAssetList<T, P1, P2, P3, P4, P5>
{
    get_member!(pub forms, Form, forms);

    get_member!(pub forms_mut, Form, forms);

    get_member_mut!(pub timestamps_mut, f64, timestamps);

    get_member_mut!(pub thumbs_mut, i32, thumbs);

    unsafe fn range(&self) -> std::ops::RangeTo<usize> {
        ..(*self.0).count
    }
}

impl<T, const P1: usize, const P2: usize, const P3: usize, const P4: usize> AssetListTrait<T>
    for AssetList<T, P1, P2, P3, P4>
{
    get_member!(assets, Option<DelphiBox<T>>, assets);

    get_member_mut!(assets_mut, Option<DelphiBox<T>>, assets);

    get_member!(names, UStr, names);

    get_member_mut!(names_mut, UStr, names);

    get_member!(timestamps, f64, timestamps);

    fn alloc(&self, count: usize) {
        unsafe {
            (*self.0).count = count;
            (*self.0).assets.alloc(count);
            (*self.0).forms.alloc(count);
            (*self.0).names.alloc(count);
            (*self.0).timestamps.alloc(count);
        }
    }
}

impl<T, const P1: usize, const P2: usize, const P3: usize, const P4: usize, const P5: usize> AssetListTrait<T>
    for GraphicAssetList<T, P1, P2, P3, P4, P5>
{
    get_member!(assets, Option<DelphiBox<T>>, assets);

    get_member_mut!(assets_mut, Option<DelphiBox<T>>, assets);

    get_member!(names, UStr, names);

    get_member_mut!(names_mut, UStr, names);

    get_member!(timestamps, f64, timestamps);

    fn alloc(&self, count: usize) {
        unsafe {
            (*self.0).count = count;
            (*self.0).assets.alloc(count);
            (*self.0).forms.alloc(count);
            (*self.0).names.alloc(count);
            (*self.0).timestamps.alloc(count);
            (*self.0).thumbs.alloc(count);
        }
    }
}

const TRIGGERS: *mut DelphiList<Option<DelphiBox<Trigger>>, 0x6bc93c> = 0x77f3f4 as _;
const TRIGGER_COUNT: IntPtr = 0x77f3f8 as _;
pub const TRIGGERS_UPDATED: *const bool = 0x790058 as _;

const CONSTANT_COUNT: IntPtr = 0x77f3c4 as _;
const CONSTANT_NAMES: *mut DelphiList<UStr, 0x696594> = 0x78c14c as _;
const CONSTANT_VALUES: *mut DelphiList<UStr, 0x6965c0> = 0x78c150 as _;
const CONSTANT_NAME_TYPE: TypeInfoPtr = 0x696594 as _;
const CONSTANT_VALUE_TYPE: TypeInfoPtr = 0x6965c0 as _;
pub const CONSTANTS_UPDATED: *mut bool = 0x78c154 as _;

pub const SOUNDS: AssetList<
    Sound,
    SOUND_TYPEINFO,
    { SOUND_TYPEINFO + SOUND_TYPESIZE },
    { SOUND_TYPEINFO + SOUND_TYPESIZE * 2 },
    { SOUND_TYPEINFO + SOUND_TYPESIZE * 3 },
> = AssetList(0x77f2b8 as _);
const SOUND_TYPEINFO: TypeInfoPtr = 0x651ce0 as _;
const SOUND_TYPESIZE: usize = 0x2c;
pub const SOUNDS_UPDATED: *mut bool = 0x78a1b0 as _;

pub const SPRITES: GraphicAssetList<
    Sprite,
    SPRITE_TYPEINFO,
    { SPRITE_TYPEINFO + SPRITE_TYPESIZE },
    { SPRITE_TYPEINFO + SPRITE_TYPESIZE * 2 },
    { SPRITE_TYPEINFO + SPRITE_TYPESIZE * 3 },
    { SPRITE_TYPEINFO + SPRITE_TYPESIZE * 4 },
> = GraphicAssetList(0x77f4c4 as _);
const SPRITE_TYPEINFO: TypeInfoPtr = 0x6f522c as _;
const SPRITE_TYPESIZE: usize = 0x2c;
pub const SPRITES_UPDATED: *mut bool = 0x790170 as _;

pub const BACKGROUNDS: GraphicAssetList<
    Background,
    BACKGROUND_TYPEINFO,
    { BACKGROUND_TYPEINFO + BACKGROUND_TYPESIZE },
    { BACKGROUND_TYPEINFO + BACKGROUND_TYPESIZE * 2 },
    { BACKGROUND_TYPEINFO + BACKGROUND_TYPESIZE * 3 },
    { BACKGROUND_TYPEINFO + BACKGROUND_TYPESIZE * 4 },
> = GraphicAssetList(0x77f1ac as _);
const BACKGROUND_TYPEINFO: TypeInfoPtr = 0x64d734 as _;
const BACKGROUND_TYPESIZE: usize = 0x30;
pub const BACKGROUNDS_UPDATED: *mut bool = 0x78a168 as _;

pub const PATHS: AssetList<
    Path,
    PATH_TYPEINFO,
    { PATH_TYPEINFO + PATH_TYPESIZE },
    { PATH_TYPEINFO + PATH_TYPESIZE * 2 },
    { PATH_TYPEINFO + PATH_TYPESIZE * 3 },
> = AssetList(0x77f608 as _);
const PATH_TYPEINFO: TypeInfoPtr = 0x72207c as _;
const PATH_TYPESIZE: usize = 0x28;
pub const PATHS_UPDATED: *mut bool = 0x7a4658 as _;

pub const SCRIPTS: AssetList<
    Script,
    SCRIPT_TYPEINFO,
    { SCRIPT_TYPEINFO + SCRIPT_TYPESIZE },
    { SCRIPT_TYPEINFO + SCRIPT_TYPESIZE * 2 },
    { SCRIPT_TYPEINFO + SCRIPT_TYPESIZE * 3 },
> = AssetList(0x77f2cc as _);
const SCRIPT_TYPEINFO: TypeInfoPtr = 0x6550a8 as _;
const SCRIPT_TYPESIZE: usize = 0x2c;
pub const SCRIPTS_UPDATED: *mut bool = 0x78a1b8 as _;

pub const FONTS: AssetList<
    Font,
    FONT_TYPEINFO,
    { FONT_TYPEINFO + FONT_TYPESIZE },
    { FONT_TYPEINFO + FONT_TYPESIZE * 2 },
    { FONT_TYPEINFO + FONT_TYPESIZE * 3 },
> = AssetList(0x77f4fc as _);
const FONT_TYPEINFO: TypeInfoPtr = 0x6fc680 as _;
const FONT_TYPESIZE: usize = 0x28;
pub const FONTS_UPDATED: *mut bool = 0x790190 as _;

pub const TIMELINES: AssetList<
    Timeline,
    TIMELINE_TYPEINFO,
    { TIMELINE_TYPEINFO + TIMELINE_TYPESIZE },
    { TIMELINE_TYPEINFO + TIMELINE_TYPESIZE * 2 },
    { TIMELINE_TYPEINFO + TIMELINE_TYPESIZE * 3 },
> = AssetList(0x77f4e4 as _);
const TIMELINE_TYPEINFO: TypeInfoPtr = 0x6fa020 as _;
const TIMELINE_TYPESIZE: usize = 0x2c;
pub const TIMELINES_UPDATED: *mut bool = 0x790188 as _;

pub const OBJECTS: AssetList<
    Object,
    OBJECT_TYPEINFO,
    { OBJECT_TYPEINFO + OBJECT_TYPESIZE },
    { OBJECT_TYPEINFO + OBJECT_TYPESIZE * 2 },
    { OBJECT_TYPEINFO + OBJECT_TYPESIZE * 3 },
> = AssetList(0x77f0d0 as _);
const OBJECT_TYPEINFO: TypeInfoPtr = 0x62c4a8 as _;
const OBJECT_TYPESIZE: usize = 0x2c;
pub const OBJECTS_UPDATED: *mut bool = 0x78a0d4 as _;

pub const ROOMS: AssetList<
    Room,
    ROOM_TYPEINFO,
    { ROOM_TYPEINFO + ROOM_TYPESIZE },
    { ROOM_TYPEINFO + ROOM_TYPESIZE * 2 },
    { ROOM_TYPEINFO + ROOM_TYPESIZE * 3 },
> = AssetList(0x77f3a8 as _);
const ROOM_TYPEINFO: TypeInfoPtr = 0x6928f8 as _;
const ROOM_TYPESIZE: usize = 0x28;
pub const ROOMS_UPDATED: *mut bool = 0x78a1f8 as _;

const INCLUDED_FILES: GaplessList<IncludedFile, 0x6cc47c> = 0x77f420 as _;
const INCLUDED_FILE_TIMESTAMPS: *mut DelphiList<f64, 0x6cc4a8> = 0x77f424 as _;
const INCLUDED_FILE_COUNT: IntPtr = 0x77f428 as _;
pub const INCLUDED_FILES_UPDATED: *mut bool = 0x7900a0 as _;

const EXTENSIONS: GaplessList<Extension, 0x712788> = 0x77f5d4 as _;
const EXTENSION_COUNT: IntPtr = 0x77f5d8 as _;
const EXTENSIONS_LOADED: *mut DelphiList<bool, 0x7127b8> = 0x790a14 as _;
pub const EXTENSIONS_UPDATED: *mut bool = 0x790a0c as _;

pub const GAME_ID: IntPtr = 0x7907f4 as IntPtr;

pub const RESOURCE_TREE: *const *mut TTreeView = 0x79a9e8 as _;
pub const RESOURCE_TREE_HIDDEN: *const *mut TTreeView = 0x79a9ec as _;
pub const RT_OBJECTS: *const *const TTreeNode = 0x79a9b8 as _;
pub const RT_SPRITES: *const *const TTreeNode = 0x79a9bc as _;
pub const RT_SOUNDS: *const *const TTreeNode = 0x79a9c0 as _;
pub const RT_ROOMS: *const *const TTreeNode = 0x79a9c4 as _;
pub const RT_BACKGROUNDS: *const *const TTreeNode = 0x79a9c8 as _;
pub const RT_PATHS: *const *const TTreeNode = 0x79a9cc as _;
pub const RT_SCRIPTS: *const *const TTreeNode = 0x79a9d0 as _;
pub const RT_FONTS: *const *const TTreeNode = 0x79a9d4 as _;
pub const RT_TIMELINES: *const *const TTreeNode = 0x79a9d8 as _;
pub const _RT_GAME_INFO: *const *const TTreeNode = 0x79a9dc as _;
pub const _RT_GLOBAL_GAME_SETTINGS: *const *const TTreeNode = 0x79a9e0 as _;
pub const _RT_EXTENSION_PACKAGES: *const *const TTreeNode = 0x79a9e4 as _;
pub const RESOURCE_TREE_UPDATED: *const bool = 0x77f5f4 as _;

pub const LAST_INSTANCE_ID: IntPtr = 0x77f2e0 as IntPtr;
pub const LAST_TILE_ID: IntPtr = 0x77f2e4 as IntPtr;

pub const SETTINGS_UPDATED: *mut bool = 0x790824 as _;
pub mod settings {
    #![allow(dead_code)]
    use crate::delphi::{DelphiBox, TBitmap, TIcon, UStr};

    pub const FULLSCREEN: *mut bool = 0x77f514 as _;
    pub const INTERPOLATE_PIXELS: *mut bool = 0x77f518 as _;
    pub const DONT_DRAW_BORDER: *mut bool = 0x77f51c as _;
    pub const DISPLAY_CURSOR: *mut bool = 0x77f520 as _;
    pub const SCALING: *mut i32 = 0x77f524 as _;
    pub const ALLOW_RESIZE: *mut bool = 0x77f528 as _;
    pub const WINDOW_ON_TOP: *mut bool = 0x77f52c as _;
    pub const CLEAR_COLOUR: *mut u32 = 0x77f530 as _;
    pub const SET_RESOLUTION: *mut bool = 0x77f534 as _;
    pub const COLOUR_DEPTH: *mut u32 = 0x77f538 as _;
    pub const RESOLUTION: *mut u32 = 0x77f53c as _;
    pub const FREQUENCY: *mut u32 = 0x77f540 as _;
    pub const DONT_SHOW_BUTTONS: *mut bool = 0x77f544 as _;
    pub const VSYNC_AND_FORCE_CPU: *mut u32 = 0x77f54c as _;
    pub const DISABLE_SCREENSAVER: *mut bool = 0x77f550 as _;
    pub const F4_FULLSCREEN: *mut bool = 0x77f554 as _;
    pub const F1_HELP: *mut bool = 0x77f558 as _;
    pub const ESC_CLOSE: *mut bool = 0x77f55c as _;
    pub const F5_SAVE_F6_LOAD: *mut bool = 0x77f560 as _;
    pub const F9_SCREENSHOT: *mut bool = 0x77f564 as _;
    pub const TREAT_CLOSE_AS_ESC: *mut bool = 0x77f568 as _;
    pub const PRIORITY: *mut u32 = 0x77f56c as _;
    pub const FREEZE_ON_LOSE_FOCUS: *mut bool = 0x77f548 as _;
    pub const LOADING_BAR: *mut u32 = 0x77f570 as _;
    pub const LOADING_BACKGROUND: *mut Option<DelphiBox<TBitmap>> = 0x77f580 as _;
    pub const LOADING_FOREGROUND: *mut Option<DelphiBox<TBitmap>> = 0x77f57c as _;
    pub const HAS_CUSTOM_LOAD_IMAGE: *mut bool = 0x77f574 as _;
    pub const CUSTOM_LOAD_IMAGE: *mut Option<DelphiBox<TBitmap>> = 0x77f578 as _;
    pub const LOADING_TRANSPARENT: *mut bool = 0x77f584 as _;
    pub const LOADING_TRANSLUCENCY: *mut u32 = 0x77f588 as _;
    pub const LOADING_PROGRESS_BAR_SCALE: *mut bool = 0x77f58c as _;
    pub const ICON: *mut DelphiBox<TIcon> = 0x77f590 as _;
    pub const SHOW_ERROR_MESSAGES: *mut bool = 0x77f594 as _;
    pub const LOG_ERRORS: *mut bool = 0x77f598 as _;
    pub const ALWAYS_ABORT: *mut bool = 0x77f59c as _;
    pub const ZERO_UNINITIALIZED_VARS: *mut bool = 0x77f5a0 as _;
    pub const ERROR_ON_UNINITIALIZED_ARGS: *mut bool = 0x77f5a4 as _;
    pub const INFO_AUTHOR: *mut UStr = 0x77f5a8 as _;
    pub const INFO_VERSION: *mut UStr = 0x77f5ac as _;
    pub const INFO_TIMESTAMP: *mut f64 = 0x79081c as _;
    pub const INFO_INFORMATION: *mut UStr = 0x77f5b0 as _;
    pub const VERSION_MAJOR: *mut u32 = 0x77f5b4 as _;
    pub const VERSION_MINOR: *mut u32 = 0x77f5b8 as _;
    pub const VERSION_RELEASE: *mut u32 = 0x77f5bc as _;
    pub const VERSION_BUILD: *mut u32 = 0x77f5c0 as _;
    pub const EXE_COMPANY: *mut UStr = 0x77f5c4 as _;
    pub const EXE_PRODUCT: *mut UStr = 0x77f5c8 as _;
    pub const EXE_COPYRIGHT: *mut UStr = 0x77f5cc as _;
    pub const EXE_DESCRIPTION: *mut UStr = 0x77f5d0 as _;
}

pub const GAME_INFO_UPDATED: *mut bool = 0x78895c as _;
pub mod game_info {
    use crate::delphi::{THelpForm, UStr};
    pub const NEW_WINDOW: *mut bool = 0x77b578 as _;
    pub const CAPTION: *mut UStr = 0x77b57c as _;
    pub const LEFT: *mut i32 = 0x77b580 as _;
    pub const TOP: *mut i32 = 0x77b584 as _;
    pub const WIDTH: *mut i32 = 0x77b588 as _;
    pub const HEIGHT: *mut i32 = 0x77b58c as _;
    pub const BORDER: *mut bool = 0x77b590 as _;
    pub const RESIZABLE: *mut bool = 0x77b594 as _;
    pub const WINDOW_ON_TOP: *mut bool = 0x77b598 as _;
    pub const FREEZE_GAME: *mut bool = 0x77b59c as _;
    pub const FORM: *mut *const THelpForm = 0x788958 as _;
}

const ACTION_LIBRARIES: *const &'static ActionLibrary = 0x79a660 as _;
const ACTION_LIBRARY_COUNT: IntPtr = 0x77f5dc as _;

pub const PROJECT_PATH: *mut UStr = 0x77f44c as _;

pub fn initialize_project() {
    unsafe {
        let _: u32 = delphi_call!(0x705964);
    }
}

macro_rules! read_array {
    ($n:ident, $nm:ident, $t:ty, $p:expr, $c:expr) => {
        pub fn $n<'a>() -> &'a [$t] {
            unsafe { $p.get_unchecked(..$c) }
        }

        pub fn $nm<'a>() -> &'a mut [$t] {
            unsafe { $p.get_unchecked_mut(..$c) }
        }
    };
}

read_array!(get_constant_names, get_constant_names_mut, UStr, *CONSTANT_NAMES, *CONSTANT_COUNT);
read_array!(get_constants, get_constants_mut, UStr, *CONSTANT_VALUES, *CONSTANT_COUNT);
read_array!(get_triggers, get_triggers_mut, Option<DelphiBox<Trigger>>, *TRIGGERS, *TRIGGER_COUNT);

read_array!(get_included_files, get_included_files_mut, DelphiBox<IncludedFile>, *INCLUDED_FILES, *INCLUDED_FILE_COUNT);
read_array!(
    get_included_file_timestamps,
    get_included_file_timestamps_mut,
    f64,
    *INCLUDED_FILE_TIMESTAMPS,
    *INCLUDED_FILE_COUNT
);
read_array!(get_extensions, get_extensions_mut, DelphiBox<Extension>, *EXTENSIONS, *EXTENSION_COUNT);
read_array!(get_extensions_loaded, get_extensions_loaded_mut, bool, *EXTENSIONS_LOADED, *EXTENSION_COUNT);

pub fn get_action_libraries<'a>() -> &'a [&'a ActionLibrary] {
    unsafe { slice::from_raw_parts(ACTION_LIBRARIES, ACTION_LIBRARY_COUNT.read()) }
}

pub fn alloc_constants(count: usize) {
    unsafe {
        CONSTANT_COUNT.write(count);
        (*CONSTANT_NAMES).alloc(count);
        (*CONSTANT_VALUES).alloc(count);
    }
}

pub fn alloc_triggers(count: usize) {
    unsafe {
        TRIGGER_COUNT.write(count);
        (*TRIGGERS).alloc(count);
    }
}

pub fn alloc_included_files(count: usize) {
    unsafe {
        INCLUDED_FILE_COUNT.write(count);
        (*INCLUDED_FILES).alloc_fill(count, IncludedFile::new);
        (*INCLUDED_FILE_TIMESTAMPS).alloc(count);
    }
}
