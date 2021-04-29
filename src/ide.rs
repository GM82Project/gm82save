use crate::{
    asset::*,
    delphi::{self, TTreeNode, TTreeView, UStr},
};
use std::slice;

type IntPtr = *mut usize;
type AssetList<T> = *mut *mut Option<&'static T>;
type GaplessList<T> = *mut *mut &'static T;
type UStrListPtr = *mut *mut UStr;
type Forms = *mut *mut Form;
type Timestamps = *mut *mut f64;
type ThumbIDs = *mut *mut i32;
type TypeInfoPtr = *const [u8; 0x2c];

const TRIGGER_COUNT: IntPtr = 0x77f3f8 as _;
const TRIGGERS: AssetList<Trigger> = 0x77f3f4 as _;

const CONSTANT_COUNT: IntPtr = 0x77f3c4 as _;
const CONSTANT_NAMES: UStrListPtr = 0x78c14c as _;
const CONSTANT_VALUES: UStrListPtr = 0x78c150 as _;

const SOUNDS: AssetList<Sound> = 0x77f2b8 as _;
const SOUND_FORMS: Forms = 0x77f2bc as _;
const SOUND_NAMES: UStrListPtr = 0x77f2c0 as _;
const SOUND_TIMESTAMPS: Timestamps = 0x77f2c4 as _;
const SOUND_COUNT: IntPtr = 0x77f2c8 as _;
const SOUND_TYPEINFO: TypeInfoPtr = 0x651ce0 as _;

const SPRITES: AssetList<Sprite> = 0x77f4c4 as _;
const SPRITE_NAMES: UStrListPtr = 0x77f4cc as _;
const SPRITE_TIMESTAMPS: Timestamps = 0x77f4d0 as _;
const SPRITE_THUMBS: ThumbIDs = 0x77f4d4 as _;
const SPRITE_COUNT: IntPtr = 0x77f4d8 as _;
const SPRITE_TYPEINFO: TypeInfoPtr = 0x6f522c as _;

const BACKGROUNDS: AssetList<Background> = 0x77f1ac as _;
const BACKGROUND_FORMS: Forms = 0x77f1b0 as _;
const BACKGROUND_NAMES: UStrListPtr = 0x77f1b4 as _;
const BACKGROUND_TIMESTAMPS: Timestamps = 0x77f1b8 as _;
const BACKGROUND_THUMBS: ThumbIDs = 0x77f1bc as _;
const BACKGROUND_COUNT: IntPtr = 0x77f1c0 as _;
const BACKGROUND_TYPEINFO: TypeInfoPtr = 0x64d734 as _;

const PATHS: AssetList<Path> = 0x77f608 as _;
const PATH_FORMS: Forms = 0x77f60c as _;
const PATH_NAMES: UStrListPtr = 0x77f610 as _;
const PATH_TIMESTAMPS: Timestamps = 0x77f614 as _;
const PATH_COUNT: IntPtr = 0x77f618 as _;
const PATH_TYPEINFO: TypeInfoPtr = 0x72207c as _;

const SCRIPTS: AssetList<Script> = 0x77f2cc as _;
const SCRIPT_FORMS: Forms = 0x77f2d0 as _;
const SCRIPT_NAMES: UStrListPtr = 0x77f2d4 as _;
const SCRIPT_TIMESTAMPS: Timestamps = 0x77f2d8 as _;
const SCRIPT_COUNT: IntPtr = 0x77f2dc as _;
const SCRIPT_TYPEINFO: TypeInfoPtr = 0x6550a8 as _;

const FONTS: AssetList<Font> = 0x77f4fc as _;
const FONT_FORMS: Forms = 0x77f500 as _;
const FONT_NAMES: UStrListPtr = 0x77f504 as _;
const FONT_TIMESTAMPS: Timestamps = 0x77f508 as _;
const FONT_COUNT: IntPtr = 0x77f50c as _;
const FONT_TYPEINFO: TypeInfoPtr = 0x6fc680 as _;

const TIMELINES: AssetList<Timeline> = 0x77f4e4 as _;
const TIMELINE_FORMS: Forms = 0x77f4e8 as _;
const TIMELINE_NAMES: UStrListPtr = 0x77f4ec as _;
const TIMELINE_TIMESTAMPS: Timestamps = 0x77f4f0 as _;
const TIMELINE_COUNT: IntPtr = 0x77f4f4 as _;
const TIMELINE_TYPEINFO: TypeInfoPtr = 0x6fa020 as _;

const OBJECTS: AssetList<Object> = 0x77f0d0 as _;
const OBJECT_FORMS: Forms = 0x77f0d4 as _;
const OBJECT_NAMES: UStrListPtr = 0x77f0d8 as _;
const OBJECT_TIMESTAMPS: Timestamps = 0x77f0dc as _;
const OBJECT_COUNT: IntPtr = 0x77f0e0 as _;
const OBJECT_TYPEINFO: TypeInfoPtr = 0x62c4a8 as _;

const ROOMS: AssetList<Room> = 0x77f3a8 as _;
const ROOM_FORMS: Forms = 0x77f3ac as _;
const ROOM_NAMES: UStrListPtr = 0x77f3b0 as _;
const ROOM_TIMESTAMPS: Timestamps = 0x77f3b4 as _;
const ROOM_COUNT: IntPtr = 0x77f3b8 as _;
const ROOM_TYPEINFO: TypeInfoPtr = 0x6928f8 as _;

const INCLUDED_FILES: GaplessList<IncludedFile> = 0x77f420 as _;
const INCLUDED_FILE_COUNT: IntPtr = 0x77f428 as _;

const EXTENSIONS: GaplessList<Extension> = 0x77f5d4 as _;
const EXTENSION_COUNT: IntPtr = 0x77f5d8 as _;
const EXTENSIONS_LOADED: *mut *mut bool = 0x790a14 as _;

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

pub const _LAST_INSTANCE_ID: IntPtr = 0x77f2e0 as IntPtr;
pub const _LAST_TILE_ID: IntPtr = 0x77f2e4 as IntPtr;

pub mod settings {
    #![allow(dead_code)]
    use crate::delphi::{TGraphic, UStr};

    pub const FULLSCREEN: *const bool = 0x77f514 as _;
    pub const INTERPOLATE_PIXELS: *const bool = 0x77f518 as _;
    pub const DONT_DRAW_BORDER: *const bool = 0x77f51c as _;
    pub const DISPLAY_CURSOR: *const bool = 0x77f520 as _;
    pub const SCALING: *const u32 = 0x77f524 as _;
    pub const ALLOW_RESIZE: *const bool = 0x77f528 as _;
    pub const WINDOW_ON_TOP: *const bool = 0x77f52c as _;
    pub const CLEAR_COLOUR: *const u32 = 0x77f530 as _;
    pub const SET_RESOLUTION: *const bool = 0x77f534 as _;
    pub const COLOUR_DEPTH: *const u32 = 0x77f538 as _;
    pub const RESOLUTION: *const u32 = 0x77f53c as _;
    pub const FREQUENCY: *const u32 = 0x77f540 as _;
    pub const DONT_SHOW_BUTTONS: *const bool = 0x77f544 as _;
    pub const VSYNC_AND_FORCE_CPU: *const u32 = 0x77f54c as _;
    pub const DISABLE_SCREENSAVER: *const bool = 0x77f550 as _;
    pub const F4_FULLSCREEN: *const bool = 0x77f554 as _;
    pub const F1_HELP: *const bool = 0x77f558 as _;
    pub const ESC_CLOSE: *const bool = 0x77f55c as _;
    pub const F5_SAVE_F6_LOAD: *const bool = 0x77f560 as _;
    pub const F9_SCREENSHOT: *const bool = 0x77f564 as _;
    pub const TREAT_CLOSE_AS_ESC: *const bool = 0x77f568 as _;
    pub const PRIORITY: *const u32 = 0x77f56c as _;
    pub const FREEZE_ON_LOSE_FOCUS: *const bool = 0x77f548 as _;
    pub const LOADING_BAR: *const u32 = 0x77f570 as _;
    pub const LOADING_BACKGROUND: *const *const TGraphic = 0x77f580 as _;
    pub const LOADING_FOREGROUND: *const *const TGraphic = 0x77f57c as _;
    pub const HAS_CUSTOM_LOAD_IMAGE: *const bool = 0x77f574 as _;
    pub const CUSTOM_LOAD_IMAGE: *const *const TGraphic = 0x77f578 as _;
    pub const LOADING_TRANSPARENT: *const bool = 0x77f584 as _;
    pub const LOADING_TRANSLUCENCY: *const u32 = 0x77f588 as _;
    pub const LOADING_PROGRESS_BAR_SCALE: *const bool = 0x77f58c as _;
    pub const ICON: *const *const TGraphic = 0x77f590 as _;
    pub const SHOW_ERROR_MESSAGES: *const bool = 0x77f594 as _;
    pub const LOG_ERRORS: *const bool = 0x77f598 as _;
    pub const ALWAYS_ABORT: *const bool = 0x77f59c as _;
    pub const ZERO_UNINITIALIZED_VARS: *const bool = 0x77f5a0 as _;
    pub const ERROR_ON_UNINITIALIZED_ARGS: *const bool = 0x77f5a4 as _;
    pub const INFO_AUTHOR: *const UStr = 0x77f5a8 as _;
    pub const INFO_VERSION: *const UStr = 0x77f5ac as _;
    pub const INFO_INFORMATION: *const UStr = 0x77f5b0 as _;
    pub const VERSION_MAJOR: *const u32 = 0x77f5b4 as _;
    pub const VERSION_MINOR: *const u32 = 0x77f5b8 as _;
    pub const VERSION_RELEASE: *const u32 = 0x77f5bc as _;
    pub const VERSION_BUILD: *const u32 = 0x77f5c0 as _;
    pub const EXE_COMPANY: *const UStr = 0x77f5c4 as _;
    pub const EXE_PRODUCT: *const UStr = 0x77f5c8 as _;
    pub const EXE_COPYRIGHT: *const UStr = 0x77f5cc as _;
    pub const EXE_DESCRIPTION: *const UStr = 0x77f5d0 as _;
}

pub mod game_info {
    use crate::delphi::{THelpForm, UStr};
    pub const NEW_WINDOW: *const bool = 0x77f578 as _;
    pub const CAPTION: *const UStr = 0x77b57c as _;
    pub const LEFT: *const i32 = 0x77b580 as _;
    pub const TOP: *const i32 = 0x77b584 as _;
    pub const WIDTH: *const i32 = 0x77b588 as _;
    pub const HEIGHT: *const i32 = 0x77b58c as _;
    pub const BORDER: *const bool = 0x77b590 as _;
    pub const RESIZABLE: *const bool = 0x77b594 as _;
    pub const WINDOW_ON_TOP: *const bool = 0x77b598 as _;
    pub const FREEZE_GAME: *const bool = 0x77b59c as _;
    pub const FORM: *const *const THelpForm = 0x788958 as _;
}

macro_rules! read_array {
    ($n:ident, $t:ty, $p:expr, $c:expr) => {
        pub fn $n<'a>() -> &'a [$t] {
            unsafe { slice::from_raw_parts(($p).read(), $c.read()) }
        }

        paste::paste! {
            pub fn [<$n _mut>]<'a>() -> &'a mut [$t] {
                unsafe { slice::from_raw_parts_mut(($p).read(), $c.read()) }
            }
        }
    };
}

macro_rules! get_assets {
    ($lo:ident, $t:ty, $assets_p:expr, $count_p:expr, $type_p:expr) => {
        paste::paste! {
            read_array!([<get_ $lo s>], Option<&'static $t>, $assets_p, $count_p);
            read_array!([<get_ $lo _forms>], Form, $assets_p.add(1) as Forms, $count_p);
            read_array!([<get_ $lo _names>], UStr, $assets_p.add(2) as UStrListPtr, $count_p);
            read_array!([<get_ $lo _timestamps>], f64, $assets_p.add(3) as Timestamps, $count_p);
            pub fn [<alloc_ $lo s>]<'a>(count: usize) {
                unsafe {
                    $count_p.write(count);
                    for i in 0..4 {
                        delphi::DynArraySetLength($assets_p.add(i), $type_p.add(i), 1, count);
                    }
                }
            }
        }
    };
}
macro_rules! get_graphics {
    ($lo:ident, $t:ty, $assets_p:expr, $count_p:expr, $type_p:expr) => {
        paste::paste! {
            read_array!([<get_ $lo s>], Option<&'static $t>, $assets_p, $count_p);
            read_array!([<get_ $lo _forms>], Form, $assets_p.add(1) as Forms, $count_p);
            read_array!([<get_ $lo _names>], UStr, $assets_p.add(2) as UStrListPtr, $count_p);
            read_array!([<get_ $lo _timestamps>], f64, $assets_p.add(3) as Timestamps, $count_p);
            read_array!([<get_ $lo _thumbs>], i32, $assets_p.add(4) as ThumbIDs, $count_p);
            pub fn [<alloc_ $lo s>]<'a>(count: usize) {
                unsafe {
                    $count_p.write(count);
                    for i in 0..5 {
                        delphi::DynArraySetLength($assets_p.add(i), $type_p.add(i), 1, count);
                    }
                }
            }
        }
    };
}

get_assets!(sound, Sound, SOUNDS, SOUND_COUNT, SOUND_TYPEINFO);
get_graphics!(sprite, Sprite, SPRITES, SPRITE_COUNT, SPRITE_TYPEINFO);
get_graphics!(background, Background, BACKGROUNDS, BACKGROUND_COUNT, BACKGROUND_TYPEINFO);
get_assets!(path, Path, PATHS, PATH_COUNT, PATH_TYPEINFO);
get_assets!(script, Script, SCRIPTS, SCRIPT_COUNT, SCRIPT_TYPEINFO);
get_assets!(font, Font, FONTS, FONT_COUNT, FONT_TYPEINFO);
get_assets!(timeline, Timeline, TIMELINES, TIMELINE_COUNT, TIMELINE_TYPEINFO);
get_assets!(object, Object, OBJECTS, OBJECT_COUNT, OBJECT_TYPEINFO);
get_assets!(room, Room, ROOMS, ROOM_COUNT, ROOM_TYPEINFO);

read_array!(get_constant_names, UStr, CONSTANT_NAMES, CONSTANT_COUNT);
read_array!(get_constants, UStr, CONSTANT_VALUES, CONSTANT_COUNT);
read_array!(get_triggers, Option<&'static Trigger>, TRIGGERS, TRIGGER_COUNT);

read_array!(get_included_files, &'static IncludedFile, INCLUDED_FILES, INCLUDED_FILE_COUNT);
read_array!(get_extensions, &'static Extension, EXTENSIONS, EXTENSION_COUNT);
read_array!(get_extensions_loaded, bool, EXTENSIONS_LOADED, EXTENSION_COUNT);
