use crate::{
    asset::*,
    delphi::{TTreeNode, UStr},
};
use std::slice;

type IntPtr = *const usize;
type AssetList<T> = *const *const *const T;
type UStrListPtr = *const *const UStr;

const TRIGGER_COUNT: IntPtr = 0x77f3f8 as _;
const TRIGGERS: AssetList<Trigger> = 0x77f3f4 as _;

const CONSTANT_COUNT: IntPtr = 0x77f3c4 as _;
const CONSTANT_NAMES: UStrListPtr = 0x78c14c as _;
const CONSTANT_VALUES: UStrListPtr = 0x78c150 as _;

const SOUNDS: AssetList<Sound> = 0x77f2b8 as _;
const SOUND_NAMES: UStrListPtr = 0x77f2c0 as _;
const SOUND_COUNT: IntPtr = 0x77f2c8 as _;

const SPRITE_COUNT: IntPtr = 0x77f4d8 as _;
const SPRITE_NAMES: UStrListPtr = 0x77f4cc as _;
const SPRITES: AssetList<Sprite> = 0x77f4c4 as _;

const BACKGROUNDS: AssetList<Background> = 0x77f1ac as _;
const BACKGROUND_NAMES: UStrListPtr = 0x77f1b4 as _;
const BACKGROUND_COUNT: IntPtr = 0x77f1c0 as _;

const PATHS: AssetList<Path> = 0x77f608 as _;
const PATH_NAMES: UStrListPtr = 0x77f610 as _;
const PATH_COUNT: IntPtr = 0x77f618 as _;

const SCRIPTS: AssetList<Script> = 0x77f2cc as _;
const SCRIPT_NAMES: UStrListPtr = 0x77f2d4 as _;
const SCRIPT_COUNT: IntPtr = 0x77f2dc as _;

const FONT_COUNT: IntPtr = 0x77f50c as _;
const FONT_NAMES: UStrListPtr = 0x77f504 as _;
const FONTS: AssetList<Font> = 0x77f4fc as _;

const TIMELINE_COUNT: IntPtr = 0x77f4f4 as _;
const TIMELINE_NAMES: UStrListPtr = 0x77f4ec as _;
const TIMELINES: AssetList<Timeline> = 0x77f4e4 as _;

const OBJECTS: AssetList<Object> = 0x77f0d0 as _;
const OBJECT_NAMES: UStrListPtr = 0x77f0d8 as _;
const OBJECT_COUNT: IntPtr = 0x77f0e0 as _;

const ROOMS: AssetList<Room> = 0x77f3a8 as _;
const ROOM_NAMES: UStrListPtr = 0x77f3b0 as _;
const ROOM_COUNT: IntPtr = 0x77f3b8 as _;

const INCLUDED_FILES: AssetList<IncludedFile> = 0x77f420 as _;
const INCLUDED_FILE_COUNT: IntPtr = 0x77f428 as _;

const EXTENSIONS: AssetList<Extension> = 0x77f5d4 as _;
const EXTENSION_COUNT: IntPtr = 0x77f5d8 as _;
const EXTENSIONS_LOADED: *const *const bool = 0x790a14 as _;

pub const GAME_ID: IntPtr = 0x7907f4 as IntPtr;

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
    use crate::delphi::UStr;
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
}

macro_rules! read_array {
    ($n:ident, $t:ty, $p:expr, $c:expr) => {
        pub fn $n<'a>() -> &'a [$t] {
            unsafe { slice::from_raw_parts($p.read(), $c.read()) }
        }
    };
}

macro_rules! get_assets {
    ($n:ident, $nn:ident, $t:ty, $p:expr, $np:expr, $c:expr) => {
        read_array!($n, *const $t, $p, $c);
        read_array!($nn, UStr, $np, $c);
    };
}

get_assets!(get_sounds, get_sound_names, Sound, SOUNDS, SOUND_NAMES, SOUND_COUNT);
get_assets!(get_sprites, get_sprite_names, Sprite, SPRITES, SPRITE_NAMES, SPRITE_COUNT);
get_assets!(get_backgrounds, get_background_names, Background, BACKGROUNDS, BACKGROUND_NAMES, BACKGROUND_COUNT);
get_assets!(get_paths, get_path_names, Path, PATHS, PATH_NAMES, PATH_COUNT);
get_assets!(get_scripts, get_script_names, Script, SCRIPTS, SCRIPT_NAMES, SCRIPT_COUNT);
get_assets!(get_fonts, get_font_names, Font, FONTS, FONT_NAMES, FONT_COUNT);
get_assets!(get_timelines, get_timeline_names, Timeline, TIMELINES, TIMELINE_NAMES, TIMELINE_COUNT);
get_assets!(get_objects, get_object_names, Object, OBJECTS, OBJECT_NAMES, OBJECT_COUNT);
get_assets!(get_rooms, get_room_names, Room, ROOMS, ROOM_NAMES, ROOM_COUNT);

read_array!(get_constant_names, UStr, CONSTANT_NAMES, CONSTANT_COUNT);
read_array!(get_constants, UStr, CONSTANT_VALUES, CONSTANT_COUNT);
read_array!(get_triggers, *const Trigger, TRIGGERS, TRIGGER_COUNT);

read_array!(get_included_files, *const IncludedFile, INCLUDED_FILES, INCLUDED_FILE_COUNT);
read_array!(get_extensions, *const Extension, EXTENSIONS, EXTENSION_COUNT);
read_array!(get_extensions_loaded, bool, EXTENSIONS_LOADED, EXTENSION_COUNT);
