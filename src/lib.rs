#![feature(asm)]

mod asset;
mod delphi;
mod io_queue;

use crate::{
    asset::*,
    delphi::{advance_progress_form, TTreeNode},
    io_queue::IOController,
};
use ctor::ctor;
use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
    ffi::{OsStr, OsString},
    fmt::Formatter,
    io::Write,
    os::windows::ffi::{OsStrExt, OsStringExt},
    path::PathBuf,
    slice, str,
};
use winapi::{
    ctypes::wchar_t,
    um::{
        memoryapi::VirtualProtect,
        processthreadsapi::{FlushInstructionCache, GetCurrentProcess},
        winnt::PAGE_READWRITE,
    },
};

fn decode_pas_str(str: *const u16) -> OsString {
    if str.is_null() {
        OsString::new()
    } else {
        unsafe {
            let len = str.cast::<u32>().sub(1).read();
            OsString::from_wide(slice::from_raw_parts(str, len as usize))
        }
    }
}

unsafe fn write_tree_children<F: Write>(parent: &delphi::TTreeNode, tabs: &mut String, f: &mut F) -> Result<()> {
    for i in 0..parent.GetCount() {
        let node = &*parent.GetItem(i);
        let name = try_decode(node.name)?;
        match (*node.data).rtype {
            2 => {
                writeln!(f, "{}+{}", tabs, name)?;
                tabs.push('\t');
                drop(name);
                write_tree_children(node, tabs, f)?;
                tabs.pop();
            },
            3 => writeln!(f, "{}|{}", tabs, name)?,
            _ => return Err(Error::Other(format!("failed to save resource tree {}", name))),
        }
    }
    Ok(())
}

fn try_decode(name: *const u16) -> Result<String> {
    decode_pas_str(name).into_string().map_err(|e| Error::UnicodeError(e.to_string_lossy().into()))
}

type IntPtr = *const u32;
type AssetList<T> = *const *const *const T;
type WStrListPtr = *const *const *const wchar_t;

const GAME_ID: IntPtr = 0x7907f4 as _;

const LAST_INSTANCE_ID: IntPtr = 0x77f2e0 as _;
const LAST_TILE_ID: IntPtr = 0x77f2e4 as _;

mod settings {
    #![allow(dead_code)]
    use crate::delphi::TGraphic;
    use winapi::ctypes::wchar_t;

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
    pub const AUTHOR: *const *const wchar_t = 0x77f5a8 as _;
    pub const VERSION_STRING: *const *const wchar_t = 0x77f5ac as _;
    pub const INFORMATION: *const *const wchar_t = 0x77f5b0 as _;
    pub const VERSION_MAJOR: *const u32 = 0x77f5b4 as _;
    pub const VERSION_MINOR: *const u32 = 0x77f5b8 as _;
    pub const VERSION_RELEASE: *const u32 = 0x77f5bc as _;
    pub const VERSION_BUILD: *const u32 = 0x77f5c0 as _;
    pub const COMPANY: *const *const wchar_t = 0x77f5c4 as _;
    pub const PRODUCT: *const *const wchar_t = 0x77f5c8 as _;
    pub const COPYRIGHT: *const *const wchar_t = 0x77f5cc as _;
    pub const DESCRIPTION: *const *const wchar_t = 0x77f5d0 as _;
}

const TRIGGER_COUNT: IntPtr = 0x77f3f8 as _;
const TRIGGERS: AssetList<Trigger> = 0x77f3f4 as _;

const CONSTANT_COUNT: IntPtr = 0x77f3c4 as _;
const CONSTANT_NAMES: WStrListPtr = 0x78c14c as _;
const CONSTANT_VALUES: WStrListPtr = 0x78c150 as _;

const SOUNDS: AssetList<Sound> = 0x77f2b8 as _;
const SOUND_NAMES: WStrListPtr = 0x77f2c0 as _;
const SOUND_COUNT: IntPtr = 0x77f2c8 as _;

const SPRITE_COUNT: IntPtr = 0x77f4d8 as _;
const SPRITE_NAMES: WStrListPtr = 0x77f4cc as _;
const SPRITES: AssetList<Sprite> = 0x77f4c4 as _;

const BACKGROUNDS: AssetList<Background> = 0x77f1ac as _;
const BACKGROUND_NAMES: WStrListPtr = 0x77f1b4 as _;
const BACKGROUND_COUNT: IntPtr = 0x77f1c0 as _;

const PATHS: AssetList<Path> = 0x77f608 as _;
const PATH_NAMES: WStrListPtr = 0x77f610 as _;
const PATH_COUNT: IntPtr = 0x77f618 as _;

const SCRIPTS: AssetList<Script> = 0x77f2cc as _;
const SCRIPT_NAMES: WStrListPtr = 0x77f2d4 as _;
const SCRIPT_COUNT: IntPtr = 0x77f2dc as _;

const FONT_COUNT: IntPtr = 0x77f50c as _;
const FONT_NAMES: WStrListPtr = 0x77f504 as _;
const FONTS: AssetList<Font> = 0x77f4fc as _;

const TIMELINE_COUNT: IntPtr = 0x77f4f4 as _;
const TIMELINE_NAMES: WStrListPtr = 0x77f4ec as _;
const TIMELINES: AssetList<Timeline> = 0x77f4e4 as _;

const OBJECTS: AssetList<Object> = 0x77f0d0 as _;
const OBJECT_NAMES: WStrListPtr = 0x77f0d8 as _;
const OBJECT_COUNT: IntPtr = 0x77f0e0 as _;

const ROOMS: AssetList<Room> = 0x77f3a8 as _;
const ROOM_NAMES: WStrListPtr = 0x77f3b0 as _;
const ROOM_COUNT: IntPtr = 0x77f3b8 as _;

const INCLUDED_FILES: AssetList<IncludedFile> = 0x77f420 as _;
const INCLUDED_FILE_COUNT: IntPtr = 0x77f428 as _;

const EXTENSIONS: AssetList<Extension> = 0x77f5d4 as _;
const EXTENSION_COUNT: IntPtr = 0x77f5d8 as _;
const EXTENSIONS_LOADED: *const *const bool = 0x790a14 as _;

const RT_OBJECTS: *const *const TTreeNode = 0x79a9b8 as _;
const RT_SPRITES: *const *const TTreeNode = 0x79a9bc as _;
const RT_SOUNDS: *const *const TTreeNode = 0x79a9c0 as _;
const RT_ROOMS: *const *const TTreeNode = 0x79a9c4 as _;
const RT_BACKGROUNDS: *const *const TTreeNode = 0x79a9c8 as _;
const RT_PATHS: *const *const TTreeNode = 0x79a9cc as _;
const RT_SCRIPTS: *const *const TTreeNode = 0x79a9d0 as _;
const RT_FONTS: *const *const TTreeNode = 0x79a9d4 as _;
const RT_TIMELINES: *const *const TTreeNode = 0x79a9d8 as _;
const RT_GAME_INFO: *const *const TTreeNode = 0x79a9dc as _;
const RT_GLOBAL_GAME_SETTINGS: *const *const TTreeNode = 0x79a9e0 as _;
const RT_EXTENSION_PACKAGES: *const *const TTreeNode = 0x79a9e4 as _;

enum Error {
    IoError(std::io::Error),
    ImageError(image::ImageError),
    UnicodeError(String),
    Other(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IoError(e) => write!(f, "io error: {}", e),
            Self::ImageError(e) => write!(f, "image error: {}", e),
            Self::UnicodeError(s) => write!(f, "couldn't encode {}", s),
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

fn get_asset<'a, T>(assets: AssetList<T>, count: IntPtr, id: i32) -> Option<&'a T> {
    unsafe {
        let assets = slice::from_raw_parts(assets.read(), count.read() as _);
        assets.get(usize::try_from(id).ok()?)?.as_ref()
    }
}

fn get_asset_name(names: WStrListPtr, count: IntPtr, id: i32) -> String {
    unsafe {
        let names = slice::from_raw_parts(names.read(), count.read() as _);
        usize::try_from(id)
            .ok()
            .and_then(|id| names.get(id))
            .and_then(|&s| decode_pas_str(s).into_string().ok())
            .unwrap_or(String::new())
    }
}

type Result<T> = std::result::Result<T, Error>;

unsafe fn save_frame(frame: &Frame, path: &std::path::Path, controller: &IOController) -> Result<()> {
    let f = controller.open_file(path)?;
    image::codecs::png::PngEncoder::new(f)
        .encode(
            slice::from_raw_parts(frame.data, (frame.width * frame.height * 4) as usize),
            frame.width,
            frame.height,
            image::ColorType::Rgba8,
        )
        .map_err(|_| Error::Other(format!("failed to save frame {}", path.to_string_lossy())))?;
    Ok(())
}

unsafe fn save_sound(sound: &Sound, path: &mut PathBuf, controller: &IOController) -> Result<()> {
    let extension = try_decode(sound.extension)?;
    path.set_extension(extension.trim_matches('.'));
    if let Some(data) = sound.data.as_ref() {
        let old_pos = data.get_pos();
        data.set_pos(0);
        let len = data.get_size() as usize;
        let mut s = Vec::with_capacity(len);
        s.set_len(len);
        data.read(s.as_mut_ptr(), len as _);
        data.set_pos(old_pos);
        controller.write_file(path, s)?;
    }
    path.set_extension("txt");
    let mut f = controller.open_file(&path)?;
    writeln!(f, "extension={}", extension)?;
    writeln!(f, "kind={}", sound.kind)?;
    writeln!(f, "effects={}", sound.effects)?;
    writeln!(f, "volume={}", sound.volume)?;
    writeln!(f, "pan={}", sound.pan)?;
    writeln!(f, "preload={}", u8::from(sound.preload))?;
    Ok(())
}

unsafe fn save_sprite(sprite: &Sprite, path: &mut PathBuf, controller: &IOController) -> Result<()> {
    std::fs::create_dir_all(&path)?;
    let frames = slice::from_raw_parts(sprite.frames, sprite.frame_count as _);
    for (i, frame) in frames.iter().enumerate() {
        path.push(format!("{}.png", i));
        save_frame(&**frame, path, controller)?;
        path.pop();
    }
    path.push("sprite.txt");
    let mut f = controller.open_file(&path)?;
    writeln!(f, "frames={}", sprite.frame_count)?;
    writeln!(f, "origin_x={}", sprite.origin_x)?;
    writeln!(f, "origin_y={}", sprite.origin_y)?;
    writeln!(f, "collision_shape={}", sprite.collision_shape)?;
    writeln!(f, "alpha_tolerance={}", sprite.alpha_tolerance)?;
    writeln!(f, "per_frame_colliders={}", sprite.per_frame_colliders as u8)?;
    writeln!(f, "bbox_type={}", sprite.bbox_type)?;
    writeln!(f, "bbox_left={}", sprite.bbox_left)?;
    writeln!(f, "bbox_bottom={}", sprite.bbox_bottom)?;
    writeln!(f, "bbox_right={}", sprite.bbox_right)?;
    writeln!(f, "bbox_top={}", sprite.bbox_top)?;
    path.pop();
    Ok(())
}

unsafe fn save_background(back: &Background, path: &mut PathBuf, controller: &IOController) -> Result<()> {
    path.set_extension("png");
    let frame = &*back.frame;
    if frame.width != 0 && frame.height != 0 {
        save_frame(frame, &path, controller)?;
    }
    path.set_extension("txt");
    let mut f = controller.open_file(path)?;
    writeln!(f, "width={}", frame.width)?;
    writeln!(f, "height={}", frame.height)?;
    writeln!(f, "tileset={}", back.is_tileset as u8)?;
    writeln!(f, "tile_width={}", back.tile_width)?;
    writeln!(f, "tile_height={}", back.tile_height)?;
    writeln!(f, "tile_hoffset={}", back.h_offset)?;
    writeln!(f, "tile_voffset={}", back.v_offset)?;
    writeln!(f, "tile_hsep={}", back.h_sep)?;
    writeln!(f, "tile_vsep={}", back.v_sep)?;
    Ok(())
}

unsafe fn save_path(path: &Path, file_path: &mut PathBuf, controller: &IOController) -> Result<()> {
    std::fs::create_dir_all(&file_path)?;
    file_path.push("path.txt");
    let mut f = controller.open_file(&file_path)?;
    writeln!(f, "connection={}", path.connection)?;
    writeln!(f, "closed={}", path.closed as u8)?;
    writeln!(f, "precision={}", path.precision)?;
    writeln!(f, "background={}", get_asset_name(ROOM_NAMES, ROOM_COUNT, path.path_editor_room_background))?;
    writeln!(f, "snap_x={}", path.snap_x)?;
    writeln!(f, "snap_y={}", path.snap_y)?;
    file_path.pop();
    file_path.push("points.txt");
    let mut f = controller.open_file(&file_path)?;
    let points = slice::from_raw_parts(path.points, path.point_count as _);
    for p in points {
        writeln!(f, "{},{},{}", p.x, p.y, p.speed)?;
    }
    file_path.pop();
    Ok(())
}

unsafe fn save_script(script: &Script, path: &mut PathBuf, controller: &IOController) -> Result<()> {
    path.set_extension("gml");
    controller.write_file(path, try_decode(script.source)?.into())?;
    Ok(())
}

unsafe fn save_font(font: &Font, path: &mut PathBuf, controller: &IOController) -> Result<()> {
    path.set_extension("txt");
    let mut f = controller.open_file(path)?;
    writeln!(f, "name={}", try_decode(font.sys_name)?)?;
    writeln!(f, "size={}", font.size)?;
    writeln!(f, "bold={}", font.bold as u8)?;
    writeln!(f, "italic={}", font.italic as u8)?;
    writeln!(f, "charset={}", font.charset)?;
    writeln!(f, "aa_level={}", font.aa_level)?;
    writeln!(f, "range_start={}", font.range_start)?;
    writeln!(f, "range_end={}", font.range_end)?;
    Ok(())
}

unsafe fn save_event<F: Write>(ev: &Event, name: &str, file: &mut F) -> Result<()> {
    writeln!(file, "#define {}", name)?;
    let actions = slice::from_raw_parts(ev.actions, ev.action_count as usize);
    for action in actions {
        let action = &**action;
        let code = try_decode(action.param_strings[0])?;
        write!(file, "{}", &code)?;
        if !code.ends_with('\n') {
            writeln!(file)?;
        }
    }
    Ok(())
}

unsafe fn save_timeline(tl: &Timeline, path: &mut PathBuf, controller: &IOController) -> Result<()> {
    path.set_extension("gml");
    let mut f = controller.open_file(path)?;
    let count = tl.moment_count as usize;
    let events = slice::from_raw_parts(tl.moment_events, count);
    let times = slice::from_raw_parts(tl.moment_times, count);
    for (time, event) in times.iter().zip(events) {
        let event = &**event;
        save_event(event, &time.to_string(), &mut f)?;
    }
    Ok(())
}

mod events {
    #![allow(dead_code)]
    pub const EV_CREATE: usize = 0;
    pub const EV_DESTROY: usize = 1;
    pub const EV_ALARM: usize = 2;
    pub const EV_STEP: usize = 3;
    pub const EV_COLLISION: usize = 4;
    pub const EV_KEYBOARD: usize = 5;
    pub const EV_MOUSE: usize = 6;
    pub const EV_OTHER: usize = 7;
    pub const EV_DRAW: usize = 8;
    pub const EV_KEYPRESS: usize = 9;
    pub const EV_KEYRELEASE: usize = 10;
    pub const EV_TRIGGER: usize = 11;

    pub const EVENT_ID_TO_NAME: [&str; 12] = [
        "Create",
        "Destroy",
        "Alarm",
        "Step",
        "Collision",
        "Keyboard",
        "Mouse",
        "Other",
        "Draw",
        "KeyPress",
        "KeyRelease",
        "Trigger",
    ];

    pub const EVENT_NAME_TO_ID: phf::Map<&'static str, usize> = phf::phf_map! {
        "Create" => EV_CREATE,
        "Destroy" => EV_DESTROY,
        "Step" => EV_STEP,
        "Alarm" => EV_ALARM,
        "Keyboard" => EV_KEYBOARD,
        "Mouse" => EV_MOUSE,
        "Collision" => EV_COLLISION,
        "Other" => EV_OTHER,
        "Draw" => EV_DRAW,
        "KeyPress" => EV_KEYPRESS,
        "KeyRelease" => EV_KEYRELEASE,
        "Trigger" => EV_TRIGGER,
    };
}

unsafe fn event_name(ev_type: usize, ev_numb: usize) -> String {
    match ev_type {
        events::EV_COLLISION => format!(
            "{}_{}",
            events::EVENT_ID_TO_NAME[ev_type],
            get_asset_name(OBJECT_NAMES, OBJECT_COUNT, ev_numb as _)
        ),
        events::EV_TRIGGER => format!(
            "{}_{}",
            events::EVENT_ID_TO_NAME[ev_type],
            get_asset(TRIGGERS, TRIGGER_COUNT, ev_numb as _)
                .and_then(|t| decode_pas_str(t.name).into_string().ok())
                .unwrap_or("".into())
        ),
        _ => format!("{}_{}", events::EVENT_ID_TO_NAME[ev_type], ev_numb),
    }
}

unsafe fn save_object(obj: &Object, path: &mut PathBuf, controller: &IOController) -> Result<()> {
    path.set_extension("txt");
    {
        let mut f = controller.open_file(&path)?;
        writeln!(f, "sprite={}", get_asset_name(SPRITE_NAMES, SPRITE_COUNT, obj.sprite_index))?;
        writeln!(f, "visible={}", u8::from(obj.visible))?;
        writeln!(f, "solid={}", u8::from(obj.solid))?;
        writeln!(f, "persistent={}", u8::from(obj.persistent))?;
        writeln!(f, "depth={}", obj.depth)?;
        writeln!(f, "parent={}", get_asset_name(OBJECT_NAMES, OBJECT_COUNT, obj.parent_index))?;
        writeln!(f, "mask={}", get_asset_name(SPRITE_NAMES, SPRITE_COUNT, obj.mask_index))?;
    }
    path.set_extension("gml");
    {
        let mut f = controller.open_file(&path)?;
        for (ev_type, event_group) in obj.events.iter().enumerate().filter(|(_, p)| !p.is_null()) {
            let count = event_group.sub(1).cast::<u32>().read() as usize;
            let events = slice::from_raw_parts(*event_group, count);
            for (ev_numb, ev) in events.iter().enumerate() {
                let ev = &**ev; // all events in the array are non-null
                if ev.action_count != 0 {
                    let name = event_name(ev_type, ev_numb);
                    save_event(ev, &name, &mut f)?;
                }
            }
        }
    }
    Ok(())
}

unsafe fn save_tiles(tiles: &[Tile], path: &mut PathBuf, controller: &IOController) -> Result<()> {
    let mut layers = HashMap::new();
    for tile in tiles {
        let f = match layers.entry(tile.depth) {
            std::collections::hash_map::Entry::Occupied(e) => e.into_mut(),
            std::collections::hash_map::Entry::Vacant(e) => {
                path.push(format!("{}.txt", tile.depth));
                let f = e.insert(controller.open_file(&path)?);
                path.pop();
                f
            },
        };
        writeln!(
            f,
            "{},{},{},{},{},{},{},{}",
            get_asset_name(BACKGROUND_NAMES, BACKGROUND_COUNT, tile.source_bg),
            tile.x,
            tile.y,
            tile.u,
            tile.v,
            tile.width,
            tile.height,
            u8::from(tile.locked)
        )?;
    }
    path.push("layers.txt");
    let mut f = controller.open_file(&path)?;
    for depth in layers.keys() {
        writeln!(f, "{}", depth)?;
    }
    path.pop();
    Ok(())
}

fn save_instances(instances: &[Instance], path: &mut PathBuf, controller: &IOController) -> Result<()> {
    path.push("instances.txt");
    let mut f = controller.open_file(&path)?;
    path.pop();
    let mut codes = HashSet::with_capacity(instances.len());
    for instance in instances {
        let code = try_decode(instance.creation_code)?;
        let mut hash_hex = [0; 8];
        let fname = if code.len() != 0 {
            let hash = crc::crc32::checksum_ieee(code.as_ref());
            for i in 0..8 {
                hash_hex[7 - i] = match (hash >> i) & 0xf {
                    i if i < 10 => b'0' + i as u8,
                    i => b'A' - 10 + i as u8,
                };
            }
            let fname = unsafe { str::from_utf8_unchecked(&hash_hex) };
            if !codes.insert(hash) {
                path.push(fname);
                path.set_extension("gml");
                controller.write_file(&path, code.into())?;
                path.pop();
            }
            fname
        } else {
            ""
        };
        writeln!(
            f,
            "{},{},{},{},{}",
            get_asset_name(OBJECT_NAMES, OBJECT_COUNT, instance.object),
            instance.x,
            instance.y,
            fname,
            u8::from(instance.locked),
        )?;
    }
    Ok(())
}

unsafe fn save_room(room: &Room, path: &mut PathBuf, controller: &IOController) -> Result<()> {
    std::fs::create_dir_all(&path)?;
    path.push("room.txt");
    {
        let mut f = controller.open_file(&path)?;
        writeln!(f, "caption={}", try_decode(room.caption)?)?;
        writeln!(f, "width={}", room.width)?;
        writeln!(f, "height={}", room.height)?;
        writeln!(f, "snap_x={}", room.snap_x)?;
        writeln!(f, "snap_y={}", room.snap_y)?;
        writeln!(f, "isometric={}", u8::from(room.isometric))?;
        writeln!(f, "roomspeed={}", room.speed)?;
        writeln!(f, "roompersistent={}", u8::from(room.persistent))?;
        writeln!(f, "bg_color={}", room.bg_colour)?; // TODO: hex?
        writeln!(f, "clear_screen={}", u8::from(room.clear_screen))?;
        writeln!(f, "clear_view={}", u8::from(room.clear_view))?;
        writeln!(f)?;
        for (i, bg) in room.backgrounds.iter().enumerate() {
            writeln!(f, "bg_visible{}={}", i, bg.visible_on_start)?;
            writeln!(f, "bg_is_foreground{}={}", i, bg.is_foreground)?;
            writeln!(f, "bg_source{}={}", i, get_asset_name(BACKGROUND_NAMES, BACKGROUND_COUNT, bg.source_bg))?;
            writeln!(f, "bg_xoffset{}={}", i, bg.xoffset)?;
            writeln!(f, "bg_yoffset{}={}", i, bg.yoffset)?;
            writeln!(f, "bg_tile_h{}={}", i, bg.tile_horz)?;
            writeln!(f, "bg_tile_v{}={}", i, bg.tile_vert)?;
            writeln!(f, "bg_hspeed{}={}", i, bg.hspeed)?;
            writeln!(f, "bg_vspeed{}={}", i, bg.vspeed)?;
            writeln!(f, "bg_stretch{}={}", i, u8::from(bg.stretch))?;
        }
        writeln!(f)?;
        writeln!(f, "views_enabled={}", room.views_enabled)?;
        for (i, view) in room.views.iter().enumerate() {
            writeln!(f, "view_visible{}={}", i, view.visible)?;
            writeln!(f, "view_xview{}={}", i, view.source_x)?;
            writeln!(f, "view_yview{}={}", i, view.source_y)?;
            writeln!(f, "view_wview{}={}", i, view.source_w)?;
            writeln!(f, "view_hview{}={}", i, view.source_h)?;
            writeln!(f, "view_xport{}={}", i, view.port_x)?;
            writeln!(f, "view_yport{}={}", i, view.port_y)?;
            writeln!(f, "view_wport{}={}", i, view.port_w)?;
            writeln!(f, "view_hport{}={}", i, view.port_h)?;
            writeln!(f, "view_fol_hbord{}={}", i, view.following_hborder)?;
            writeln!(f, "view_fol_vbord{}={}", i, view.following_vborder)?;
            writeln!(f, "view_fol_hspeed{}={}", i, view.following_hspeed)?;
            writeln!(f, "view_fol_vspeed{}={}", i, view.following_vspeed)?;
            writeln!(f, "view_fol_target{}={}", i, get_asset_name(OBJECT_NAMES, OBJECT_COUNT, view.following_target))?;
        }
        writeln!(f)?;
        writeln!(f, "remember={}", u8::from(room.remember_room_editor_info))?;
        writeln!(f, "editor_width={}", room.editor_width)?;
        writeln!(f, "editor_height={}", room.editor_height)?;
        writeln!(f, "show_grid={}", u8::from(room.show_grid))?;
        writeln!(f, "show_objects={}", u8::from(room.show_objects))?;
        writeln!(f, "show_tiles={}", u8::from(room.show_tiles))?;
        writeln!(f, "show_backgrounds={}", u8::from(room.show_backgrounds))?;
        writeln!(f, "show_foregrounds={}", u8::from(room.show_foregrounds))?;
        writeln!(f, "show_views={}", u8::from(room.show_views))?;
        writeln!(f, "delete_underlying_objects={}", u8::from(room.delete_underlying_objects))?;
        writeln!(f, "delete_underlying_tiles={}", u8::from(room.delete_underlying_tiles))?;
        writeln!(f, "tab={}", room.tab)?; // wtf is this
        writeln!(f, "editor_x={}", room.x_position_scroll)?;
        writeln!(f, "editor_y={}", room.y_position_scroll)?;
    }
    path.pop();
    path.push("code.gml");
    std::fs::write(&path, try_decode(room.creation_code)?)?;
    path.pop();

    let tiles = slice::from_raw_parts(room.tiles, room.tile_count as usize);
    save_tiles(tiles, path, controller)?;

    let instances = slice::from_raw_parts(room.instances, room.instance_count as usize);
    save_instances(instances, path, controller)?;
    Ok(())
}

unsafe fn save_constants(path: &mut PathBuf, controller: &IOController) -> Result<()> {
    path.push("constants.txt");
    let mut f = controller.open_file(&path)?;
    path.pop();
    let count = CONSTANT_COUNT.read() as usize;
    let names = slice::from_raw_parts(CONSTANT_NAMES.read(), count);
    let values = slice::from_raw_parts(CONSTANT_VALUES.read(), count);
    for i in 0..count {
        writeln!(f, "{}={}", try_decode(names[i])?, try_decode(values[i])?)?;
    }
    Ok(())
}

unsafe fn save_settings(path: &mut PathBuf, controller: &IOController) -> Result<()> {
    use settings::*;
    path.push("settings");
    std::fs::create_dir_all(&path)?;
    save_constants(path, controller)?;
    path.push("settings.txt");
    let mut f = controller.open_file(&path)?;
    path.pop();
    writeln!(f, "fullscreen={}", u8::from(settings::FULLSCREEN.read()))?;
    writeln!(f, "interpolate_pixels={}", u8::from(settings::INTERPOLATE_PIXELS.read()))?;
    writeln!(f, "dont_draw_border={}", u8::from(DONT_DRAW_BORDER.read()))?;
    writeln!(f, "display_cursor={}", u8::from(DISPLAY_CURSOR.read()))?;
    writeln!(f, "scaling={}", SCALING.read())?;
    writeln!(f, "allow_resize={}", u8::from(ALLOW_RESIZE.read()))?;
    writeln!(f, "window_on_top={}", u8::from(*WINDOW_ON_TOP))?;
    writeln!(f, "clear_color={}", *CLEAR_COLOUR)?;
    writeln!(f, "set_resolution={}", u8::from(*SET_RESOLUTION))?;
    writeln!(f, "color_depth={}", *COLOUR_DEPTH)?;
    writeln!(f, "resolution={}", *RESOLUTION)?;
    writeln!(f, "frequency={}", *FREQUENCY)?;
    writeln!(f, "dont_show_buttons={}", u8::from(*DONT_SHOW_BUTTONS))?;
    writeln!(f, "vsync={}", *VSYNC_AND_FORCE_CPU & 1)?;
    writeln!(f, "force_cpu_render={}", u8::from(*VSYNC_AND_FORCE_CPU & (1 << 7) != 0))?;
    writeln!(f, "disable_screensaver={}", u8::from(*DISABLE_SCREENSAVER))?;
    writeln!(f, "f4_fullscreen_toggle={}", u8::from(*F4_FULLSCREEN))?;
    writeln!(f, "f1_help_menu={}", u8::from(*F1_HELP))?;
    writeln!(f, "esc_close_game={}", u8::from(*ESC_CLOSE))?;
    writeln!(f, "f5_save_f6_load={}", u8::from(*F5_SAVE_F6_LOAD))?;
    writeln!(f, "f9_screenshot={}", u8::from(*F9_SCREENSHOT))?;
    writeln!(f, "treat_close_as_esc={}", u8::from(*TREAT_CLOSE_AS_ESC))?;
    writeln!(f, "priority={}", *PRIORITY)?;
    writeln!(f, "freeze_on_lose_focus={}", *FREEZE_ON_LOSE_FOCUS)?;
    writeln!(f, "custom_loader={}", u8::from(*HAS_CUSTOM_LOAD_IMAGE))?;
    writeln!(f, "custom_bar={}", *LOADING_BAR)?;
    writeln!(f, "transparent={}", u8::from(*LOADING_TRANSPARENT))?;
    writeln!(f, "translucency={}", *LOADING_TRANSLUCENCY)?;
    writeln!(f, "scale_progress_bar={}", u8::from(*LOADING_PROGRESS_BAR_SCALE))?;
    writeln!(f, "show_error_messages={}", u8::from(*SHOW_ERROR_MESSAGES))?;
    writeln!(f, "log_errors={}", u8::from(*LOG_ERRORS))?;
    writeln!(f, "always_abort={}", u8::from(*ALWAYS_ABORT))?;
    writeln!(f, "zero_uninitialized_vars={}", u8::from(*ZERO_UNINITIALIZED_VARS))?;
    writeln!(f, "error_on_uninitialized_args={}", u8::from(*ERROR_ON_UNINITIALIZED_ARGS))?;
    if settings::LOADING_BAR.read() == 2 {
        path.push("back.bmp");
        (&*LOADING_BACKGROUND.read()).SaveToFile(DelphiUStr::new(path.as_ref()).0);
        path.pop();
        path.push("front.bmp");
        (&*LOADING_FOREGROUND.read()).SaveToFile(DelphiUStr::new(path.as_ref()).0);
        path.pop();
    }
    if HAS_CUSTOM_LOAD_IMAGE.read() {
        path.push("loader.bmp");
        (&*CUSTOM_LOAD_IMAGE.read()).SaveToFile(DelphiUStr::new(path.as_ref()).0);
        path.pop();
    }
    path.push("icon.ico");
    (&*ICON.read()).SaveToFile(DelphiUStr::new(path.as_ref()).0);
    path.pop();
    path.pop();
    Ok(())
}

unsafe fn save_assets<T>(
    bar_start: u32,
    bar_end: u32,
    name: &str,
    assets: AssetList<T>,
    names: WStrListPtr,
    count: IntPtr,
    tree: *const *const TTreeNode,
    save_func: unsafe fn(&T, &mut PathBuf, &IOController) -> Result<()>,
    path: &mut PathBuf,
    controller: &IOController,
) -> Result<()> {
    path.push(name);
    std::fs::create_dir_all(&path)?;
    let count = count.read() as usize;
    let assets = slice::from_raw_parts(assets.read(), count);
    let names = slice::from_raw_parts(names.read(), count);
    path.push("index.yyd");
    let mut index = controller.open_file(&path)?;
    path.pop();
    for i in 0..count {
        if let Some(asset) = assets[i].as_ref() {
            let name = try_decode(names[i])?;
            path.push(&name);
            save_func(asset, path, controller)?;
            path.pop();
            writeln!(index, "{}", name)?;
        } else {
            writeln!(index, "")?;
        }
        advance_progress_form(bar_start + (bar_end - bar_start) * i as u32 / count as u32);
    }
    path.push("tree.yyd");
    if let Some(tree) = tree.as_ref() {
        write_tree_children(&**tree, &mut String::new(), &mut controller.open_file(&path)?)?;
    }
    path.pop();
    path.pop();
    Ok(())
}

unsafe fn save_gmk(mut path: PathBuf) -> Result<()> {
    let controller = io_queue::IOController::new();
    {
        // some stuff to go in the main gmk
        let mut f = controller.open_file(&path)?;
        writeln!(f, "game_id={}", GAME_ID.read())?;
        writeln!(f)?;
        writeln!(f, "company={}", try_decode(*settings::COMPANY)?)?;
        writeln!(f, "product={}", try_decode(*settings::PRODUCT)?)?;
        writeln!(f, "copyright={}", try_decode(*settings::COPYRIGHT)?)?;
        writeln!(f, "description={}", try_decode(*settings::DESCRIPTION)?)?;
        writeln!(
            f,
            "version={}.{}.{}.{}",
            *settings::VERSION_MAJOR,
            *settings::VERSION_MINOR,
            *settings::VERSION_RELEASE,
            *settings::VERSION_BUILD
        )?;
        writeln!(f)?;
        /*
        writeln!(f, "last_instance_id={}", *LAST_INSTANCE_ID)?;
        writeln!(f, "last_tile_id={}", *LAST_TILE_ID)?;
         */
    }
    path.pop();
    advance_progress_form(5);
    save_settings(&mut path, &controller)?;
    advance_progress_form(10);
    // triggers
    advance_progress_form(15);
    save_assets(15, 30, "sounds", SOUNDS, SOUND_NAMES, SOUND_COUNT, RT_SOUNDS, save_sound, &mut path, &controller)?;
    advance_progress_form(30);
    save_assets(
        30,
        55,
        "sprites",
        SPRITES,
        SPRITE_NAMES,
        SPRITE_COUNT,
        RT_SPRITES,
        save_sprite,
        &mut path,
        &controller,
    )?;
    advance_progress_form(55);
    save_assets(
        55,
        65,
        "backgrounds",
        BACKGROUNDS,
        BACKGROUND_NAMES,
        BACKGROUND_COUNT,
        RT_BACKGROUNDS,
        save_background,
        &mut path,
        &controller,
    )?;
    advance_progress_form(65);
    save_assets(65, 70, "paths", PATHS, PATH_NAMES, PATH_COUNT, RT_PATHS, save_path, &mut path, &controller)?;
    advance_progress_form(70);
    save_assets(
        70,
        75,
        "scripts",
        SCRIPTS,
        SCRIPT_NAMES,
        SCRIPT_COUNT,
        RT_SCRIPTS,
        save_script,
        &mut path,
        &controller,
    )?;
    advance_progress_form(75);
    save_assets(75, 80, "fonts", FONTS, FONT_NAMES, FONT_COUNT, RT_FONTS, save_font, &mut path, &controller)?;
    advance_progress_form(80);
    save_assets(
        80,
        85,
        "timelines",
        TIMELINES,
        TIMELINE_NAMES,
        TIMELINE_COUNT,
        RT_TIMELINES,
        save_timeline,
        &mut path,
        &controller,
    )?;
    advance_progress_form(85);
    save_assets(
        85,
        90,
        "objects",
        OBJECTS,
        OBJECT_NAMES,
        OBJECT_COUNT,
        RT_OBJECTS,
        save_object,
        &mut path,
        &controller,
    )?;
    advance_progress_form(90);
    save_assets(90, 95, "rooms", ROOMS, ROOM_NAMES, ROOM_COUNT, RT_ROOMS, save_room, &mut path, &controller)?;
    advance_progress_form(95);
    // included files

    controller.finish()?;
    advance_progress_form(100);
    Ok(())
}

#[repr(transparent)]
struct DelphiUStr(*const u16);

impl DelphiUStr {
    fn new(s: &OsStr) -> Self {
        let mut out = std::ptr::null();
        let s = s.encode_wide().collect::<Vec<_>>();
        unsafe {
            delphi::UStrFromPWCharLen(&mut out, s.as_ptr(), s.len());
        }
        DelphiUStr(out)
    }
}

impl Drop for DelphiUStr {
    fn drop(&mut self) {
        unsafe { delphi::UStrClr(&mut self.0) }
    }
}

#[no_mangle]
pub unsafe extern "C" fn injected() -> u32 {
    // get the path to the gm81 file
    let ebp: *const *const u16;
    asm!("mov {}, [ebp]", out(reg) ebp);
    let real_string = ebp.sub(1).read();
    let path = decode_pas_str(real_string);

    if let Err(e) = save_gmk(path.into()) {
        // display the error
        delphi::ShowMessage(DelphiUStr::new(format!("Failed to save: {}", e).as_ref()).0);
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
        delphi::ShowMessage(DelphiUStr::new(info.to_string().as_ref()).0);
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
}

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
