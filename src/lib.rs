#![feature(asm)]

mod asset;
mod delphi;
mod ide;
mod io_queue;
mod stub;

use crate::{
    asset::*,
    delphi::{advance_progress_form, TTreeNode, UStr},
    io_queue::IOController,
};
use ctor::ctor;
use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
    fmt::Formatter,
    io::Write,
    path::PathBuf,
    slice, str,
};
use winapi::um::{
    memoryapi::VirtualProtect,
    processthreadsapi::{FlushInstructionCache, GetCurrentProcess},
    winnt::PAGE_READWRITE,
};

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

fn try_decode(name: &UStr) -> Result<String> {
    name.to_os_string().into_string().map_err(|e| Error::UnicodeError(e.to_string_lossy().into()))
}

trait GetAsset<T> {
    fn get_asset(&self, id: i32) -> T;
}

impl<'a, T> GetAsset<Option<&'a T>> for &'a [*const T] {
    fn get_asset(&self, id: i32) -> Option<&'a T> {
        unsafe { self.get(usize::try_from(id).ok()?)?.as_ref() }
    }
}

impl<'a> GetAsset<String> for &'a [UStr] {
    fn get_asset(&self, id: i32) -> String {
        usize::try_from(id)
            .ok()
            .and_then(|id| self.get(id))
            .and_then(|s| s.to_os_string().into_string().ok())
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

unsafe fn save_stream(data: &delphi::TStream, path: &std::path::Path, controller: &IOController) -> Result<()> {
    let old_pos = data.get_pos();
    data.set_pos(0);
    let len = data.get_size() as usize;
    let mut s = Vec::with_capacity(len);
    s.set_len(len);
    data.read(s.as_mut_ptr(), len as _);
    data.set_pos(old_pos);
    controller.write_file(path, s)?;
    Ok(())
}

unsafe fn save_sound(sound: &Sound, path: &mut PathBuf, controller: &IOController) -> Result<()> {
    let extension = try_decode(&sound.extension)?;
    path.set_extension(extension.trim_matches('.'));
    if let Some(data) = sound.data.as_ref() {
        save_stream(data, &path, controller)?;
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
    writeln!(f, "background={}", ide::get_room_names().get_asset(path.path_editor_room_background))?;
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
    controller.write_file(path, try_decode(&script.source)?.into())?;
    Ok(())
}

unsafe fn save_font(font: &Font, path: &mut PathBuf, controller: &IOController) -> Result<()> {
    path.set_extension("txt");
    let mut f = controller.open_file(path)?;
    writeln!(f, "name={}", try_decode(&font.sys_name)?)?;
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
        let code = try_decode(&action.param_strings[0])?;
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
        events::EV_COLLISION => {
            format!("{}_{}", events::EVENT_ID_TO_NAME[ev_type], ide::get_object_names().get_asset(ev_numb as _))
        },
        events::EV_TRIGGER => format!(
            "{}_{}",
            events::EVENT_ID_TO_NAME[ev_type],
            ide::get_triggers().get_asset(ev_numb as _).and_then(|t| try_decode(&t.name).ok()).unwrap_or("".into())
        ),
        _ => format!("{}_{}", events::EVENT_ID_TO_NAME[ev_type], ev_numb),
    }
}

unsafe fn save_object(obj: &Object, path: &mut PathBuf, controller: &IOController) -> Result<()> {
    path.set_extension("txt");
    {
        let mut f = controller.open_file(&path)?;
        writeln!(f, "sprite={}", ide::get_sprite_names().get_asset(obj.sprite_index))?;
        writeln!(f, "visible={}", u8::from(obj.visible))?;
        writeln!(f, "solid={}", u8::from(obj.solid))?;
        writeln!(f, "persistent={}", u8::from(obj.persistent))?;
        writeln!(f, "depth={}", obj.depth)?;
        writeln!(f, "parent={}", ide::get_object_names().get_asset(obj.parent_index))?;
        writeln!(f, "mask={}", ide::get_sprite_names().get_asset(obj.mask_index))?;
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
            ide::get_background_names().get_asset(tile.source_bg),
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
        let code = try_decode(&instance.creation_code)?;
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
            ide::get_object_names().get_asset(instance.object),
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
        writeln!(f, "caption={}", try_decode(&room.caption)?)?;
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
            writeln!(f, "bg_source{}={}", i, ide::get_background_names().get_asset(bg.source_bg))?;
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
            writeln!(f, "view_fol_target{}={}", i, ide::get_object_names().get_asset(view.following_target))?;
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
    std::fs::write(&path, try_decode(&room.creation_code)?)?;
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
    let names = ide::get_constant_names();
    let values = ide::get_constants();
    for (name, value) in names.iter().zip(values) {
        writeln!(f, "{}={}", try_decode(name)?, try_decode(value)?)?;
    }
    Ok(())
}

unsafe fn save_settings(path: &mut PathBuf, controller: &IOController) -> Result<()> {
    use ide::settings::*;
    path.push("settings");
    std::fs::create_dir_all(&path)?;
    save_constants(path, controller)?;
    path.push("information.txt");
    controller.write_file(&path, try_decode(&*ide::settings::INFO_INFORMATION)?.into())?;
    path.pop();
    path.push("settings.txt");
    let mut f = controller.open_file(&path)?;
    path.pop();
    writeln!(f, "fullscreen={}", u8::from(FULLSCREEN.read()))?;
    writeln!(f, "interpolate_pixels={}", u8::from(INTERPOLATE_PIXELS.read()))?;
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
    writeln!(f, "freeze_on_lose_focus={}", u8::from(*FREEZE_ON_LOSE_FOCUS))?;
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
    if LOADING_BAR.read() == 2 {
        path.push("back.bmp");
        (&*LOADING_BACKGROUND.read()).SaveToFile(&UStr::new(path.as_ref()));
        path.pop();
        path.push("front.bmp");
        (&*LOADING_FOREGROUND.read()).SaveToFile(&UStr::new(path.as_ref()));
        path.pop();
    }
    if HAS_CUSTOM_LOAD_IMAGE.read() {
        path.push("loader.bmp");
        (&*CUSTOM_LOAD_IMAGE.read()).SaveToFile(&UStr::new(path.as_ref()));
        path.pop();
    }
    path.push("icon.ico");
    (&*ICON.read()).SaveToFile(&UStr::new(path.as_ref()));
    path.pop();
    path.push("extensions.txt");
    let extensions = ide::get_extensions();
    let extensions_loaded = ide::get_extensions_loaded();
    let mut f = controller.open_file(&path)?;
    for (extension, &loaded) in extensions.iter().zip(extensions_loaded) {
        if loaded {
            writeln!(f, "{}", try_decode(&(&**extension).name)?)?;
        }
    }
    path.pop();
    path.pop();
    Ok(())
}

unsafe fn save_triggers(path: &mut PathBuf, controller: &IOController) -> Result<()> {
    path.push("triggers");
    std::fs::create_dir_all(&path)?;
    path.push("index.yyd");
    let mut index = controller.open_file(&path)?;
    path.pop();
    let triggers = ide::get_triggers();
    for trigger in triggers {
        if let Some(trigger) = trigger.as_ref() {
            let name = try_decode(&trigger.name)?;
            path.push(&name);
            path.set_extension("txt");
            let mut f = controller.open_file(&path)?;
            writeln!(f, "constant={}", try_decode(&trigger.constant_name)?)?;
            writeln!(f, "kind={}", trigger.kind)?;
            path.set_extension("gml");
            controller.write_file(&path, try_decode(&trigger.condition)?.into())?;
            path.pop();
            writeln!(index, "{}", name)?;
        }
    }
    path.pop();
    Ok(())
}

unsafe fn save_assets<T>(
    bar_start: u32,
    bar_end: u32,
    name: &str,
    assets: &[*const T],
    names: &[UStr],
    tree: *const *const TTreeNode,
    save_func: unsafe fn(&T, &mut PathBuf, &IOController) -> Result<()>,
    path: &mut PathBuf,
    controller: &IOController,
) -> Result<()> {
    path.push(name);
    std::fs::create_dir_all(&path)?;
    path.push("index.yyd");
    let mut index = controller.open_file(&path)?;
    path.pop();
    let count = assets.len();
    for i in 0..count {
        if let Some(asset) = assets[i].as_ref() {
            let name = try_decode(&names[i])?;
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

unsafe fn save_included_files(path: &mut PathBuf, controller: &IOController) -> Result<()> {
    path.push("datafiles");
    path.push("include");
    std::fs::create_dir_all(&path)?;
    path.pop();
    path.push("index.txt");
    let mut index = controller.open_file(&path)?;
    path.pop();
    let files = ide::get_included_files();
    for file in files {
        let file = &**file;
        let mut name = try_decode(&file.file_name)?;
        writeln!(index, "{}", &name)?;
        if file.data_exists && file.stored_in_gmk {
            if let Some(stream) = file.data.as_ref() {
                path.push("include");
                path.push(&name);
                save_stream(stream, &path, controller)?;
                path.pop();
                path.pop();
            }
        }
        name += ".txt";
        path.push(&name);
        let mut f = controller.open_file(&path)?;
        writeln!(f, "store={}", u8::from(file.stored_in_gmk))?;
        writeln!(f, "free={}", u8::from(file.free_memory))?;
        writeln!(f, "overwrite={}", u8::from(file.overwrite_file))?;
        writeln!(f, "remove={}", u8::from(file.remove_at_end))?;
        writeln!(f, "export={}", file.export_setting)?;
        if file.export_setting == 3 {
            writeln!(f, "export_folder={}", try_decode(&file.export_custom_folder)?)?;
        }
    }
    path.pop();
    Ok(())
}

unsafe fn write_tree_children<F: Write>(parent: &delphi::TTreeNode, tabs: &mut String, f: &mut F) -> Result<()> {
    for i in 0..parent.GetCount() {
        let node = &*parent.GetItem(i);
        let name = try_decode(&node.name)?;
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

unsafe fn save_gmk(mut path: PathBuf) -> Result<()> {
    let controller = io_queue::IOController::new();
    {
        // some stuff to go in the main gmk
        let mut f = controller.open_file(&path)?;
        writeln!(f, "gameid={}", ide::GAME_ID.read())?;
        writeln!(f)?;
        writeln!(f, "info_author={}", try_decode(&*ide::settings::INFO_AUTHOR)?)?;
        writeln!(f, "info_version={}", try_decode(&*ide::settings::INFO_VERSION)?)?;
        writeln!(f, "info_timestamp={}", delphi::Now())?;
        writeln!(
            f,
            "info_information={}",
            try_decode(&*ide::settings::INFO_INFORMATION)?
                .replace('\\', "\\\\")
                .replace('\r', "\\r")
                .replace('\n', "\\n")
        )?;
        writeln!(f)?;
        writeln!(f, "exe_company={}", try_decode(&*ide::settings::EXE_COMPANY)?)?;
        writeln!(f, "exe_product={}", try_decode(&*ide::settings::EXE_PRODUCT)?)?;
        writeln!(f, "exe_copyright={}", try_decode(&*ide::settings::EXE_COPYRIGHT)?)?;
        writeln!(f, "exe_description={}", try_decode(&*ide::settings::EXE_DESCRIPTION)?)?;
        writeln!(
            f,
            "exe_version={}.{}.{}.{}",
            *ide::settings::VERSION_MAJOR,
            *ide::settings::VERSION_MINOR,
            *ide::settings::VERSION_RELEASE,
            *ide::settings::VERSION_BUILD
        )?;
        writeln!(f)?;
        /*
        writeln!(f, "last_instance_id={}", *ide::_LAST_INSTANCE_ID)?;
        writeln!(f, "last_tile_id={}", *ide::_LAST_TILE_ID)?;
         */
    }
    path.pop();
    advance_progress_form(5);
    save_settings(&mut path, &controller)?;
    advance_progress_form(10);
    save_triggers(&mut path, &controller)?;
    advance_progress_form(15);
    save_assets(
        15,
        30,
        "sounds",
        ide::get_sounds(),
        ide::get_sound_names(),
        ide::RT_SOUNDS,
        save_sound,
        &mut path,
        &controller,
    )?;
    advance_progress_form(30);
    save_assets(
        30,
        55,
        "sprites",
        ide::get_sprites(),
        ide::get_sprite_names(),
        ide::RT_SPRITES,
        save_sprite,
        &mut path,
        &controller,
    )?;
    advance_progress_form(55);
    save_assets(
        55,
        65,
        "backgrounds",
        ide::get_backgrounds(),
        ide::get_background_names(),
        ide::RT_BACKGROUNDS,
        save_background,
        &mut path,
        &controller,
    )?;
    advance_progress_form(65);
    save_assets(
        65,
        70,
        "paths",
        ide::get_paths(),
        ide::get_path_names(),
        ide::RT_PATHS,
        save_path,
        &mut path,
        &controller,
    )?;
    advance_progress_form(70);
    save_assets(
        70,
        75,
        "scripts",
        ide::get_scripts(),
        ide::get_script_names(),
        ide::RT_SCRIPTS,
        save_script,
        &mut path,
        &controller,
    )?;
    advance_progress_form(75);
    save_assets(
        75,
        80,
        "fonts",
        ide::get_fonts(),
        ide::get_font_names(),
        ide::RT_FONTS,
        save_font,
        &mut path,
        &controller,
    )?;
    advance_progress_form(80);
    save_assets(
        80,
        85,
        "timelines",
        ide::get_timelines(),
        ide::get_timeline_names(),
        ide::RT_TIMELINES,
        save_timeline,
        &mut path,
        &controller,
    )?;
    advance_progress_form(85);
    save_assets(
        85,
        90,
        "objects",
        ide::get_objects(),
        ide::get_object_names(),
        ide::RT_OBJECTS,
        save_object,
        &mut path,
        &controller,
    )?;
    advance_progress_form(90);
    save_assets(
        90,
        95,
        "rooms",
        ide::get_rooms(),
        ide::get_room_names(),
        ide::RT_ROOMS,
        save_room,
        &mut path,
        &controller,
    )?;
    advance_progress_form(95);
    save_included_files(&mut path, &controller)?;
    // no game information or library init code

    controller.finish()?;
    advance_progress_form(100);
    Ok(())
}

fn show_message(msg: &str) {
    unsafe {
        delphi::ShowMessage(&UStr::new(msg.as_ref()));
    }
}

unsafe extern "C" fn injected() -> u32 {
    // get the path to the gm81 file
    let ebp: *const UStr;
    asm!("mov {}, [ebp]", out(reg) ebp);
    let real_string = ebp.sub(1).read();
    let path = real_string.to_os_string();

    if let Err(e) = save_gmk(path.into()) {
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
}
