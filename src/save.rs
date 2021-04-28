use crate::{
    asset::*,
    delphi,
    delphi::{advance_progress_form, TTreeNode, UStr},
    ide,
};
use rayon::prelude::*;
use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
    slice, str,
};

pub enum Error {
    IoError(std::io::Error),
    ImageError(image::ImageError),
    UnicodeError(String),
    Other(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

type Result<T> = std::result::Result<T, Error>;

impl UStr {
    fn try_decode(&self) -> Result<String> {
        self.to_os_string().into_string().map_err(|e| Error::UnicodeError(e.to_string_lossy().into()))
    }

    fn try_decode_opt(&self) -> Option<String> {
        // i feel like having this separately might provide better optimization?
        self.to_os_string().into_string().ok()
    }

    fn try_delimit(&self) -> Result<String> {
        self.try_decode()
            .map(|s| s.replace('\\', "\\\\").replace('\r', "\\r").replace('\n', "\\n").replace("*/", "*\\/"))
    }
}

trait GetAsset<T> {
    fn get_asset(&self, id: i32) -> T;
}

impl<'a, T> GetAsset<Option<&'a T>> for &'a [Option<&'a T>] {
    fn get_asset(&self, id: i32) -> Option<&'a T> {
        self.get(usize::try_from(id).ok()?)?.clone()
    }
}

impl<'a> GetAsset<String> for &'a [UStr] {
    fn get_asset(&self, id: i32) -> String {
        // it's ok to ignore errors here because it's invalid UTF-16 that'll get caught elsewhere
        usize::try_from(id).ok().and_then(|id| self.get(id)).and_then(|s| s.try_decode_opt()).unwrap_or(String::new())
    }
}

fn open_file(path: &std::path::Path) -> Result<BufWriter<File>> {
    Ok(BufWriter::new(File::create(path)?))
}

unsafe fn save_frame(frame: &Frame, path: &std::path::Path) -> Result<()> {
    let f = open_file(path)?;
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

unsafe fn save_stream(data: &delphi::TStream, path: &std::path::Path) -> Result<()> {
    let old_pos = data.get_pos();
    data.set_pos(0);
    let len = data.get_size() as usize;
    let mut s = Vec::with_capacity(len);
    s.set_len(len);
    data.read(s.as_mut_ptr(), len as _);
    data.set_pos(old_pos);
    std::fs::write(path, s)?;
    Ok(())
}

unsafe fn save_sound(sound: &Sound, path: &mut PathBuf) -> Result<()> {
    let extension = sound.extension.try_decode()?;
    path.set_extension(extension.trim_matches('.'));
    if let Some(data) = sound.data.as_ref() {
        save_stream(data, &path)?;
    }
    path.set_extension("txt");
    let mut f = open_file(&path)?;
    writeln!(f, "extension={}", extension)?;
    writeln!(f, "kind={}", sound.kind)?;
    writeln!(f, "effects={}", sound.effects)?;
    writeln!(f, "volume={}", sound.volume)?;
    writeln!(f, "pan={}", sound.pan)?;
    writeln!(f, "preload={}", u8::from(sound.preload))?;
    Ok(())
}

unsafe fn save_sprite(sprite: &Sprite, path: &mut PathBuf) -> Result<()> {
    std::fs::create_dir_all(&path)?;
    let frames = slice::from_raw_parts(sprite.frames, sprite.frame_count as _);
    for (i, frame) in frames.iter().enumerate() {
        path.push(format!("{}.png", i));
        save_frame(&**frame, path)?;
        path.pop();
    }
    path.push("sprite.txt");
    let mut f = open_file(&path)?;
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

unsafe fn save_background(back: &Background, path: &mut PathBuf) -> Result<()> {
    path.set_extension("png");
    let frame = &*back.frame;
    if frame.width != 0 && frame.height != 0 {
        save_frame(frame, &path)?;
    }
    path.set_extension("txt");
    let mut f = open_file(path)?;
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

unsafe fn save_path(path: &Path, file_path: &mut PathBuf) -> Result<()> {
    std::fs::create_dir_all(&file_path)?;
    file_path.push("path.txt");
    let mut f = open_file(&file_path)?;
    writeln!(f, "connection={}", path.connection)?;
    writeln!(f, "closed={}", path.closed as u8)?;
    writeln!(f, "precision={}", path.precision)?;
    writeln!(f, "background={}", ide::get_room_names().get_asset(path.path_editor_room_background))?;
    writeln!(f, "snap_x={}", path.snap_x)?;
    writeln!(f, "snap_y={}", path.snap_y)?;
    file_path.pop();
    file_path.push("points.txt");
    let mut f = open_file(&file_path)?;
    let points = slice::from_raw_parts(path.points, path.point_count as _);
    for p in points {
        writeln!(f, "{},{},{}", p.x, p.y, p.speed)?;
    }
    file_path.pop();
    Ok(())
}

unsafe fn save_script(script: &Script, path: &mut PathBuf) -> Result<()> {
    path.set_extension("gml");
    std::fs::write(path, script.source.try_decode()?)?;
    Ok(())
}

unsafe fn save_font(font: &Font, path: &mut PathBuf) -> Result<()> {
    path.set_extension("txt");
    let mut f = open_file(path)?;
    writeln!(f, "name={}", font.sys_name.try_decode()?)?;
    writeln!(f, "size={}", font.size)?;
    writeln!(f, "bold={}", font.bold as u8)?;
    writeln!(f, "italic={}", font.italic as u8)?;
    writeln!(f, "charset={}", font.charset)?;
    writeln!(f, "aa_level={}", font.aa_level)?;
    writeln!(f, "range_start={}", font.range_start)?;
    writeln!(f, "range_end={}", font.range_end)?;
    Ok(())
}

const ACTION_TOKEN: &str = "/*\"/*'/**//* YYD ACTION";

unsafe fn save_event<F: Write>(ev: &Event, name: &str, file: &mut F) -> Result<()> {
    writeln!(file, "#define {}", name)?;
    let actions = slice::from_raw_parts(ev.actions, ev.action_count as usize);
    for action in actions {
        let action = &**action;
        writeln!(file, "{}", ACTION_TOKEN)?;
        writeln!(file, "lib_id={}", action.lib_id)?;
        writeln!(file, "action_id={}", action.id)?;
        writeln!(file, "kind={}", action.action_kind)?;
        match action.action_kind {
            0 => {
                // normal
                if action.can_be_relative {
                    writeln!(file, "relative={}", u8::from(action.is_relative))?;
                }
                if action.applies_to_something {
                    writeln!(file, "applies_to={}", action.applies_to)?;
                }
                writeln!(file, "invert={}", u8::from(action.invert_condition))?;
                for i in 0..action.param_count as usize {
                    // params can have newlines so delimit
                    writeln!(file, "arg{}={}", i, action.param_strings[i].try_delimit()?)?;
                }
            },
            5 => {
                // repeat
                writeln!(file, "repeats={}", action.param_strings[0].try_decode()?)?;
            },
            6 => {
                // variable
                writeln!(file, "var_name={}", action.param_strings[0].try_decode()?)?;
                writeln!(file, "var_value={}", action.param_strings[1].try_decode()?)?;
                if action.can_be_relative {
                    writeln!(file, "relative={}", u8::from(action.is_relative))?;
                }
                if action.applies_to_something {
                    writeln!(file, "applies_to={}", action.applies_to)?;
                }
            },
            7 => {
                // code
                if action.applies_to_something {
                    writeln!(file, "applies_to={}", action.applies_to)?;
                }
            },
            _ => (),
        }
        writeln!(file, "*/")?;
        if action.action_kind == 7 {
            // code
            let code = action.param_strings[0].try_decode()?;
            write!(file, "{}", &code)?;
            if !code.ends_with('\n') {
                writeln!(file)?;
            }
        }
    }
    Ok(())
}

unsafe fn save_timeline(tl: &Timeline, path: &mut PathBuf) -> Result<()> {
    path.set_extension("gml");
    let mut f = open_file(path)?;
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
            ide::get_triggers().get_asset(ev_numb as _).and_then(|t| t.name.try_decode_opt()).unwrap_or("".into())
        ),
        _ => format!("{}_{}", events::EVENT_ID_TO_NAME[ev_type], ev_numb),
    }
}

unsafe fn save_object(obj: &Object, path: &mut PathBuf) -> Result<()> {
    path.set_extension("txt");
    {
        let mut f = open_file(&path)?;
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
        let mut f = open_file(&path)?;
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

unsafe fn save_tiles(tiles: &[Tile], path: &mut PathBuf) -> Result<()> {
    let mut layers = HashMap::new();
    for tile in tiles {
        let f = match layers.entry(tile.depth) {
            std::collections::hash_map::Entry::Occupied(e) => e.into_mut(),
            std::collections::hash_map::Entry::Vacant(e) => {
                path.push(format!("{}.txt", tile.depth));
                let f = e.insert(open_file(&path)?);
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
    let mut f = open_file(&path)?;
    for depth in layers.keys() {
        writeln!(f, "{}", depth)?;
    }
    path.pop();
    Ok(())
}

fn save_instances(instances: &[Instance], path: &mut PathBuf) -> Result<()> {
    path.push("instances.txt");
    let mut f = open_file(&path)?;
    path.pop();
    let mut codes = HashSet::with_capacity(instances.len());
    for instance in instances {
        let code = instance.creation_code.try_decode()?;
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
                std::fs::write(&path, code)?;
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

unsafe fn save_room(room: &Room, path: &mut PathBuf) -> Result<()> {
    std::fs::create_dir_all(&path)?;
    path.push("room.txt");
    {
        let mut f = open_file(&path)?;
        writeln!(f, "caption={}", room.caption.try_decode()?)?;
        writeln!(f, "width={}", room.width)?;
        writeln!(f, "height={}", room.height)?;
        writeln!(f, "snap_x={}", room.snap_x)?;
        writeln!(f, "snap_y={}", room.snap_y)?;
        writeln!(f, "isometric={}", u8::from(room.isometric))?;
        writeln!(f, "roomspeed={}", room.speed)?;
        writeln!(f, "roompersistent={}", u8::from(room.persistent))?;
        writeln!(f, "bg_color={}", room.bg_colour)?;
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
    std::fs::write(&path, room.creation_code.try_decode()?)?;
    path.pop();

    let tiles = slice::from_raw_parts(room.tiles, room.tile_count as usize);
    save_tiles(tiles, path)?;

    let instances = slice::from_raw_parts(room.instances, room.instance_count as usize);
    save_instances(instances, path)?;
    Ok(())
}

unsafe fn save_constants(path: &mut PathBuf) -> Result<()> {
    path.push("constants.txt");
    let mut f = open_file(&path)?;
    path.pop();
    let names = ide::get_constant_names();
    let values = ide::get_constants();
    for (name, value) in names.iter().zip(values) {
        writeln!(f, "{}={}", name.try_decode()?, value.try_decode()?)?;
    }
    Ok(())
}

unsafe fn save_settings(path: &mut PathBuf) -> Result<()> {
    use ide::settings::*;
    path.push("settings");
    std::fs::create_dir_all(&path)?;
    save_constants(path)?;
    path.push("information.txt");
    std::fs::write(&path, (&*ide::settings::INFO_INFORMATION).try_decode()?)?;
    path.pop();
    path.push("settings.txt");
    let mut f = open_file(&path)?;
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
    let mut f = open_file(&path)?;
    for (extension, &loaded) in extensions.iter().zip(extensions_loaded) {
        if loaded {
            writeln!(f, "{}", &(&**extension).name.try_decode()?)?;
        }
    }
    path.pop();
    save_game_information(path)?;
    path.pop();
    Ok(())
}

unsafe fn save_triggers(path: &mut PathBuf) -> Result<()> {
    path.push("triggers");
    std::fs::create_dir_all(&path)?;
    path.push("index.yyd");
    let mut index = open_file(&path)?;
    path.pop();
    let triggers = ide::get_triggers();
    for trigger in triggers {
        if let Some(trigger) = trigger.as_ref() {
            let name = trigger.name.try_decode()?;
            path.push(&name);
            path.set_extension("txt");
            let mut f = open_file(&path)?;
            writeln!(f, "constant={}", trigger.constant_name.try_decode()?)?;
            writeln!(f, "kind={}", trigger.kind)?;
            path.set_extension("gml");
            std::fs::write(&path, trigger.condition.try_decode()?)?;
            path.pop();
            writeln!(index, "{}", name)?;
        }
    }
    path.pop();
    Ok(())
}

unsafe fn save_assets<'a, T: Sync>(
    _bar_start: u32,
    _bar_end: u32,
    name: &str,
    assets: &[Option<&'a T>],
    names: &[UStr],
    tree: *const *const TTreeNode,
    save_func: unsafe fn(&T, &mut PathBuf) -> Result<()>,
    path: &mut PathBuf,
) -> Result<()> {
    path.push(name);
    std::fs::create_dir_all(&path)?;
    path.push("index.yyd");
    let mut index = open_file(&path)?;
    path.pop();
    for name in names {
        let name = name.try_decode()?;
        writeln!(index, "{}", name)?;
    }
    (assets, names).into_par_iter().try_for_each(|(asset, name)| -> Result<()> {
        if let Some(asset) = asset {
            let name = name.try_decode()?;
            let mut p = path.join(name);
            save_func(asset, &mut p)?;
        }
        Ok(())
    })?;
    path.push("tree.yyd");
    if let Some(tree) = tree.as_ref() {
        write_tree_children(&**tree, &mut String::new(), &mut open_file(&path)?)?;
    }
    path.pop();
    path.pop();
    Ok(())
}

unsafe fn save_included_files(path: &mut PathBuf) -> Result<()> {
    path.push("datafiles");
    path.push("include");
    std::fs::create_dir_all(&path)?;
    path.pop();
    path.push("index.txt");
    let mut index = open_file(&path)?;
    path.pop();
    let files = ide::get_included_files();
    for file in files {
        let file = &**file;
        let mut name = file.file_name.try_decode()?;
        writeln!(index, "{}", &name)?;
        if file.data_exists && file.stored_in_gmk {
            if let Some(stream) = file.data.as_ref() {
                path.push("include");
                path.push(&name);
                save_stream(stream, &path)?;
                path.pop();
                path.pop();
            }
        }
        name += ".txt";
        path.push(&name);
        let mut f = open_file(&path)?;
        writeln!(f, "store={}", u8::from(file.stored_in_gmk))?;
        writeln!(f, "free={}", u8::from(file.free_memory))?;
        writeln!(f, "overwrite={}", u8::from(file.overwrite_file))?;
        writeln!(f, "remove={}", u8::from(file.remove_at_end))?;
        writeln!(f, "export={}", file.export_setting)?;
        if file.export_setting == 3 {
            writeln!(f, "export_folder={}", file.export_custom_folder.try_decode()?)?;
        }
    }
    path.pop();
    Ok(())
}

unsafe fn save_game_information(path: &mut PathBuf) -> Result<()> {
    use ide::game_info::*;
    path.push("game_information.txt");
    let mut f = open_file(&path)?;
    let editor = &*(&**FORM).editor;
    writeln!(f, "color={}", editor.colour)?;
    writeln!(f, "new_window={}", u8::from(*NEW_WINDOW))?;
    writeln!(f, "caption={}", (&*CAPTION).try_decode()?)?;
    writeln!(f, "left={}", *LEFT)?;
    writeln!(f, "top={}", *TOP)?;
    writeln!(f, "width={}", *WIDTH)?;
    writeln!(f, "height={}", *HEIGHT)?;
    writeln!(f, "border={}", u8::from(*BORDER))?;
    writeln!(f, "resizable={}", u8::from(*RESIZABLE))?;
    writeln!(f, "window_on_top={}", u8::from(*WINDOW_ON_TOP))?;
    writeln!(f, "freeze_game={}", u8::from(*FREEZE_GAME))?;
    path.set_extension("rtf");
    (&*editor.rich_edit_strings).SaveToFile(&UStr::new(path.as_ref()));
    path.pop();
    Ok(())
}

unsafe fn write_tree_children<F: Write>(parent: &delphi::TTreeNode, tabs: &mut String, f: &mut F) -> Result<()> {
    for i in 0..parent.GetCount() {
        let node = &*parent.GetItem(i);
        let name = node.name.try_decode()?;
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

pub unsafe fn save_gmk(mut path: PathBuf) -> Result<()> {
    {
        // some stuff to go in the main gmk
        let mut f = open_file(&path)?;
        writeln!(f, "gameid={}", ide::GAME_ID.read())?;
        writeln!(f)?;
        writeln!(f, "info_author={}", (&*ide::settings::INFO_AUTHOR).try_decode()?)?;
        writeln!(f, "info_version={}", (&*ide::settings::INFO_VERSION).try_decode()?)?;
        writeln!(f, "info_timestamp={}", delphi::Now())?;
        writeln!(f, "info_information={}", (&*ide::settings::INFO_INFORMATION).try_delimit()?)?;
        writeln!(f)?;
        writeln!(f, "exe_company={}", (&*ide::settings::EXE_COMPANY).try_decode()?)?;
        writeln!(f, "exe_product={}", (&*ide::settings::EXE_PRODUCT).try_decode()?)?;
        writeln!(f, "exe_copyright={}", (&*ide::settings::EXE_COPYRIGHT).try_decode()?)?;
        writeln!(f, "exe_description={}", (&*ide::settings::EXE_DESCRIPTION).try_decode()?)?;
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
    save_settings(&mut path)?;
    advance_progress_form(10);
    save_triggers(&mut path)?;
    advance_progress_form(15);
    save_assets(15, 30, "sounds", ide::get_sounds(), ide::get_sound_names(), ide::RT_SOUNDS, save_sound, &mut path)?;
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
    )?;
    advance_progress_form(65);
    save_assets(65, 70, "paths", ide::get_paths(), ide::get_path_names(), ide::RT_PATHS, save_path, &mut path)?;
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
    )?;
    advance_progress_form(75);
    save_assets(75, 80, "fonts", ide::get_fonts(), ide::get_font_names(), ide::RT_FONTS, save_font, &mut path)?;
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
    )?;
    advance_progress_form(90);
    save_assets(90, 95, "rooms", ide::get_rooms(), ide::get_room_names(), ide::RT_ROOMS, save_room, &mut path)?;
    advance_progress_form(95);
    save_included_files(&mut path)?;

    advance_progress_form(100);
    Ok(())
}
