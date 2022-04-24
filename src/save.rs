use crate::{
    asset::*,
    delphi,
    delphi::{advance_progress_form, TTreeNode, UStr},
    events, ide, run_while_updating_bar, show_message, update_timestamp, Error, InstanceExtra, Result, TileExtra,
    ACTION_TOKEN, EXTRA_DATA, SAVING_FOR_ROOM_EDITOR, SAW_APPLIES_TO_WARNING,
};
use itertools::Itertools;
use rayon::prelude::*;
use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
    slice, str,
};

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
        *self.get(usize::try_from(id).ok()?)?
    }
}

impl<'a> GetAsset<String> for &'a [UStr] {
    fn get_asset(&self, id: i32) -> String {
        // it's ok to ignore errors here because it's invalid UTF-16 that'll get caught elsewhere
        usize::try_from(id).ok().and_then(|id| self.get(id)).and_then(|s| s.try_decode_opt()).unwrap_or_default()
    }
}

fn filename_invalid(s: &str) -> Option<u8> {
    if s == "." || s == ".." || s.as_bytes().last().copied() == Some(b'.') {
        return Some(b'.')
    }
    for c in b"<>:\"/\\|?*" {
        if s.as_bytes().contains(c) {
            return Some(*c)
        }
    }
    return None
}

fn make_unicase(s: String, u: &UStr) -> unicase::UniCase<String> {
    // implement my own ascii check because it's Faster
    if s.len() == u.len() {
        // nothing got expanded, so as far as we're concerned it's ascii
        // windows isn't case insensitive for some of the wackier characters anyway
        unicase::UniCase::ascii(s)
    } else {
        unicase::UniCase::unicode(s)
    }
}

fn create_dirs(path: &std::path::Path) -> Result<()> {
    std::fs::create_dir_all(path).map_err(|e| Error::DirIoError(e, path.to_path_buf()))
}

fn open_file(path: &std::path::Path) -> Result<BufWriter<File>> {
    Ok(BufWriter::new(File::create(path).map_err(|e| Error::FileIoError(e, path.to_path_buf()))?))
}

fn write_file(path: &std::path::Path, content: impl AsRef<[u8]>) -> Result<()> {
    std::fs::write(path, content).map_err(|e| Error::FileIoError(e, path.to_path_buf()))
}

fn write_gml<F: Write>(f: &mut F, code: &UStr) -> Result<()> {
    for line in code.try_decode()?.trim_end().lines() {
        writeln!(f, "{}", line.trim_end())?;
    }
    Ok(())
}

fn save_gml(path: &std::path::Path, code: &UStr) -> Result<()> {
    let mut f = open_file(path)?;
    write_gml(&mut f, code)?;
    f.flush()?;
    Ok(())
}

unsafe fn save_frame(frame: &Frame, path: &std::path::Path) -> Result<()> {
    let err = |_| Error::Other(format!("failed to save frame {}", path.to_string_lossy()));
    // set up encoder
    let mut f = open_file(path)?;
    let mut encoder = png::Encoder::new(&mut f, frame.width, frame.height);
    encoder.set_color(png::ColorType::Rgba);
    encoder.set_filter(png::FilterType::NoFilter);
    let mut writer = encoder.write_header().map_err(err)?;
    // BGRA8 -> RGBA8
    let mut pixels = slice::from_raw_parts(frame.data, frame.width as usize * frame.height as usize * 4).to_vec();
    pixels.par_chunks_exact_mut(4).for_each(|px| px.swap(0, 2));
    // save
    writer.write_image_data(&pixels).map_err(err)?;
    drop(writer);
    f.flush()?;
    Ok(())
}

unsafe fn save_stream(data: &delphi::TMemoryStream, path: &std::path::Path) -> Result<()> {
    let old_pos = data.get_pos();
    data.set_pos(0);
    let len = data.get_size() as usize;
    let mut s = Vec::with_capacity(len);
    s.set_len(len);
    data.read(s.as_mut_ptr(), len as _);
    data.set_pos(old_pos);
    write_file(path, s)?;
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
    writeln!(f, "exists={}", u8::from(!sound.data.is_null()))?;
    writeln!(f, "source={}", sound.source.try_decode()?)?;
    writeln!(f, "kind={}", sound.kind)?;
    writeln!(f, "effects={}", sound.effects)?;
    writeln!(f, "volume={}", sound.volume)?;
    writeln!(f, "pan={}", sound.pan)?;
    writeln!(f, "preload={}", u8::from(sound.preload))?;
    f.flush()?;
    Ok(())
}

unsafe fn save_sprite(sprite: &Sprite, path: &mut PathBuf) -> Result<()> {
    create_dirs(&path)?;
    for (i, frame) in sprite.get_frames().iter().enumerate() {
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
    writeln!(f, "bbox_top={}", sprite.bbox_top)?;
    writeln!(f, "bbox_right={}", sprite.bbox_right)?;
    writeln!(f, "bbox_bottom={}", sprite.bbox_bottom)?;
    f.flush()?;
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
    writeln!(f, "exists={}", u8::from(frame.width != 0 && frame.height != 0))?;
    writeln!(f, "tileset={}", back.is_tileset as u8)?;
    writeln!(f, "tile_width={}", back.tile_width)?;
    writeln!(f, "tile_height={}", back.tile_height)?;
    writeln!(f, "tile_hoffset={}", back.h_offset)?;
    writeln!(f, "tile_voffset={}", back.v_offset)?;
    writeln!(f, "tile_hsep={}", back.h_sep)?;
    writeln!(f, "tile_vsep={}", back.v_sep)?;
    f.flush()?;
    Ok(())
}

unsafe fn save_path(path: &Path, file_path: &mut PathBuf) -> Result<()> {
    create_dirs(&file_path)?;
    file_path.push("path.txt");
    let mut f = open_file(&file_path)?;
    writeln!(f, "connection={}", path.connection)?;
    writeln!(f, "closed={}", path.closed as u8)?;
    writeln!(f, "precision={}", path.precision)?;
    writeln!(f, "background={}", ide::get_room_names().get_asset(path.path_editor_room_background))?;
    writeln!(f, "snap_x={}", path.snap_x)?;
    writeln!(f, "snap_y={}", path.snap_y)?;
    f.flush()?;
    file_path.pop();
    file_path.push("points.txt");
    let mut f = open_file(&file_path)?;
    for p in path.get_points() {
        writeln!(f, "{},{},{}", p.x, p.y, p.speed)?;
    }
    f.flush()?;
    file_path.pop();
    Ok(())
}

unsafe fn save_script(script: &Script, path: &mut PathBuf) -> Result<()> {
    path.set_extension("gml");
    save_gml(&path, &script.source)?;
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
    writeln!(f, "aa_level={}", font.aa_level)?; // DOES NOT CORRESPOND TO .GMK OR .EXE
    writeln!(f, "range_start={}", font.range_start)?;
    writeln!(f, "range_end={}", font.range_end)?;
    f.flush()?;
    Ok(())
}

unsafe fn save_event<F: Write>(ev: &Event, name: &str, file: &mut F) -> Result<()> {
    writeln!(file, "#define {}", name)?;
    for action in ev.get_actions() {
        let action = &**action;
        writeln!(file, "{}", ACTION_TOKEN)?;
        writeln!(file, "lib_id={}", action.lib_id)?;
        writeln!(file, "action_id={}", action.id)?;
        if action.can_be_relative {
            writeln!(file, "relative={}", u8::from(action.is_relative))?;
        }
        if action.applies_to_something {
            match action.applies_to {
                -2 => writeln!(file, "applies_to=other")?,
                -1 => writeln!(file, "applies_to=self")?,
                i => {
                    if i >= 0 && ide::get_objects().get_asset(i).is_none() && !SAW_APPLIES_TO_WARNING {
                        show_message(
                            "WARNING: Project contains actions that apply to an object that has been deleted. \
                            These will do absolutely nothing when executed. \
                            You may want to find them and make sure nothing is broken. \
                            You can find them by searching the project for \"apply_to\\n\" with Notepad++.",
                        );
                        SAW_APPLIES_TO_WARNING = true;
                    }
                    writeln!(file, "applies_to={}", ide::get_object_names().get_asset(i))?
                },
            }
        }
        match action.action_kind {
            0 => {
                // normal
                writeln!(file, "invert={}", u8::from(action.invert_condition))?;
                for i in 0..action.param_count as usize {
                    writeln!(file, "arg{}={}", i, match action.param_types[i] {
                        5 => ide::get_sprite_names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        6 => ide::get_sound_names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        7 => ide::get_background_names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        8 => ide::get_path_names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        9 => ide::get_script_names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        10 => ide::get_object_names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        11 => ide::get_room_names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        12 => ide::get_font_names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        14 => ide::get_timeline_names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        // params can have newlines so delimit
                        _ => action.param_strings[i].try_delimit()?,
                    })?;
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
            },
            _ => (),
        }
        writeln!(file, "*/")?;
        if action.action_kind == 7 {
            // code
            write_gml(file, &action.param_strings[0])?;
        }
    }
    Ok(())
}

unsafe fn save_timeline(tl: &Timeline, path: &mut PathBuf) -> Result<()> {
    path.set_extension("gml");
    let mut f = open_file(path)?;
    for (time, event) in tl.get_times().iter().zip(tl.get_events()) {
        let event = &**event;
        if event.action_count != 0 {
            save_event(event, &time.to_string(), &mut f)?;
        }
    }
    f.flush()?;
    Ok(())
}

unsafe fn event_name(ev_type: usize, ev_numb: usize) -> String {
    match ev_type {
        events::EV_COLLISION => {
            format!("{}_{}", events::EVENT_NAMES[ev_type], ide::get_object_names().get_asset(ev_numb as _))
        },
        events::EV_TRIGGER => format!(
            "{}_{}",
            events::EVENT_NAMES[ev_type],
            ide::get_triggers().get_asset(ev_numb as _).and_then(|t| t.name.try_decode_opt()).unwrap_or_default()
        ),
        _ => format!("{}_{}", events::EVENT_NAMES[ev_type], ev_numb),
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
        f.flush()?;
    }
    path.set_extension("gml");
    {
        let mut f = open_file(&path)?;
        for (ev_type, event_group) in obj.events.iter().enumerate() {
            for (ev_numb, ev) in event_group.iter().enumerate() {
                let ev = &**ev; // all events in the array are non-null
                if ev.action_count != 0 {
                    let name = event_name(ev_type, ev_numb);
                    save_event(ev, &name, &mut f)?;
                }
            }
        }
        f.flush()?;
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
        let TileExtra { xscale, yscale, blend } =
            EXTRA_DATA.as_ref().and_then(|e| e.1.get(&tile.id).cloned()).unwrap_or_default();
        writeln!(
            f,
            "{},{},{},{},{},{},{},{},{},{},{}",
            ide::get_background_names().get_asset(tile.source_bg),
            tile.x,
            tile.y,
            tile.u,
            tile.v,
            tile.width,
            tile.height,
            u8::from(tile.locked),
            xscale,
            yscale,
            blend,
        )?;
    }
    layers.values_mut().try_for_each(Write::flush)?;
    path.push("layers.txt");
    let mut f = open_file(&path)?;
    for depth in layers.keys().sorted() {
        writeln!(f, "{}", depth)?;
    }
    f.flush()?;
    path.pop();
    Ok(())
}

fn save_instances(instances: &[Instance], path: &mut PathBuf) -> Result<()> {
    path.push("instances.txt");
    let mut f = open_file(&path)?;
    path.pop();
    let mut codes = HashMap::with_capacity(instances.len());
    for instance in instances {
        // pre-process the code so the hash stays consistent
        let mut code = Vec::with_capacity(instance.creation_code.len());
        write_gml(&mut code, &instance.creation_code)?;
        let mut hash_hex = [0; 8];
        let fname = if !code.is_empty() {
            let hash = crc::crc32::checksum_ieee(&code);
            for i in 0..8 {
                hash_hex[7 - i] = match (hash >> (i * 4)) & 0xf {
                    i if i < 10 => b'0' + i as u8,
                    i => b'A' - 10 + i as u8,
                };
            }
            let mut fname = unsafe { str::from_utf8_unchecked(&hash_hex).to_string() };
            if let Some(fname) = loop {
                if let Some(old_code) = codes.get(&fname) {
                    if &code == old_code {
                        // we already saved this code, no need to repeat
                        break None
                    } else {
                        // new code with hash collision
                        fname.push('_');
                    }
                } else {
                    // new code, safe hash
                    codes.insert(fname.to_string(), code.clone());
                    break Some(&fname)
                }
            } {
                path.push(&fname);
                path.set_extension("gml");
                write_file(&path, code)?;
                path.pop();
            }
            fname
        } else {
            String::new() // ""
        };
        let InstanceExtra { xscale, yscale, blend, angle } =
            unsafe { EXTRA_DATA.as_ref().and_then(|e| e.0.get(&instance.id).cloned()).unwrap_or_default() };
        writeln!(
            f,
            "{},{},{},{},{},{},{},{},{}",
            ide::get_object_names().get_asset(instance.object),
            instance.x,
            instance.y,
            fname,
            u8::from(instance.locked),
            xscale,
            yscale,
            blend,
            angle,
        )?;
    }
    f.flush()?;
    Ok(())
}

unsafe fn save_room(room: &Room, path: &mut PathBuf) -> Result<()> {
    let _: u32 = delphi_call!(0x6576fc, room); // clean unused assets
    create_dirs(&path)?;
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
            writeln!(f, "bg_visible{}={}", i, u8::from(bg.visible_on_start))?;
            writeln!(f, "bg_is_foreground{}={}", i, u8::from(bg.is_foreground))?;
            writeln!(f, "bg_source{}={}", i, ide::get_background_names().get_asset(bg.source_bg))?;
            writeln!(f, "bg_xoffset{}={}", i, bg.xoffset)?;
            writeln!(f, "bg_yoffset{}={}", i, bg.yoffset)?;
            writeln!(f, "bg_tile_h{}={}", i, u8::from(bg.tile_horz))?;
            writeln!(f, "bg_tile_v{}={}", i, u8::from(bg.tile_vert))?;
            writeln!(f, "bg_hspeed{}={}", i, bg.hspeed)?;
            writeln!(f, "bg_vspeed{}={}", i, bg.vspeed)?;
            writeln!(f, "bg_stretch{}={}", i, u8::from(bg.stretch))?;
        }
        writeln!(f)?;
        writeln!(f, "views_enabled={}", u8::from(room.views_enabled))?;
        for (i, view) in room.views.iter().enumerate() {
            writeln!(f, "view_visible{}={}", i, u8::from(view.visible))?;
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
        f.flush()?;
    }
    path.pop();
    {
        path.push("code.gml");
        save_gml(&path, &room.creation_code)?;
        path.pop();
    }

    save_tiles(room.get_tiles(), path)?;

    save_instances(room.get_instances(), path)?;
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
    f.flush()?;
    Ok(())
}

unsafe fn save_settings(path: &mut PathBuf) -> Result<()> {
    use ide::settings::*;
    path.push("settings");
    create_dirs(&path)?;
    save_constants(path)?;
    // not the usual behaviour, but i don't feel like adding more flags than necessary
    if *HAS_CUSTOM_LOAD_IMAGE && CUSTOM_LOAD_IMAGE.read().is_null() {
        HAS_CUSTOM_LOAD_IMAGE.write(false);
    }
    {
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
        writeln!(f, "swap_creation_events={}", u8::from(*VSYNC_AND_FORCE_CPU & (1 << 31) != 0))?;
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
        writeln!(f, "bar_has_bg={}", u8::from(!LOADING_BACKGROUND.read().is_null()))?;
        writeln!(f, "bar_has_fg={}", u8::from(!LOADING_FOREGROUND.read().is_null()))?;
        writeln!(f, "transparent={}", u8::from(*LOADING_TRANSPARENT))?;
        writeln!(f, "translucency={}", *LOADING_TRANSLUCENCY)?;
        writeln!(f, "scale_progress_bar={}", u8::from(*LOADING_PROGRESS_BAR_SCALE))?;
        writeln!(f, "show_error_messages={}", u8::from(*SHOW_ERROR_MESSAGES))?;
        writeln!(f, "log_errors={}", u8::from(*LOG_ERRORS))?;
        writeln!(f, "always_abort={}", u8::from(*ALWAYS_ABORT))?;
        writeln!(f, "zero_uninitialized_vars={}", u8::from(*ZERO_UNINITIALIZED_VARS))?;
        writeln!(f, "error_on_uninitialized_args={}", u8::from(*ERROR_ON_UNINITIALIZED_ARGS))?;
        f.flush()?;
    }
    if LOADING_BAR.read() == 2 {
        if let Some(bg) = LOADING_BACKGROUND.read().as_ref() {
            path.push("back.bmp");
            bg.SaveToFile(&UStr::new(&path));
            path.pop();
        }
        if let Some(fg) = LOADING_FOREGROUND.read().as_ref() {
            path.push("front.bmp");
            fg.SaveToFile(&UStr::new(&path));
            path.pop();
        }
    }
    if HAS_CUSTOM_LOAD_IMAGE.read() {
        path.push("loader.bmp");
        (&*CUSTOM_LOAD_IMAGE.read()).SaveToFile(&UStr::new(&path));
        path.pop();
    }
    // icon is never legally null, so no need to check
    path.push("icon.ico");
    (&*ICON.read()).SaveToFile(&UStr::new(&path));
    path.pop();
    path.push("extensions.txt");
    let extensions = ide::get_extensions();
    let extensions_loaded = ide::get_extensions_loaded();
    {
        let mut f = open_file(&path)?;
        for (extension, &loaded) in extensions.iter().zip(extensions_loaded) {
            if loaded {
                writeln!(f, "{}", &(**extension).name.try_decode()?)?;
            }
        }
        f.flush()?;
    }
    path.pop();
    save_game_information(path)?;
    path.pop();
    Ok(())
}

unsafe fn save_triggers(path: &mut PathBuf) -> Result<()> {
    path.push("triggers");
    create_dirs(&path)?;
    let triggers = ide::get_triggers();
    {
        let mut index = Vec::with_capacity(triggers.len());
        let mut name_set = HashSet::with_capacity(triggers.len());
        for trigger in triggers {
            if let Some(trigger) = trigger.as_ref() {
                let name = trigger.name.try_decode()?;
                writeln!(index, "{}", name)?;
                if let Some(c) = filename_invalid(&name) {
                    return Err(Error::BadTriggerName(name, char::from(c)))
                }
                if !name_set.insert(make_unicase(name, &trigger.name)) {
                    return Err(Error::DuplicateTrigger(trigger.name.try_decode()?))
                }
            } else {
                writeln!(index)?;
            }
        }
        path.push("index.yyd");
        write_file(&path, index)?;
        path.pop();
    }
    for trigger in triggers {
        if let Some(trigger) = trigger.as_ref() {
            let name = trigger.name.try_decode()?;
            path.push(&name);
            path.set_extension("txt");
            {
                let mut f = open_file(&path)?;
                writeln!(f, "constant={}", trigger.constant_name.try_decode()?)?;
                writeln!(f, "kind={}", trigger.kind)?;
                f.flush()?;
            }
            path.set_extension("gml");
            save_gml(&path, &trigger.condition)?;
            path.pop();
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
    create_dirs(&path)?;
    let mut count = 0;
    {
        let mut name_set = HashSet::with_capacity(names.len());
        let mut index = Vec::with_capacity(names.len());
        for name_wide in names {
            let name = name_wide.try_decode()?;
            writeln!(&mut index, "{}", name)?;
            if !name.is_empty() {
                count += 1;
                if let Some(c) = filename_invalid(&name) {
                    return Err(Error::BadAssetName(name, char::from(c)))
                }
                if !name_set.insert(make_unicase(name, name_wide)) {
                    return Err(Error::DuplicateAsset(name_wide.try_decode()?))
                }
            }
        }
        path.push("index.yyd");
        write_file(&path, index)?;
        path.pop();
    }
    run_while_updating_bar(_bar_start, _bar_end, count, |tx| {
        (assets, names).into_par_iter().try_for_each(|(asset, name)| -> Result<()> {
            if let Some(asset) = asset {
                let name = name.try_decode()?;
                let mut p = path.join(name);
                save_func(asset, &mut p)?;
                let _ = tx.send(());
            }
            Ok(())
        })
    })?;
    path.push("tree.yyd");
    if let Some(tree) = tree.as_ref() {
        let mut f = open_file(&path)?;
        write_tree_children(&**tree, names, &mut String::new(), &mut f)?;
        f.flush()?;
    }
    path.pop();
    path.pop();
    Ok(())
}

unsafe fn save_included_files(path: &mut PathBuf) -> Result<()> {
    path.push("datafiles");
    path.push("include");
    create_dirs(&path)?;
    path.pop();
    let files = ide::get_included_files();
    {
        let mut index = Vec::with_capacity(files.len());
        let mut names_set = HashSet::with_capacity(files.len());
        for file in files {
            let name = file.file_name.try_decode()?;
            writeln!(index, "{}", name)?;

            if let Some(c) = filename_invalid(&name) {
                return Err(Error::BadIncludedFileName(name, char::from(c)))
            }
            if !names_set.insert(name) {
                return Err(Error::DuplicateIncludedFile(file.file_name.try_decode()?))
            }
        }
        path.push("index.yyd");
        write_file(&path, index)?;
        path.pop();
    }
    for file in files {
        let file = &**file;
        let mut name = file.file_name.try_decode()?;
        if file.data_exists {
            if file.stored_in_gmk {
                if let Some(stream) = file.data.as_ref() {
                    path.push("include");
                    path.push(&name);
                    save_stream(stream, &path)?;
                    path.pop();
                    path.pop();
                }
            } else {
                // try to copy it to gmk dir if not already done
                path.push("include");
                path.push(&name);
                if !path.exists() {
                    std::fs::copy(file.source_path.to_os_string(), &path)
                        .map_err(|e| Error::FileIoError(e, path.to_path_buf()))?;
                }
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
        f.flush()?;
        path.pop();
    }
    path.pop();
    Ok(())
}

unsafe fn save_game_information(path: &mut PathBuf) -> Result<()> {
    use ide::game_info::*;
    let editor = &*(**FORM).editor;
    path.push("game_information.txt");
    {
        let mut f = open_file(&path)?;
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
        f.flush()?;
    }
    path.set_extension("rtf");
    (&*editor.rich_edit_strings).SaveToFile(&UStr::new(&path));
    path.pop();
    Ok(())
}

unsafe fn write_tree_children<F: Write>(
    parent: &delphi::TTreeNode,
    names: &[UStr],
    tabs: &mut String,
    f: &mut F,
) -> Result<()> {
    for i in 0..parent.GetCount() {
        let node = &*parent.GetItem(i);
        let name = node.name.try_decode()?;
        match (*node.data).rtype {
            2 => {
                writeln!(f, "{}+{}", tabs, name)?;
                tabs.push('\t');
                drop(name);
                write_tree_children(node, names, tabs, f)?;
                tabs.pop();
            },
            3 => writeln!(f, "{}|{}", tabs, names[(*node.data).index].try_decode()?)?,
            _ => return Err(Error::Other(format!("failed to save resource tree {}", name))),
        }
    }
    Ok(())
}

unsafe fn save_icon_cache(path: &mut PathBuf) -> Result<()> {
    const BMP_HEADER: &[u8] = include_bytes!("../assets/thumb_header.dat");
    const BMP_SIZE: usize = 16 * 16 * 4 + BMP_HEADER.len();
    unsafe fn save_frame(frame: &Frame, name: &UStr, path: &std::path::Path) -> Result<()> {
        if frame.width == 0 || frame.height == 0 {
            return Ok(())
        }
        let mut out = Vec::with_capacity(BMP_SIZE);
        out.extend_from_slice(BMP_HEADER);
        out.set_len(BMP_SIZE);
        frame.thumb(&mut out[BMP_HEADER.len()..], true);
        let mut path = path.join(name.to_os_string());
        path.set_extension("bmp");
        write_file(&path, out)
    }
    path.push("cache");
    path.push("sprites");
    create_dirs(path)?;
    ide::get_sprites().par_iter().zip(ide::get_sprite_names()).try_for_each(|(sprite, name)| -> Result<()> {
        if let Some(&frame) = sprite.and_then(|s| s.get_frames().get(0)) {
            save_frame(&*frame, name, path)?;
        }
        Ok(())
    })?;
    path.pop();
    path.push("backgrounds");
    create_dirs(path)?;
    ide::get_backgrounds().par_iter().zip(ide::get_background_names()).try_for_each(|(bg, name)| -> Result<()> {
        if let Some(bg) = bg {
            save_frame(&*bg.frame, name, path)?;
        }
        Ok(())
    })?;
    path.pop();
    path.pop();
    Ok(())
}

pub unsafe fn save_gmk(mut path: PathBuf) -> Result<()> {
    {
        create_dirs(path.parent().unwrap())?;
        // some stuff to go in the main gmk
        let mut f = open_file(&path)?;
        writeln!(f, "gm82_version=3")?;
        writeln!(f, "gameid={}", ide::GAME_ID.read())?;
        writeln!(f)?;
        writeln!(f, "info_author={}", (&*ide::settings::INFO_AUTHOR).try_decode()?)?;
        writeln!(f, "info_version={}", (&*ide::settings::INFO_VERSION).try_decode()?)?;
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
        f.flush()?;
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

    if SAVING_FOR_ROOM_EDITOR {
        save_icon_cache(&mut path)?;
    }

    advance_progress_form(100);

    update_timestamp();

    Ok(())
}
