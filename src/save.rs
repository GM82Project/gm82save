use crate::{
    asset::*,
    delphi,
    delphi::{advance_progress_form, DelphiBox, TTreeNode, UStr},
    events, ide,
    ide::AssetListTrait,
    regular::project_watcher,
    run_while_updating_bar, show_message, update_timestamp, Error, GMLLines, InstanceExtra, Result, TileExtra,
    ACTION_TOKEN, EXTRA_DATA, LAST_SAVE, PATH_FORM_UPDATED, SAW_APPLIES_TO_WARNING,
};
use itertools::Itertools;
use png::Compression;
use rayon::prelude::*;
use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
    str,
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

pub trait GetAsset<T> {
    fn get_asset(&self, id: i32) -> T;
}

impl<'a, T> GetAsset<Option<&'a T>> for &'a [Option<DelphiBox<T>>] {
    fn get_asset(&self, id: i32) -> Option<&'a T> {
        self.get(usize::try_from(id).ok()?)?.as_deref()
    }
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

impl<'a> GetAsset<f64> for &'a [f64] {
    fn get_asset(&self, id: i32) -> f64 {
        usize::try_from(id).ok().and_then(|id| self.get(id).copied()).unwrap_or_default()
    }
}

fn filename_invalid(s: &str) -> Option<u8> {
    if s == "." || s == ".." || s.as_bytes().last().copied() == Some(b'.') {
        return Some(b'.')
    }
    if s.trim().is_empty() {
        return Some(b' ')
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
    for line in GMLLines::new(code.try_decode()?.trim_end().lines()) {
        writeln!(f, "{}", line)?;
    }
    Ok(())
}

fn save_gml(path: &std::path::Path, code: &UStr) -> Result<()> {
    let mut f = open_file(path)?;
    write_gml(&mut f, code)?;
    f.flush()?;
    Ok(())
}

fn save_frame(frame: &Frame, path: &std::path::Path) -> Result<()> {
    let err = |_| Error::Other(format!("failed to save frame {}", path.to_string_lossy()));
    // set up encoder
    let mut f = open_file(path)?;
    let mut encoder = png::Encoder::new(&mut f, frame.width, frame.height);
    encoder.set_compression(Compression::Default);
    encoder.set_color(png::ColorType::Rgba);
    encoder.set_filter(png::FilterType::NoFilter);
    let mut writer = encoder.write_header().map_err(err)?;
    // BGRA8 -> RGBA8
    let mut pixels = frame.get_data().to_vec();
    pixels.par_chunks_exact_mut(4).for_each(|px| px.swap(0, 2));
    // save
    writer.write_image_data(&pixels).map_err(err)?;
    drop(writer);
    f.flush()?;
    Ok(())
}

fn no_dependencies<T>(_: &T) -> bool {
    false
}

fn save_sound(sound: &Sound, path: &mut PathBuf) -> Result<()> {
    let extension = sound.extension.try_decode()?;
    path.set_extension(extension.trim_matches('.'));
    if let Some(data) = sound.data.as_ref() {
        write_file(&path, data.get_slice())?;
    }
    path.set_extension("txt");
    let mut f = open_file(&path)?;
    writeln!(f, "extension={}", extension)?;
    writeln!(f, "exists={}", u8::from(!sound.data.is_none()))?;
    writeln!(f, "source={}", sound.source.try_decode()?)?;
    writeln!(f, "kind={}", sound.kind)?;
    writeln!(f, "effects={}", sound.effects)?;
    writeln!(f, "volume={}", sound.volume)?;
    writeln!(f, "pan={}", sound.pan)?;
    writeln!(f, "preload={}", u8::from(sound.preload))?;
    f.flush()?;
    Ok(())
}

fn save_sprite(sprite: &Sprite, path: &mut PathBuf) -> Result<()> {
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

fn save_background(back: &Background, path: &mut PathBuf) -> Result<()> {
    path.set_extension("png");
    let frame = &back.frame;
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

unsafe fn path_needs_update(path: &Path) -> bool {
    PATH_FORM_UPDATED
        || path.path_editor_room_background >= 0
            && (ide::ROOMS.assets().get_asset(path.path_editor_room_background).is_none()
                || ide::ROOMS.timestamps().get_asset(path.path_editor_room_background) > LAST_SAVE)
}

fn save_path(path: &Path, file_path: &mut PathBuf) -> Result<()> {
    create_dirs(&file_path)?;
    file_path.push("path.txt");
    let mut f = open_file(&file_path)?;
    writeln!(f, "connection={}", path.connection)?;
    writeln!(f, "closed={}", path.closed as u8)?;
    writeln!(f, "precision={}", path.precision)?;
    writeln!(f, "background={}", ide::ROOMS.names().get_asset(path.path_editor_room_background))?;
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

fn save_script(script: &Script, path: &mut PathBuf) -> Result<()> {
    path.set_extension("gml");
    save_gml(&path, &script.source)?;
    Ok(())
}

fn save_font(font: &Font, path: &mut PathBuf) -> Result<()> {
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

unsafe fn event_needs_update(ev: &Event) -> bool {
    for action in ev.get_actions() {
        if ide::OBJECTS.timestamps().get_asset(action.applies_to) > LAST_SAVE {
            return true
        }
        if action.action_kind == 0 {
            for (ty, val) in action.param_types.iter().zip(&action.param_strings) {
                fn parameter_get_timestamp(ty: u32, val: &UStr) -> Result<f64> {
                    let val = val.try_decode()?.parse()?;
                    if val < 0 {
                        return Ok(0.0)
                    }
                    let time = match ty {
                        5 => {
                            if ide::SPRITES.assets().get_asset(val).is_none() {
                                return Ok(f64::MAX)
                            }
                            ide::SPRITES.timestamps().get_asset(val)
                        },
                        6 => {
                            if ide::SOUNDS.assets().get_asset(val).is_none() {
                                return Ok(f64::MAX)
                            }
                            ide::SOUNDS.timestamps().get_asset(val)
                        },
                        7 => {
                            if ide::BACKGROUNDS.assets().get_asset(val).is_none() {
                                return Ok(f64::MAX)
                            }
                            ide::BACKGROUNDS.timestamps().get_asset(val)
                        },
                        8 => {
                            if ide::PATHS.assets().get_asset(val).is_none() {
                                return Ok(f64::MAX)
                            }
                            ide::PATHS.timestamps().get_asset(val)
                        },
                        9 => {
                            if ide::SCRIPTS.assets().get_asset(val).is_none() {
                                return Ok(f64::MAX)
                            }
                            ide::SCRIPTS.timestamps().get_asset(val)
                        },
                        10 => {
                            if ide::OBJECTS.assets().get_asset(val).is_none() {
                                return Ok(f64::MAX)
                            }
                            ide::OBJECTS.timestamps().get_asset(val)
                        },
                        11 => {
                            if ide::ROOMS.assets().get_asset(val).is_none() {
                                return Ok(f64::MAX)
                            }
                            ide::ROOMS.timestamps().get_asset(val)
                        },
                        12 => {
                            if ide::FONTS.assets().get_asset(val).is_none() {
                                return Ok(f64::MAX)
                            }
                            ide::FONTS.timestamps().get_asset(val)
                        },
                        14 => {
                            if ide::TIMELINES.assets().get_asset(val).is_none() {
                                return Ok(f64::MAX)
                            }
                            ide::TIMELINES.timestamps().get_asset(val)
                        },
                        _ => return Ok(0.0),
                    };

                    Ok(time)
                }
                if !matches!(parameter_get_timestamp(*ty, val).map(|t| t > LAST_SAVE), Ok(false)) {
                    return true
                }
            }
        }
    }
    return false
}

unsafe fn save_event<F: Write>(ev: &Event, name: &str, file: &mut F) -> Result<()> {
    writeln!(file, "#define {}", name)?;
    for action in ev.get_actions() {
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
                    if i >= 0 && ide::OBJECTS.assets().get_asset(i).is_none() && !SAW_APPLIES_TO_WARNING {
                        show_message(
                            "WARNING: Project contains actions that apply to an object that has been deleted. \
                            These will do absolutely nothing when executed. \
                            You may want to find them and make sure nothing is broken. \
                            You can find them by searching the project for \"apply_to\\n\" with Notepad++.",
                        );
                        SAW_APPLIES_TO_WARNING = true;
                    }
                    writeln!(file, "applies_to={}", ide::OBJECTS.names().get_asset(i))?
                },
            }
        }
        match action.action_kind {
            0 => {
                // normal
                writeln!(file, "invert={}", u8::from(action.invert_condition))?;
                for i in 0..action.param_count as usize {
                    writeln!(file, "arg{}={}", i, match action.param_types[i] {
                        5 => ide::SPRITES.names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        6 => ide::SOUNDS.names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        7 => ide::BACKGROUNDS.names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        8 => ide::PATHS.names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        9 => ide::SCRIPTS.names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        10 => ide::OBJECTS.names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        11 => ide::ROOMS.names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        12 => ide::FONTS.names().get_asset(action.param_strings[i].try_decode()?.parse()?),
                        14 => ide::TIMELINES.names().get_asset(action.param_strings[i].try_decode()?.parse()?),
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
            let code = action.param_strings[0].try_decode()?;
            if (code.starts_with("#define") || code.contains("\n#define")) {
                return Err(Error::Other("events should not contain #define".to_string()))
            }
            write_gml(file, &action.param_strings[0])?;
        }
    }
    Ok(())
}

unsafe fn timeline_needs_update(tl: &Timeline) -> bool {
    tl.get_events().iter().any(|e| event_needs_update(e))
}

unsafe fn save_timeline(tl: &Timeline, path: &mut PathBuf) -> Result<()> {
    path.set_extension("gml");
    let mut f = open_file(path)?;
    for (time, event) in tl.get_times().iter().zip(tl.get_events()) {
        if event.action_count != 0 {
            save_event(event, &time.to_string(), &mut f)?;
        }
    }
    f.flush()?;
    Ok(())
}

fn event_name(ev_type: usize, ev_numb: usize) -> String {
    match ev_type {
        events::EV_COLLISION => {
            format!("{}_{}", events::EVENT_NAMES[ev_type], ide::OBJECTS.names().get_asset(ev_numb as _))
        },
        events::EV_TRIGGER => format!(
            "{}_{}",
            events::EVENT_NAMES[ev_type],
            ide::get_triggers().get_asset(ev_numb as _).and_then(|t| t.name.try_decode_opt()).unwrap_or_default()
        ),
        _ => format!("{}_{}", events::EVENT_NAMES[ev_type], ev_numb),
    }
}

unsafe fn object_needs_update(obj: &Object) -> bool {
    return obj.sprite_index >= 0
        && (ide::SPRITES.assets().get_asset(obj.sprite_index).is_none()
            || ide::SPRITES.timestamps().get_asset(obj.sprite_index) > LAST_SAVE)
        || obj.mask_index >= 0
            && (ide::SPRITES.assets().get_asset(obj.mask_index).is_none()
                || ide::SPRITES.timestamps().get_asset(obj.mask_index) > LAST_SAVE)
        || obj.parent_index >= 0
            && (ide::OBJECTS.assets().get_asset(obj.parent_index).is_none()
                || ide::OBJECTS.timestamps().get_asset(obj.parent_index) > LAST_SAVE)
        || obj.events.iter().flatten().any(|e| event_needs_update(e))
        || !obj.events[events::EV_TRIGGER].is_empty() && *ide::TRIGGERS_UPDATED
        || obj.events[events::EV_COLLISION]
            .iter()
            .enumerate()
            .any(|(i, e)| e.action_count != 0 && ide::OBJECTS.timestamps().get_asset(i as _) > LAST_SAVE)
}

unsafe fn save_object(obj: &Object, path: &mut PathBuf) -> Result<()> {
    path.set_extension("txt");
    {
        let mut f = open_file(&path)?;
        writeln!(f, "sprite={}", ide::SPRITES.names().get_asset(obj.sprite_index))?;
        writeln!(f, "visible={}", u8::from(obj.visible))?;
        writeln!(f, "solid={}", u8::from(obj.solid))?;
        writeln!(f, "persistent={}", u8::from(obj.persistent))?;
        writeln!(f, "depth={}", obj.depth)?;
        writeln!(f, "parent={}", ide::OBJECTS.names().get_asset(obj.parent_index))?;
        writeln!(f, "mask={}", ide::SPRITES.names().get_asset(obj.mask_index))?;
        f.flush()?;
    }
    path.set_extension("gml");
    {
        let mut f = open_file(&path)?;
        for (ev_type, event_group) in obj.events.iter().enumerate() {
            for (ev_numb, ev) in event_group.iter().enumerate() {
                if ev.action_count != 0 {
                    if (ev_type == events::EV_COLLISION && ide::OBJECTS.assets().get_asset(ev_numb as _).is_none())
                        || (ev_type == events::EV_TRIGGER && ide::get_triggers().get_asset(ev_numb as _).is_none())
                    {
                        continue
                    }
                    let name = event_name(ev_type, ev_numb);
                    save_event(ev, &name, &mut f)?;
                }
            }
        }
        f.flush()?;
    }
    Ok(())
}

unsafe fn room_needs_update(room: &Room) -> bool {
    return room.backgrounds.iter().any(|bg| {
        bg.source_bg >= 0
            && (ide::BACKGROUNDS.assets().get_asset(bg.source_bg).is_none()
                || ide::BACKGROUNDS.timestamps().get_asset(bg.source_bg) > LAST_SAVE)
    }) || room.views.iter().any(|view| {
        view.following_target >= 0
            && (ide::OBJECTS.assets().get_asset(view.following_target).is_none()
                || ide::OBJECTS.timestamps().get_asset(view.following_target) > LAST_SAVE)
    }) || room.get_instances().iter().any(|i| {
        i.object >= 0
            && (ide::OBJECTS.assets().get_asset(i.object).is_none()
                || ide::OBJECTS.timestamps().get_asset(i.object) > LAST_SAVE)
    }) || room.get_tiles().iter().any(|t| {
        t.source_bg >= 0
            && (ide::BACKGROUNDS.assets().get_asset(t.source_bg).is_none()
                || ide::BACKGROUNDS.timestamps().get_asset(t.source_bg) > LAST_SAVE)
    })
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
            ide::BACKGROUNDS.names().get_asset(tile.source_bg),
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
    let extra_data = unsafe { &mut EXTRA_DATA.get_or_insert_with(Default::default).0 };

    for instance in instances {
        let mut code = Vec::with_capacity(instance.creation_code.len());
        write_gml(&mut code, &instance.creation_code)?;
        // get id
        let fname = format!("{:08X}", extra_data.get(&instance.id).unwrap().name);
        if !code.is_empty() {
            path.push(&fname);
            path.set_extension("gml");
            write_file(&path, &code)?;
            path.pop();
        }
        let InstanceExtra { xscale, yscale, blend, angle, .. } =
            unsafe { EXTRA_DATA.as_ref().and_then(|e| e.0.get(&instance.id).cloned()).unwrap_or_default() };
        writeln!(
            f,
            "{},{},{},{},{},{},{},{},{},{}",
            ide::OBJECTS.names().get_asset(instance.object),
            instance.x,
            instance.y,
            fname,
            u8::from(instance.locked),
            xscale,
            yscale,
            blend,
            angle,
            u8::from(!code.is_empty()),
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
            writeln!(f, "bg_source{}={}", i, ide::BACKGROUNDS.names().get_asset(bg.source_bg))?;
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
            writeln!(f, "view_fol_target{}={}", i, ide::OBJECTS.names().get_asset(view.following_target))?;
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

fn save_constants(path: &mut PathBuf) -> Result<()> {
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

unsafe fn save_settings(path: &mut PathBuf, smart_save: bool) -> Result<()> {
    use ide::settings::*;
    path.push("settings");
    create_dirs(&path)?;
    if !smart_save || *ide::CONSTANTS_UPDATED {
        save_constants(path)?;
    }
    if !smart_save || *ide::SETTINGS_UPDATED {
        // not the usual behaviour, but i don't feel like adding more flags than necessary
        if *HAS_CUSTOM_LOAD_IMAGE && (*CUSTOM_LOAD_IMAGE).is_none() {
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
            writeln!(f, "bar_has_bg={}", u8::from(!(*LOADING_BACKGROUND).is_none()))?;
            writeln!(f, "bar_has_fg={}", u8::from(!(*LOADING_FOREGROUND).is_none()))?;
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
            if let Some(bg) = (*LOADING_BACKGROUND).as_ref() {
                path.push("back.bmp");
                bg.SaveToFile(&UStr::new(&path));
                path.pop();
            }
            if let Some(fg) = (*LOADING_FOREGROUND).as_ref() {
                path.push("front.bmp");
                fg.SaveToFile(&UStr::new(&path));
                path.pop();
            }
        }
        if HAS_CUSTOM_LOAD_IMAGE.read() {
            path.push("loader.bmp");
            (*CUSTOM_LOAD_IMAGE).as_ref().unwrap().SaveToFile(&UStr::new(&path));
            path.pop();
        }
        // icon is never legally null, so no need to check
        path.push("icon.ico");
        (*ICON).SaveToFile(&UStr::new(&path));
        path.pop();
    }
    if !smart_save || *ide::EXTENSIONS_UPDATED {
        path.push("extensions.txt");
        let extensions = ide::get_extensions();
        let extensions_loaded = ide::get_extensions_loaded();
        {
            let mut f = open_file(&path)?;
            for (extension, &loaded) in extensions.iter().zip(extensions_loaded) {
                if loaded {
                    writeln!(f, "{}", extension.name.try_decode()?)?;
                }
            }
            f.flush()?;
        }
        path.pop();
    }
    if !smart_save || *ide::GAME_INFO_UPDATED {
        save_game_information(path)?;
    }
    path.pop();
    Ok(())
}

fn save_triggers(path: &mut PathBuf) -> Result<()> {
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
    assets: &[Option<DelphiBox<T>>],
    names: &[UStr],
    timestamps: &[f64],
    tree: *const *const TTreeNode,
    save_func: unsafe fn(&T, &mut PathBuf) -> Result<()>,
    smart_save: bool,
    dependency_check: unsafe fn(&T) -> bool,
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
        (assets, names, timestamps).into_par_iter().try_for_each(|(asset, name, timestamp)| -> Result<()> {
            if let Some(asset) = asset {
                if !smart_save || *timestamp > LAST_SAVE || dependency_check(asset) {
                    let name = name.try_decode()?;
                    let mut p = path.join(name);
                    save_func(asset, &mut p)?;
                    let _ = tx.send(());
                }
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

unsafe fn save_included_files(path: &mut PathBuf, smart_save: bool) -> Result<()> {
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
    for (file, timestamp) in files.iter().zip(ide::get_included_file_timestamps()) {
        if smart_save && *timestamp < LAST_SAVE {
            continue
        }
        let file = &**file;
        let mut name = file.file_name.try_decode()?;
        if file.data_exists {
            if file.stored_in_gmk {
                path.push("include");
                path.push(&name);
                write_file(&path, file.data.get_slice())?;
                path.pop();
                path.pop();
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

unsafe fn save_icon_cache(path: &mut PathBuf, smart_save: bool) -> Result<()> {
    const BMP_HEADER: &[u8] = include_bytes!("../assets/thumb_header.dat");
    const BMP_SIZE: usize = 16 * 16 * 4 + BMP_HEADER.len();
    unsafe fn save_frame(frame: &Frame, name: &UStr, path: &std::path::Path, only_if_needed: bool) -> Result<()> {
        if frame.width == 0 || frame.height == 0 {
            return Ok(())
        }
        let mut path = path.join(name.to_os_string());
        path.set_extension("bmp");
        if only_if_needed && path.exists() {
            return Ok(())
        }
        let mut out = Vec::with_capacity(BMP_SIZE);
        out.extend_from_slice(BMP_HEADER);
        out.set_len(BMP_SIZE);
        frame.thumb(&mut out[BMP_HEADER.len()..], true, [255, 255, 255]);
        write_file(&path, out)
    }
    path.push("cache");
    path.push("sprites");
    create_dirs(path)?;
    (ide::SPRITES.assets(), ide::SPRITES.names(), ide::SPRITES.timestamps()).into_par_iter().try_for_each(
        |(sprite, name, timestamp)| -> Result<()> {
            if let Some(frame) = sprite.as_deref().and_then(|s| s.get_frames().get(0)) {
                save_frame(frame, name, path, smart_save && *timestamp < LAST_SAVE)?;
            }
            Ok(())
        },
    )?;
    path.pop();
    path.push("backgrounds");
    create_dirs(path)?;
    (ide::BACKGROUNDS.assets(), ide::BACKGROUNDS.names(), ide::BACKGROUNDS.timestamps()).into_par_iter().try_for_each(
        |(bg, name, timestamp)| -> Result<()> {
            if let Some(bg) = bg {
                save_frame(&bg.frame, name, path, smart_save && *timestamp < LAST_SAVE)?;
            }
            Ok(())
        },
    )?;
    path.pop();
    path.pop();
    Ok(())
}

pub unsafe fn save_gmk(path: &mut PathBuf) -> Result<()> {
    // if we have a watcher, we can do a smart save
    // but if time went backwards, we must do a full save
    let smart_save = project_watcher::watching() && LAST_SAVE != 0.0;
    project_watcher::unwatch();
    PATH_FORM_UPDATED = false;
    // check if we have any assets to save
    let has_backgrounds = ide::BACKGROUNDS.assets().iter().any(Option::is_some);
    let has_datafiles = !ide::get_included_files().is_empty();
    let has_fonts = ide::FONTS.assets().iter().any(Option::is_some);
    let has_objects = ide::OBJECTS.assets().iter().any(Option::is_some);
    let has_paths = ide::PATHS.assets().iter().any(Option::is_some);
    let has_scripts = ide::SCRIPTS.assets().iter().any(Option::is_some);
    let has_sounds = ide::SOUNDS.assets().iter().any(Option::is_some);
    let has_sprites = ide::SPRITES.assets().iter().any(Option::is_some);
    let has_timelines = ide::TIMELINES.assets().iter().any(Option::is_some);
    let has_triggers = !ide::get_triggers().is_empty();
    // always rewrite the .gm82 file so you can easily see the timestamp
    {
        create_dirs(path.parent().unwrap())?;
        // some stuff to go in the main gmk
        let mut f = open_file(&path)?;
        writeln!(f, "gm82_version=5")?;
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
        writeln!(f, "has_backgrounds={}", u8::from(has_backgrounds))?;
        writeln!(f, "has_datafiles={}", u8::from(has_datafiles))?;
        writeln!(f, "has_fonts={}", u8::from(has_fonts))?;
        writeln!(f, "has_objects={}", u8::from(has_objects))?;
        writeln!(f, "has_paths={}", u8::from(has_paths))?;
        writeln!(f, "has_scripts={}", u8::from(has_scripts))?;
        writeln!(f, "has_sounds={}", u8::from(has_sounds))?;
        writeln!(f, "has_sprites={}", u8::from(has_sprites))?;
        writeln!(f, "has_timelines={}", u8::from(has_timelines))?;
        writeln!(f, "has_triggers={}", u8::from(has_triggers))?;
        /*
        writeln!(f, "last_instance_id={}", *ide::_LAST_INSTANCE_ID)?;
        writeln!(f, "last_tile_id={}", *ide::_LAST_TILE_ID)?;
         */
        f.flush()?;
    }
    path.pop();
    advance_progress_form(5);
    save_settings(path, smart_save)?;
    advance_progress_form(10);
    if has_triggers && (!smart_save || *ide::TRIGGERS_UPDATED) {
        save_triggers(path)?;
    }
    advance_progress_form(15);
    if has_sounds && (!smart_save || *ide::SOUNDS_UPDATED || *ide::RESOURCE_TREE_UPDATED) {
        save_assets(
            15,
            30,
            "sounds",
            ide::SOUNDS.assets(),
            ide::SOUNDS.names(),
            ide::SOUNDS.timestamps(),
            ide::RT_SOUNDS,
            save_sound,
            smart_save,
            no_dependencies,
            path,
        )?;
    }
    advance_progress_form(30);
    if has_sprites && (!smart_save || *ide::SPRITES_UPDATED || *ide::RESOURCE_TREE_UPDATED) {
        save_assets(
            30,
            55,
            "sprites",
            ide::SPRITES.assets(),
            ide::SPRITES.names(),
            ide::SPRITES.timestamps(),
            ide::RT_SPRITES,
            save_sprite,
            smart_save,
            no_dependencies,
            path,
        )?;
    }
    advance_progress_form(55);
    if has_backgrounds && (!smart_save || *ide::BACKGROUNDS_UPDATED || *ide::RESOURCE_TREE_UPDATED) {
        save_assets(
            55,
            65,
            "backgrounds",
            ide::BACKGROUNDS.assets(),
            ide::BACKGROUNDS.names(),
            ide::BACKGROUNDS.timestamps(),
            ide::RT_BACKGROUNDS,
            save_background,
            smart_save,
            no_dependencies,
            path,
        )?;
    }
    advance_progress_form(65);
    if has_paths {
        save_assets(
            65,
            70,
            "paths",
            ide::PATHS.assets(),
            ide::PATHS.names(),
            ide::PATHS.timestamps(),
            ide::RT_PATHS,
            save_path,
            smart_save,
            path_needs_update,
            path,
        )?;
    }
    advance_progress_form(70);
    if has_scripts && (!smart_save || *ide::SCRIPTS_UPDATED || *ide::RESOURCE_TREE_UPDATED) {
        save_assets(
            70,
            75,
            "scripts",
            ide::SCRIPTS.assets(),
            ide::SCRIPTS.names(),
            ide::SCRIPTS.timestamps(),
            ide::RT_SCRIPTS,
            save_script,
            smart_save,
            no_dependencies,
            path,
        )?;
    }
    advance_progress_form(75);
    if has_fonts && (!smart_save || *ide::FONTS_UPDATED || *ide::RESOURCE_TREE_UPDATED) {
        save_assets(
            75,
            80,
            "fonts",
            ide::FONTS.assets(),
            ide::FONTS.names(),
            ide::FONTS.timestamps(),
            ide::RT_FONTS,
            save_font,
            smart_save,
            no_dependencies,
            path,
        )?;
    }
    advance_progress_form(80);
    if has_timelines {
        save_assets(
            80,
            85,
            "timelines",
            ide::TIMELINES.assets(),
            ide::TIMELINES.names(),
            ide::TIMELINES.timestamps(),
            ide::RT_TIMELINES,
            save_timeline,
            smart_save,
            timeline_needs_update,
            path,
        )?;
    }
    advance_progress_form(85);
    if has_objects {
        save_assets(
            85,
            90,
            "objects",
            ide::OBJECTS.assets(),
            ide::OBJECTS.names(),
            ide::OBJECTS.timestamps(),
            ide::RT_OBJECTS,
            save_object,
            smart_save,
            object_needs_update,
            path,
        )?;
    }
    advance_progress_form(90);
    // give instances ids if they don't already have one
    for (room, timestamp) in
        ide::ROOMS.assets().iter().zip(ide::ROOMS.timestamps_mut()).filter_map(|(r, t)| Some((r.as_deref()?, t)))
    {
        let extra_data = &mut EXTRA_DATA.get_or_insert_with(Default::default).0;
        for id in room.get_instances().iter().map(|i| i.id) {
            let mut name = extra_data.entry(id).or_default().name;
            if name == 0 {
                *ide::ROOMS_UPDATED = true;
                delphi::Now(timestamp);
                loop {
                    name = delphi::Random();
                    if !extra_data.values().any(|ex| ex.name == name) {
                        extra_data.get_mut(&id).unwrap().name = name;
                        break
                    }
                }
            }
        }
    }
    save_assets(
        90,
        95,
        "rooms",
        ide::ROOMS.assets(),
        ide::ROOMS.names(),
        ide::ROOMS.timestamps(),
        ide::RT_ROOMS,
        save_room,
        smart_save,
        room_needs_update,
        path,
    )?;
    advance_progress_form(95);
    if has_datafiles && (!smart_save || *ide::INCLUDED_FILES_UPDATED) {
        save_included_files(path, smart_save)?;
    }

    if !smart_save || *ide::SPRITES_UPDATED || *ide::BACKGROUNDS_UPDATED || {
        path.push("cache");
        let exists = path.exists();
        path.pop();
        !exists
    } {
        save_icon_cache(path, smart_save)?;
    }

    advance_progress_form(100);

    update_timestamp();

    Ok(())
}
