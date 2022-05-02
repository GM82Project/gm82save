use crate::{
    asset::*,
    delphi,
    delphi::{advance_progress_form, UStr},
    events, ide, run_while_updating_bar, update_timestamp, Error, InstanceExtra, Result, TileExtra, ACTION_TOKEN,
    EXTRA_DATA, PATH_FORM_UPDATED,
};
use itertools::izip;
use parking_lot::Mutex;
use rayon::prelude::*;
use std::{
    collections::{HashMap, HashSet},
    ffi::OsStr,
    fs::File,
    hint::unreachable_unchecked,
    io::{BufRead, BufReader},
    path::PathBuf,
    slice,
};

fn undelimit(s: &str) -> String {
    s.replace("*\\/", "*/").replace("\\n", "\n").replace("\\r", "\r").replace("\\\\", "\\")
}

pub trait UStrPtr {
    fn asg(self, s: impl AsRef<OsStr>);
    fn asg_undelimit(self, s: &str);
}

impl UStrPtr for *mut UStr {
    fn asg(self, s: impl AsRef<OsStr>) {
        unsafe {
            delphi::UStrAsg(self.as_mut().unwrap(), &UStr::new(s));
        }
    }

    fn asg_undelimit(self, s: &str) {
        self.asg(&undelimit(s));
    }
}

fn load_gml(code: &str) -> UStr {
    let mut buf = String::with_capacity(code.len());
    // don't use string.replace() in case your gml is \r\n for some reason
    for line in code.lines() {
        buf += line.trim_end();
        buf += "\r\n";
    }
    UStr::new(buf)
}

struct Assets {
    index: Vec<String>,
    map: HashMap<String, usize>,
}
pub struct AssetMaps {
    triggers: Assets,
    sprites: Assets,
    sounds: Assets,
    backgrounds: Assets,
    paths: Assets,
    scripts: Assets,
    objects: Assets,
    rooms: Assets,
    fonts: Assets,
    timelines: Assets,
}

fn open_file(path: &std::path::Path) -> Result<BufReader<File>> {
    Ok(BufReader::new(File::open(path).map_err(|e| Error::FileIoError(e, path.to_path_buf()))?))
}

fn read_file<P: AsRef<std::path::Path>>(path: P) -> Result<String> {
    std::fs::read_to_string(path.as_ref()).map_err(|e| Error::FileIoError(e, path.as_ref().to_path_buf()))
}

fn decode_line<'a, F: FnMut(&'a str, &'a str) -> Result<()>>(
    path: &std::path::Path,
    line: &'a str,
    func: &mut F,
) -> Result<()> {
    if !line.is_empty() {
        let (key, value) = line.split_once('=').ok_or_else(|| Error::SyntaxError(path.to_path_buf()))?;
        func(key, value)?;
    }
    Ok(())
}

fn read_txt<F: FnMut(&str, &str) -> Result<()>>(path: &std::path::Path, mut func: F) -> Result<()> {
    let f = open_file(path)?;
    for line in f.lines() {
        decode_line(path, &line?, &mut func)?;
    }
    Ok(())
}

unsafe fn read_resource_tree(
    base: *const *const delphi::TTreeNode,
    kind: u32,
    type_name: &str,
    names: &HashMap<String, usize>,
    visible: bool,
    path: &mut PathBuf,
) -> Result<()> {
    path.push(type_name);
    path.push("tree.yyd");
    let f = open_file(path)?;
    let nodes = &*((**if visible { ide::RESOURCE_TREE } else { ide::RESOURCE_TREE_HIDDEN }).nodes);
    let mut stack = vec![base.read()];
    for line in f.lines() {
        let line = line?;
        if line.is_empty() {
            continue
        }
        let trimmed = line.trim_start();
        let level = line.len() - trimmed.len();
        stack.truncate(level + 1);
        let rtype = match trimmed.chars().next() {
            Some('+') => 2,
            Some('|') => 3,
            _ => return Err(Error::SyntaxError(path.to_path_buf())),
        };
        let name = &trimmed[1..];
        let index =
            if rtype == 3 { *names.get(name).ok_or_else(|| Error::AssetNotFound(name.to_string()))? } else { 0 };

        let node = &*nodes.AddChild(*stack.last().unwrap(), &UStr::new(name));
        node.SetData(delphi::TreeNodeData::new(rtype, kind, index));
        node.SetImageIndex(1);
        if rtype == 2 {
            stack.push(node);
        }
    }
    path.pop();
    path.pop();
    Ok(())
}

unsafe fn load_triggers(maps: &AssetMaps, path: &mut PathBuf) -> Result<()> {
    path.push("triggers");
    let names = &maps.triggers.index;
    ide::alloc_triggers(names.len());
    for (name, trig_p) in names.iter().zip(ide::get_triggers_mut()) {
        if name.is_empty() {
            continue
        }
        let trig = &mut *Trigger::new();
        trig.name = UStr::new(name);
        path.push(name);
        path.set_extension("txt");
        read_txt(&path, |k, v| {
            match k {
                "constant" => trig.constant_name = UStr::new(v),
                "kind" => trig.kind = v.parse()?,
                _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
            }
            Ok(())
        })?;
        path.set_extension("gml");
        trig.condition = load_gml(&read_file(&path)?);
        path.pop();
        *trig_p = Some(trig);
    }
    path.pop();
    Ok(())
}

fn verify_path(path: &std::path::Path) -> Result<()> {
    if path.exists() {
        Ok(())
    } else {
        Err(Error::FileIoError(std::io::Error::new(std::io::ErrorKind::NotFound, "file not found"), path.to_path_buf()))
    }
}

unsafe fn load_sound(path: &mut PathBuf, _asset_maps: &AssetMaps) -> Result<*const Sound> {
    let snd = &mut *Sound::new();
    path.set_extension("txt");
    let mut extension = String::new();
    let mut exists = false;
    read_txt(&path, |k, v| {
        match k {
            "extension" => {
                extension = v.to_string();
                snd.extension = UStr::new(v)
            },
            "source" => snd.source = UStr::new(v),
            "exists" => exists = v.parse::<u8>()? != 0,
            "kind" => snd.kind = v.parse()?,
            "effects" => snd.effects = v.parse()?,
            "volume" => snd.volume = v.parse()?,
            "pan" => snd.pan = v.parse()?,
            "preload" => snd.preload = v.parse::<u8>()? != 0,
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        }
        Ok(())
    })?;
    if exists {
        path.set_extension(extension.trim_matches('.'));
        verify_path(&path)?;
        let data = &mut *delphi::TMemoryStream::new();
        data.load(&UStr::new(path));
        snd.data = data;
    }
    Ok(snd)
}

unsafe fn load_frame(path: &std::path::Path, frame: &mut Frame) -> Result<()> {
    use png::{BitDepth, ColorType, Decoder, Transformations};
    let err = |e| Error::PngDecodeError(path.to_path_buf(), e);
    // no open_file because png uses BufReader internally
    let mut decoder = Decoder::new(File::open(&path).map_err(|e| Error::FileIoError(e, path.to_path_buf()))?);
    decoder.set_transformations(Transformations::EXPAND | Transformations::STRIP_16);
    let mut reader = decoder.read_info().map_err(err)?;
    let info = reader.info();
    frame.width = info.width;
    frame.height = info.height;
    // do this calculation myself in case the png is a weird format
    let line_size = info.width as usize * 4;
    let data_size = line_size * info.height as usize;
    let data = slice::from_raw_parts_mut(delphi::GetMem(data_size), data_size);
    match (info.bit_depth, info.color_type) {
        (BitDepth::Eight, ColorType::Rgba) => {
            // this should be the only one that actually gets used
            // but i'll allow other formats too just to be nice
            reader.next_frame(data).map_err(err)?;
            // RGBA8 -> BGRA8
            data.par_chunks_exact_mut(4).for_each(|px| px.swap(0, 2));
        },
        (BitDepth::Eight, ColorType::Rgb) => {
            for dst_row in data.chunks_exact_mut(line_size) {
                let src_row = reader.next_row().map_err(err)?.ok_or_else(|| {
                    Error::Other(format!("decoding ended too soon for image {}", path.to_string_lossy()))
                })?;
                // RGB8 -> BGR8
                for (dst, src) in dst_row.chunks_exact_mut(4).zip(src_row.data().chunks_exact(3)) {
                    dst[0] = src[2];
                    dst[1] = src[1];
                    dst[2] = src[0];
                    dst[3] = 255;
                }
            }
        },
        (BitDepth::Eight, ColorType::Grayscale) => {
            for dst_row in data.chunks_exact_mut(line_size) {
                let src_row = reader.next_row().map_err(err)?.ok_or_else(|| {
                    Error::Other(format!("decoding ended too soon for image {}", path.to_string_lossy()))
                })?;
                for (dst, &src) in dst_row.chunks_exact_mut(4).zip(src_row.data()) {
                    dst[0..3].fill(src);
                    dst[3] = 255;
                }
            }
        },
        (BitDepth::Eight, ColorType::GrayscaleAlpha) => {
            for dst_row in data.chunks_exact_mut(line_size) {
                let src_row = reader.next_row().map_err(err)?.ok_or_else(|| {
                    Error::Other(format!("decoding ended too soon for image {}", path.to_string_lossy()))
                })?;
                for (dst, src) in dst_row.chunks_exact_mut(4).zip(src_row.data().chunks_exact(2)) {
                    dst[0..3].fill(src[0]);
                    dst[3] = src[1];
                }
            }
        },
        (depth, coltype) => {
            // the above should cover all valid formats
            // see https://www.w3.org/TR/PNG-Chunks.html
            return Err(Error::Other(format!(
                "couldn't understand format {:?}/{:?} for image {}",
                depth,
                coltype,
                path.to_string_lossy()
            )))
        },
    }
    frame.data = data.as_ptr();
    Ok(())
}

unsafe fn load_background(path: &mut PathBuf, _asset_maps: &AssetMaps) -> Result<*const Background> {
    let bg = &mut *Background::new();
    path.set_extension("txt");
    let mut bg_exists = false;
    read_txt(path, |k, v| {
        match k {
            "exists" => bg_exists = v.parse::<u8>()? != 0,
            "tileset" => bg.is_tileset = v.parse::<u8>()? != 0,
            "tile_width" => bg.tile_width = v.parse()?,
            "tile_height" => bg.tile_height = v.parse()?,
            "tile_hoffset" => bg.h_offset = v.parse()?,
            "tile_voffset" => bg.v_offset = v.parse()?,
            "tile_hsep" => bg.h_sep = v.parse()?,
            "tile_vsep" => bg.v_sep = v.parse()?,
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        }
        Ok(())
    })?;
    if bg_exists {
        path.set_extension("png");
        load_frame(path, &mut *bg.frame)?;
    }
    Ok(bg)
}

unsafe fn load_sprite(path: &mut PathBuf, _asset_maps: &AssetMaps) -> Result<*const Sprite> {
    let sp = &mut *Sprite::new();
    path.push("sprite.txt");
    read_txt(&path, |k, v| {
        match k {
            "frames" => sp.frame_count = v.parse()?,
            "origin_x" => sp.origin_x = v.parse()?,
            "origin_y" => sp.origin_y = v.parse()?,
            "collision_shape" => sp.collision_shape = v.parse()?,
            "alpha_tolerance" => sp.alpha_tolerance = v.parse()?,
            "per_frame_colliders" => sp.per_frame_colliders = v.parse::<u8>()? != 0,
            "bbox_type" => sp.bbox_type = v.parse()?,
            "bbox_left" => sp.bbox_left = v.parse()?,
            "bbox_bottom" => sp.bbox_bottom = v.parse()?,
            "bbox_right" => sp.bbox_right = v.parse()?,
            "bbox_top" => sp.bbox_top = v.parse()?,
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        }
        Ok(())
    })?;
    path.pop();
    for (i, f) in sp.alloc_frames(sp.frame_count as usize).iter_mut().enumerate() {
        path.push(format!("{}.png", i));
        *f = Frame::new();
        load_frame(&path, &mut **f)?;
        path.pop();
    }
    Ok(sp)
}

unsafe fn load_script(path: &mut PathBuf, _asset_maps: &AssetMaps) -> Result<*const Script> {
    path.set_extension("gml");
    let s = Script::new();
    (*s).source = load_gml(&read_file(path)?);
    Ok(s)
}

unsafe fn load_font(path: &mut PathBuf, _asset_maps: &AssetMaps) -> Result<*const Font> {
    let f = &mut *Font::new();
    path.set_extension("txt");
    read_txt(path, |k, v| {
        match k {
            "name" => f.sys_name = UStr::new(v),
            "size" => f.size = v.parse()?,
            "bold" => f.bold = v.parse::<u8>()? != 0,
            "italic" => f.italic = v.parse::<u8>()? != 0,
            "charset" => f.charset = v.parse()?,
            "aa_level" => f.aa_level = v.parse()?, // DOES NOT CORRESPOND TO .GMK OR .EXE
            "range_start" => f.range_start = v.parse()?,
            "range_end" => f.range_end = v.parse()?,
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        }
        Ok(())
    })?;
    Ok(f)
}

unsafe fn load_event(
    path: &std::path::Path,
    event: &mut Event,
    event_code: &str,
    asset_maps: &AssetMaps,
) -> Result<()> {
    for action_code in event_code.split(ACTION_TOKEN) {
        if action_code.trim().is_empty() {
            continue
        }
        let (params, code) = action_code.split_once("*/").ok_or_else(|| Error::SyntaxError(path.to_path_buf()))?;
        let action = &mut *event.add_action(0, 0);
        let mut lib_id_set = false;
        let mut act_id_set = false;
        for line in params.lines() {
            decode_line(path, line, &mut |k, v| {
                match k {
                    "lib_id" => {
                        action.lib_id = v.parse()?;
                        if lib_id_set {
                            return Err(Error::SyntaxError(path.to_path_buf()))
                        }
                        lib_id_set = true;
                    },
                    "action_id" => {
                        action.id = v.parse()?;
                        if !lib_id_set || act_id_set {
                            return Err(Error::SyntaxError(path.to_path_buf()))
                        }
                        act_id_set = true;
                        // manually check if action exists so we can throw an error
                        if ide::get_action_libraries().iter().any(|&l| {
                            l.id == action.lib_id
                                && slice::from_raw_parts(l.actions, l.action_count).iter().any(|&a| a.id == action.id)
                        }) {
                            action.fill_in(action.lib_id, action.id);
                        } else {
                            return Err(Error::UnknownAction(action.lib_id, action.id))
                        }
                    },
                    "relative" => action.is_relative = v.parse::<u8>()? != 0,
                    "applies_to" => {
                        action.applies_to = match v {
                            "other" => -2,
                            "self" => -1,
                            "" => -4,
                            name => *asset_maps
                                .objects
                                .map
                                .get(name)
                                .ok_or_else(|| Error::AssetNotFound(name.to_string()))?
                                as _,
                        }
                    },
                    "invert" => action.invert_condition = v.parse::<u8>()? != 0,
                    "var_name" | "repeats" => action.param_strings[0] = UStr::new(v),
                    "var_value" => action.param_strings[1] = UStr::new(v),
                    "arg0" | "arg1" | "arg2" | "arg3" | "arg4" | "arg5" | "arg6" | "arg7" => {
                        if !act_id_set {
                            return Err(Error::SyntaxError(path.to_path_buf()))
                        }
                        let i = k.chars().last().unwrap().to_digit(8).unwrap() as usize;
                        let err = || Error::AssetNotFound(v.to_string());
                        let ptype = action.param_types[i];
                        if (5..=14).contains(&ptype) && ptype != 13 {
                            action.param_strings[i] = UStr::new(
                                match action.param_types[i] {
                                    _ if v.is_empty() => -1,
                                    5 => *asset_maps.sprites.map.get(v).ok_or_else(err)? as _,
                                    6 => *asset_maps.sounds.map.get(v).ok_or_else(err)? as _,
                                    7 => *asset_maps.backgrounds.map.get(v).ok_or_else(err)? as _,
                                    8 => *asset_maps.paths.map.get(v).ok_or_else(err)? as _,
                                    9 => *asset_maps.scripts.map.get(v).ok_or_else(err)? as _,
                                    10 => *asset_maps.objects.map.get(v).ok_or_else(err)? as _,
                                    11 => *asset_maps.rooms.map.get(v).ok_or_else(err)? as _,
                                    12 => *asset_maps.fonts.map.get(v).ok_or_else(err)? as _,
                                    14 => *asset_maps.timelines.map.get(v).ok_or_else(err)? as _,
                                    _ => unreachable_unchecked(),
                                }
                                .to_string(),
                            );
                        } else {
                            action.param_strings[i] = UStr::new(undelimit(v));
                        }
                    },
                    _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
                }
                Ok(())
            })?;
        }
        if action.action_kind == 7 {
            // first character will always be a newline because it doesn't cut the newline when searching for */
            // so skip it
            let code = if code.starts_with('\n') {
                &code[1..]
            } else if code.starts_with("\r\n") {
                &code[2..]
            } else {
                code
            };
            action.param_strings[0] = load_gml(code);
        }
    }
    Ok(())
}

unsafe fn load_object(path: &mut PathBuf, asset_maps: &AssetMaps) -> Result<*const Object> {
    path.set_extension("txt");
    let obj = &mut *Object::new();
    let sprite_map = &asset_maps.sprites.map;
    let object_map = &asset_maps.objects.map;
    let trigger_map = &asset_maps.triggers.map;
    read_txt(&path, |k, v| {
        match k {
            "sprite" => {
                obj.sprite_index = match sprite_map.get(v) {
                    Some(&i) => i as _,
                    None if v.is_empty() => -1,
                    _ => return Err(Error::AssetNotFound(v.to_string())),
                }
            },
            "visible" => obj.visible = v.parse::<u8>()? != 0,
            "solid" => obj.solid = v.parse::<u8>()? != 0,
            "persistent" => obj.persistent = v.parse::<u8>()? != 0,
            "depth" => obj.depth = v.parse()?,
            "parent" => {
                obj.parent_index = match object_map.get(v) {
                    Some(&i) => i as _,
                    None if v.is_empty() => -1,
                    _ => return Err(Error::AssetNotFound(v.to_string())),
                }
            },
            "mask" => {
                obj.mask_index = match sprite_map.get(v) {
                    Some(&i) => i as _,
                    None if v.is_empty() => -1,
                    _ => return Err(Error::AssetNotFound(v.to_string())),
                }
            },
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        }
        Ok(())
    })?;
    path.set_extension("gml");
    let code = read_file(&path)?;
    for event in code.trim_start_matches("#define ").split("\n#define ") {
        if event.trim().is_empty() {
            continue
        }
        let err = || Error::SyntaxError(path.to_path_buf());
        let (name, actions) = event.split_once("\n").ok_or_else(err)?;
        let (ev_type_s, ev_numb_s) = name.trim().split_once("_").ok_or_else(err)?;
        let ev_type = events::EVENT_NAMES.iter().position(|&s| s == ev_type_s).ok_or_else(err)?;
        let ev_numb = match ev_type {
            events::EV_COLLISION => *object_map.get(ev_numb_s).ok_or_else(err)?,
            events::EV_TRIGGER => *trigger_map.get(ev_numb_s).ok_or_else(err)?,
            _ => ev_numb_s.parse()?,
        };
        let event = &mut *obj.get_event(ev_type, ev_numb);
        load_event(&path, event, actions, asset_maps)?;
    }
    Ok(obj)
}

unsafe fn load_timeline(path: &mut PathBuf, asset_maps: &AssetMaps) -> Result<*const Timeline> {
    let tl = &mut *Timeline::new();
    path.set_extension("gml");
    let code = read_file(&path)?;
    let iter = code.trim_start_matches("#define ").split("\n#define ");
    let (events, times) = tl.alloc(iter.clone().count());
    for (code, time_p, event_p) in izip!(iter, times, events) {
        if code.trim().is_empty() {
            continue
        }
        let (name, actions) = match code.split_once("\n") {
            Some(tuple) => tuple,
            None if code.as_bytes().iter().all(u8::is_ascii_digit) => (code, ""), // #define 1\n#define 2
            None => return Err(Error::SyntaxError(path.to_path_buf())),
        };
        *time_p = name.trim().parse()?;
        let event = Event::new();
        load_event(&path, &mut *event, actions, asset_maps)?;
        *event_p = event;
    }
    Ok(tl)
}

unsafe fn load_path(file_path: &mut PathBuf, asset_maps: &AssetMaps) -> Result<*const Path> {
    let path = &mut *Path::new();
    file_path.push("path.txt");
    read_txt(&file_path, |k, v| {
        match k {
            "connection" => path.connection = v.parse()?,
            "closed" => path.closed = v.parse::<u8>()? != 0,
            "precision" => path.precision = v.parse()?,
            "background" => {
                path.path_editor_room_background = if v.is_empty() {
                    -1
                } else {
                    *asset_maps.rooms.map.get(v).ok_or_else(|| Error::AssetNotFound(v.to_string()))? as _
                }
            },
            "snap_x" => path.snap_x = v.parse()?,
            "snap_y" => path.snap_y = v.parse()?,
            _ => return Err(Error::UnknownKey(file_path.to_path_buf(), k.to_string())),
        }
        Ok(())
    })?;
    file_path.pop();
    file_path.push("points.txt");
    let points_txt = read_file(&file_path)?;
    let point_lines: Vec<_> = points_txt.par_lines().collect();
    for (point, line) in path.alloc_points(point_lines.len()).iter_mut().zip(point_lines) {
        let mut iter = line.split(',');
        let err = || Error::SyntaxError(file_path.to_path_buf());
        point.x = iter.next().ok_or_else(err)?.parse()?;
        point.y = iter.next().ok_or_else(err)?.parse()?;
        point.speed = iter.next().ok_or_else(err)?.parse()?;
        if iter.next() != None {
            return Err(err())
        }
    }
    path.commit();
    file_path.pop();
    Ok(path)
}

unsafe fn load_instances(room: &mut Room, path: &mut PathBuf, objs: &HashMap<String, usize>) -> Result<()> {
    path.push("instances.txt");
    let instances_txt = read_file(&path)?;
    let instances: Vec<_> = instances_txt.lines().filter(|s| !s.is_empty()).collect();
    let inst_path = path.to_path_buf(); // save instances.txt path for errors
    path.pop();
    let err = || Error::SyntaxError(inst_path.to_path_buf());
    let last_instance_id = *ide::LAST_INSTANCE_ID + 1;
    let ids = Mutex::new(HashSet::with_capacity(instances.len()));
    let mut extras: Vec<InstanceExtra> = vec![Default::default(); instances.len()];
    room.alloc_instances(instances.len()).into_par_iter().zip(&instances).enumerate().zip(&mut extras).try_for_each(
        |((i, (instance, line)), extra)| -> Result<()> {
            let mut iter = line.split(',');
            instance.object = match iter.next().ok_or_else(err)? {
                "" => -1,
                obj => *objs.get(obj).ok_or_else(|| Error::AssetNotFound(obj.to_string()))? as _,
            };
            instance.x = iter.next().ok_or_else(err)?.parse()?;
            instance.y = iter.next().ok_or_else(err)?.parse()?;
            let code_hash = iter.next().ok_or_else(err)?;
            extra.name = if !code_hash.is_empty() {
                // check if hash is taken
                let mut id = u32::from_str_radix(code_hash, 16)?;
                // while it's taken, generate random id
                while ids.lock().contains(&id) {
                    id = delphi::Random();
                }
                id
            } else {
                // no id, generate random id until we get an unused one
                loop {
                    let id = delphi::Random();
                    if !ids.lock().contains(&id) {
                        break id
                    }
                }
            };
            ids.lock().insert(extra.name);
            instance.locked = iter.next().ok_or_else(err)?.parse::<u8>()? != 0;
            instance.id = last_instance_id + i;
            if let Some(s) = iter.next() {
                extra.xscale = s.parse()?;
            }
            if let Some(s) = iter.next() {
                extra.yscale = s.parse()?;
            }
            if let Some(s) = iter.next() {
                extra.blend = s.parse()?;
            }
            if let Some(s) = iter.next() {
                extra.angle = s.parse()?;
            }
            // use the id from the file, not the generated one if relevant
            let has_code = if let Some(s) = iter.next() { s.parse::<u8>()? != 0 } else { !code_hash.is_empty() };
            if has_code {
                let mut path = path.join(code_hash);
                path.set_extension("gml");
                instance.creation_code = load_gml(&read_file(&path)?);
            }
            Ok(())
        },
    )?;
    let extra_data = &mut EXTRA_DATA.as_mut().unwrap().0;
    extra_data.reserve(extras.len());
    for (inst, extra) in room.get_instances().iter().zip(extras.drain(..)) {
        extra_data.insert(inst.id, extra);
    }
    *ide::LAST_INSTANCE_ID += instances.len();
    Ok(())
}

unsafe fn load_tiles(path: &mut PathBuf, bgs: &HashMap<String, usize>) -> Result<Vec<Tile>> {
    let mut tiles = Vec::new();
    path.push("layers.txt");
    let f = open_file(&path)?;
    path.pop();
    let extra_data = &mut EXTRA_DATA.as_mut().unwrap().1;
    for line in f.lines() {
        let line = line?;
        if line.is_empty() {
            continue
        }
        let depth = line.parse()?;
        path.push(line);
        path.set_extension("txt");
        let layer_txt = read_file(&path)?;
        let layer: Vec<_> = layer_txt.lines().filter(|s| !s.is_empty()).collect();
        tiles.reserve(layer.len());
        let err = || Error::SyntaxError(path.to_path_buf());
        let last_tile_id = *ide::LAST_TILE_ID + 1;
        let mut extras: Vec<TileExtra> = vec![Default::default(); layer.len()];
        let layer_tiles = layer
            .par_iter()
            .enumerate()
            .zip(&mut extras)
            .map(|((i, tile), extra)| {
                let mut iter = tile.split(',');
                let t = Tile {
                    source_bg: match iter.next().ok_or_else(err)? {
                        "" => -1,
                        bg => *bgs.get(bg).ok_or_else(|| Error::AssetNotFound(bg.to_string()))? as _,
                    },
                    x: iter.next().ok_or_else(err)?.parse()?,
                    y: iter.next().ok_or_else(err)?.parse()?,
                    u: iter.next().ok_or_else(err)?.parse()?,
                    v: iter.next().ok_or_else(err)?.parse()?,
                    width: iter.next().ok_or_else(err)?.parse()?,
                    height: iter.next().ok_or_else(err)?.parse()?,
                    locked: iter.next().ok_or_else(err)?.parse::<u8>()? != 0,
                    depth,
                    id: last_tile_id + i,
                };
                if let Some(s) = iter.next() {
                    extra.xscale = s.parse()?;
                }
                if let Some(s) = iter.next() {
                    extra.yscale = s.parse()?;
                }
                if let Some(s) = iter.next() {
                    extra.blend = s.parse()?;
                }
                if iter.next() != None {
                    return Err(err())
                }
                Ok(t)
            })
            .collect::<Result<Vec<_>>>()?;
        extra_data.reserve(extras.len());
        for (tile, extra) in layer_tiles.iter().zip(extras.drain(..)) {
            extra_data.insert(tile.id, extra);
        }
        *ide::LAST_TILE_ID += layer_tiles.len();
        tiles.extend_from_slice(&layer_tiles);
        path.pop();
    }
    Ok(tiles)
}

pub unsafe fn load_room(path: &mut PathBuf, asset_maps: &AssetMaps) -> Result<*const Room> {
    let room = &mut *Room::new();
    path.push("room.txt");
    read_txt(&path, |k, v| {
        match k {
            "caption" => room.caption = UStr::new(v),
            "width" => room.width = v.parse()?,
            "height" => room.height = v.parse()?,
            "snap_x" => room.snap_x = v.parse()?,
            "snap_y" => room.snap_y = v.parse()?,
            "isometric" => room.isometric = v.parse::<u8>()? != 0,
            "roomspeed" => room.speed = v.parse()?,
            "roompersistent" => room.persistent = v.parse::<u8>()? != 0,
            "bg_color" => room.bg_colour = v.parse()?,
            "clear_screen" => room.clear_screen = v.parse::<u8>()? != 0,
            "clear_view" => room.clear_view = v.parse::<u8>()? != 0,
            // 8 backgrounds/views
            k if k.chars().last().map(|c| c.is_digit(8)) == Some(true) => {
                let i = k.chars().last().and_then(|c| c.to_digit(8)).unwrap() as usize;
                match &k[..k.len() - 1] {
                    "bg_visible" => room.backgrounds[i].visible_on_start = v.parse::<u8>()? != 0,
                    "bg_is_foreground" => room.backgrounds[i].is_foreground = v.parse::<u8>()? != 0,
                    "bg_source" => {
                        room.backgrounds[i].source_bg = if v.is_empty() {
                            -1
                        } else {
                            *asset_maps.backgrounds.map.get(v).ok_or_else(|| Error::AssetNotFound(v.to_string()))? as _
                        }
                    },
                    "bg_xoffset" => room.backgrounds[i].xoffset = v.parse()?,
                    "bg_yoffset" => room.backgrounds[i].yoffset = v.parse()?,
                    "bg_tile_h" => room.backgrounds[i].tile_horz = v.parse::<u8>()? != 0,
                    "bg_tile_v" => room.backgrounds[i].tile_vert = v.parse::<u8>()? != 0,
                    "bg_hspeed" => room.backgrounds[i].hspeed = v.parse()?,
                    "bg_vspeed" => room.backgrounds[i].vspeed = v.parse()?,
                    "bg_stretch" => room.backgrounds[i].stretch = v.parse::<u8>()? != 0,
                    "view_visible" => room.views[i].visible = v.parse::<u8>()? != 0,
                    "view_xview" => room.views[i].source_x = v.parse()?,
                    "view_yview" => room.views[i].source_y = v.parse()?,
                    "view_wview" => room.views[i].source_w = v.parse()?,
                    "view_hview" => room.views[i].source_h = v.parse()?,
                    "view_xport" => room.views[i].port_x = v.parse()?,
                    "view_yport" => room.views[i].port_y = v.parse()?,
                    "view_wport" => room.views[i].port_w = v.parse()?,
                    "view_hport" => room.views[i].port_h = v.parse()?,
                    "view_fol_hbord" => room.views[i].following_hborder = v.parse()?,
                    "view_fol_vbord" => room.views[i].following_vborder = v.parse()?,
                    "view_fol_hspeed" => {
                        room.views[i].following_hspeed =
                            if let Ok(s) = v.parse() { s } else { v.parse::<u32>()? as i32 };
                    },
                    "view_fol_vspeed" => {
                        room.views[i].following_vspeed =
                            if let Ok(s) = v.parse() { s } else { v.parse::<u32>()? as i32 };
                    },
                    "view_fol_target" => {
                        room.views[i].following_target = if v.is_empty() {
                            -1
                        } else {
                            *asset_maps.objects.map.get(v).ok_or_else(|| Error::AssetNotFound(v.to_string()))? as _
                        }
                    },
                    _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
                }
            },
            "views_enabled" => room.views_enabled = v.parse::<u8>()? != 0,
            // views
            "remember" => room.remember_room_editor_info = v.parse::<u8>()? != 0,
            "editor_width" => room.editor_width = v.parse()?,
            "editor_height" => room.editor_height = v.parse()?,
            "show_grid" => room.show_grid = v.parse::<u8>()? != 0,
            "show_objects" => room.show_objects = v.parse::<u8>()? != 0,
            "show_tiles" => room.show_tiles = v.parse::<u8>()? != 0,
            "show_backgrounds" => room.show_backgrounds = v.parse::<u8>()? != 0,
            "show_foregrounds" => room.show_foregrounds = v.parse::<u8>()? != 0,
            "show_views" => room.show_views = v.parse::<u8>()? != 0,
            "delete_underlying_objects" => room.delete_underlying_objects = v.parse::<u8>()? != 0,
            "delete_underlying_tiles" => room.delete_underlying_tiles = v.parse::<u8>()? != 0,
            "tab" => room.tab = v.parse()?, // i still don't know wtf this is
            "editor_x" => room.x_position_scroll = v.parse()?,
            "editor_y" => room.y_position_scroll = v.parse()?,
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        }
        Ok(())
    })?;
    path.pop();
    path.push("code.gml");
    room.creation_code = load_gml(&read_file(&path)?);
    path.pop();
    load_instances(room, path, &asset_maps.objects.map)?;
    room.put_tiles(load_tiles(path, &asset_maps.backgrounds.map)?);
    room.calc_extents();
    Ok(room)
}

unsafe fn load_constants(path: &mut PathBuf) -> Result<()> {
    path.push("constants.txt");
    let s = read_file(&path)?;
    path.pop();
    let lines: Vec<_> = s.par_lines().collect();
    ide::alloc_constants(lines.len());
    for (line, name_p, value_p) in izip!(lines, ide::get_constant_names_mut(), ide::get_constants_mut()) {
        decode_line(&path, line, &mut |name, value| {
            *name_p = UStr::new(name);
            *value_p = UStr::new(value);
            Ok(())
        })?;
    }
    Ok(())
}

unsafe fn load_included_files(path: &mut PathBuf) -> Result<()> {
    path.push("datafiles");
    path.push("index.yyd");
    let index = read_file(&path)?;
    path.pop();
    let files: Vec<_> = index.par_lines().collect();
    ide::alloc_included_files(files.len());
    for (fname, file_p) in files.iter().zip(ide::get_included_files_mut()) {
        let file = &mut *IncludedFile::new();
        file.file_name = UStr::new(fname);
        path.push(fname.to_string() + ".txt");
        read_txt(&path, |k, v| {
            match k {
                "store" => file.stored_in_gmk = v.parse::<u8>()? != 0,
                "free" => file.free_memory = v.parse::<u8>()? != 0,
                "overwrite" => file.overwrite_file = v.parse::<u8>()? != 0,
                "remove" => file.remove_at_end = v.parse::<u8>()? != 0,
                "export" => file.export_setting = v.parse()?,
                "export_folder" => file.export_custom_folder = UStr::new(v),
                _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
            }
            Ok(())
        })?;
        path.pop();
        path.push("include");
        path.push(fname);
        file.source_path = UStr::new(&path);
        file.source_length =
            std::fs::metadata(&path).map_err(|e| Error::FileIoError(e, path.to_path_buf()))?.len() as _;
        if file.stored_in_gmk {
            verify_path(&path)?;
            file.data_exists = true;
            let data = &mut *delphi::TMemoryStream::new();
            data.load(&UStr::new(&path));
            file.source_length = data.get_size();
            file.data = data;
        }
        path.pop();
        path.pop();
        *file_p = file;
    }
    path.pop();
    Ok(())
}

unsafe fn load_game_information(path: &mut PathBuf) -> Result<()> {
    use ide::game_info::*;
    let editor = &mut *(**FORM).editor;
    path.push("game_information.txt");
    read_txt(&path, |k, v| {
        match k {
            "color" => editor.colour = v.parse()?,
            "new_window" => NEW_WINDOW.write(v.parse::<u8>()? != 0),
            "caption" => CAPTION.asg(v),
            "left" => LEFT.write(v.parse()?),
            "top" => TOP.write(v.parse()?),
            "width" => WIDTH.write(v.parse()?),
            "height" => HEIGHT.write(v.parse()?),
            "border" => BORDER.write(v.parse::<u8>()? != 0),
            "resizable" => RESIZABLE.write(v.parse::<u8>()? != 0),
            "window_on_top" => WINDOW_ON_TOP.write(v.parse::<u8>()? != 0),
            "freeze_game" => FREEZE_GAME.write(v.parse::<u8>()? != 0),
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        }
        Ok(())
    })?;
    path.set_extension("rtf");
    verify_path(&path)?;
    (&mut *editor.rich_edit_strings).LoadFromFile(&UStr::new(&path));
    path.pop();
    Ok(())
}

unsafe fn load_settings(path: &mut PathBuf) -> Result<()> {
    use ide::settings::*;
    path.push("settings");
    load_constants(path)?;
    let mut custom_load_bar = false;
    let mut bar_bg = false;
    let mut bar_fg = false;
    let mut custom_load_bg = false;
    path.push("settings.txt");
    read_txt(&path, |k, v| {
        match k {
            "fullscreen" => FULLSCREEN.write(v.parse::<u8>()? != 0),
            "interpolate_pixels" => INTERPOLATE_PIXELS.write(v.parse::<u8>()? != 0),
            "dont_draw_border" => DONT_DRAW_BORDER.write(v.parse::<u8>()? != 0),
            "display_cursor" => DISPLAY_CURSOR.write(v.parse::<u8>()? != 0),
            "scaling" => SCALING.write(if let Ok(s) = v.parse() { s } else { v.parse::<u32>()? as i32 }),
            "allow_resize" => ALLOW_RESIZE.write(v.parse::<u8>()? != 0),
            "window_on_top" => WINDOW_ON_TOP.write(v.parse::<u8>()? != 0),
            "clear_color" => CLEAR_COLOUR.write(v.parse()?),
            "set_resolution" => SET_RESOLUTION.write(v.parse::<u8>()? != 0),
            "color_depth" => COLOUR_DEPTH.write(v.parse()?),
            "resolution" => RESOLUTION.write(v.parse()?),
            "frequency" => FREQUENCY.write(v.parse()?),
            "dont_show_buttons" => DONT_SHOW_BUTTONS.write(v.parse::<u8>()? != 0),
            "vsync" => *VSYNC_AND_FORCE_CPU |= u32::from(v.parse::<u8>()? != 0),
            "force_cpu_render" => (), // legacy, and saving didn't even work so
            "swap_creation_events" => *VSYNC_AND_FORCE_CPU |= u32::from(v.parse::<u8>()? != 0) << 31,
            "disable_screensaver" => DISABLE_SCREENSAVER.write(v.parse::<u8>()? != 0),
            "f4_fullscreen_toggle" => F4_FULLSCREEN.write(v.parse::<u8>()? != 0),
            "f1_help_menu" => F1_HELP.write(v.parse::<u8>()? != 0),
            "esc_close_game" => ESC_CLOSE.write(v.parse::<u8>()? != 0),
            "f5_save_f6_load" => F5_SAVE_F6_LOAD.write(v.parse::<u8>()? != 0),
            "f9_screenshot" => F9_SCREENSHOT.write(v.parse::<u8>()? != 0),
            "treat_close_as_esc" => TREAT_CLOSE_AS_ESC.write(v.parse::<u8>()? != 0),
            "priority" => PRIORITY.write(v.parse()?),
            "freeze_on_lose_focus" => FREEZE_ON_LOSE_FOCUS.write(v.parse::<u8>()? != 0),
            "custom_loader" => {
                custom_load_bg = v.parse::<u8>()? != 0;
                HAS_CUSTOM_LOAD_IMAGE.write(custom_load_bg);
            },
            "custom_bar" => {
                let bar = v.parse()?;
                custom_load_bar = bar == 2;
                LOADING_BAR.write(bar);
            },
            "bar_has_bg" => bar_bg = v.parse::<u8>()? != 0,
            "bar_has_fg" => bar_fg = v.parse::<u8>()? != 0,
            "transparent" => LOADING_TRANSPARENT.write(v.parse::<u8>()? != 0),
            "translucency" => LOADING_TRANSLUCENCY.write(v.parse()?),
            "scale_progress_bar" => LOADING_PROGRESS_BAR_SCALE.write(v.parse::<u8>()? != 0),
            "show_error_messages" => SHOW_ERROR_MESSAGES.write(v.parse::<u8>()? != 0),
            "log_errors" => LOG_ERRORS.write(v.parse::<u8>()? != 0),
            "always_abort" => ALWAYS_ABORT.write(v.parse::<u8>()? != 0),
            "zero_uninitialized_vars" => ZERO_UNINITIALIZED_VARS.write(v.parse::<u8>()? != 0),
            "error_on_uninitialized_args" => ERROR_ON_UNINITIALIZED_ARGS.write(v.parse::<u8>()? != 0),
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        }
        Ok(())
    })?;
    path.pop();
    if custom_load_bar {
        if bar_bg {
            path.push("back.bmp");
            verify_path(&path)?;
            let bg = &mut *delphi::TBitmap::new();
            bg.LoadFromFile(&UStr::new(&path));
            *LOADING_BACKGROUND = bg;
            path.pop();
        }
        if bar_fg {
            path.push("front.bmp");
            verify_path(&path)?;
            let fg = &mut *delphi::TBitmap::new();
            fg.LoadFromFile(&UStr::new(&path));
            *LOADING_FOREGROUND = fg;
            path.pop();
        }
    }
    if custom_load_bg {
        path.push("loader.bmp");
        let im = &mut *delphi::TBitmap::new();
        im.LoadFromFile(&UStr::new(&path));
        *CUSTOM_LOAD_IMAGE = im;
        path.pop();
    }
    path.push("icon.ico");
    verify_path(&path)?;
    (&mut **ICON).LoadFromFile(&UStr::new(&path));
    path.pop();
    {
        path.push("extensions.txt");
        let f = open_file(&path)?;
        let mut any = false;
        for line in f.lines() {
            let name = line?;
            if let Some((_, loaded)) = ide::get_extensions()
                .iter()
                .zip(ide::get_extensions_loaded_mut())
                .find(|(ex, _)| ex.name.to_os_string() == OsStr::new(&name))
            {
                *loaded = true;
                any = true;
            } else {
                crate::show_message(&format!("Cannot find extension package: {}", name));
            }
        }
        if any {
            // reload action libraries, including the extensions
            let _: u32 = delphi_call!(0x7149c4);
        }
        path.pop();
    }
    load_game_information(path)?;
    path.pop();
    Ok(())
}

fn load_index(name: &str, path: &mut PathBuf) -> Result<Assets> {
    path.push(name);
    path.push("index.yyd");
    let text = read_file(&path)?;
    let index: Vec<_> = text.par_lines().map(String::from).collect();
    let map = index.par_iter().enumerate().filter_map(|(i, s)| (!s.is_empty()).then(|| (s.to_string(), i))).collect();
    path.pop();
    path.pop();
    Ok(Assets { index, map })
}

unsafe fn load_assets<'a, T: Sync>(
    name: &str,
    load_asset: unsafe fn(&mut PathBuf, &AssetMaps) -> Result<*const T>,
    get_assets: fn() -> &'a mut [Option<&'a T>],
    get_names: fn() -> &'a mut [UStr],
    alloc: fn(usize),
    assets: &Assets,
    bar_start: u32,
    bar_end: u32,
    path: &mut PathBuf,
    asset_maps: &AssetMaps,
) -> Result<()> {
    path.push(name);
    let names = &assets.index;
    alloc(names.len());
    if name != "rooms" {
        run_while_updating_bar(bar_start, bar_end, names.len() as u32, |tx| {
            names.par_iter().zip(get_assets()).zip(get_names()).try_for_each(|((name, asset), name_p)| -> Result<()> {
                if !name.is_empty() {
                    *name_p = UStr::new(name);
                    *asset = load_asset(&mut path.join(name), asset_maps)?.as_ref();
                }
                let _ = tx.send(());
                Ok(())
            })
        })?;
    } else {
        run_while_updating_bar(bar_start, bar_end, names.len() as u32, |tx| {
            names.iter().zip(get_assets()).zip(get_names()).try_for_each(|((name, asset), name_p)| -> Result<()> {
                if !name.is_empty() {
                    *name_p = UStr::new(name);
                    *asset = load_asset(&mut path.join(name), asset_maps)?.as_ref();
                }
                let _ = tx.send(());
                Ok(())
            })
        })?;
    }
    path.pop();
    Ok(())
}

pub fn load_asset_maps(path: &mut PathBuf) -> Result<AssetMaps> {
    Ok(AssetMaps {
        triggers: load_index("triggers", path)?,
        sprites: load_index("sprites", path)?,
        sounds: load_index("sounds", path)?,
        backgrounds: load_index("backgrounds", path)?,
        paths: load_index("paths", path)?,
        scripts: load_index("scripts", path)?,
        objects: load_index("objects", path)?,
        rooms: load_index("rooms", path)?,
        fonts: load_index("fonts", path)?,
        timelines: load_index("timelines", path)?,
    })
}

pub unsafe fn load_gmk(mut path: PathBuf) -> Result<()> {
    ide::initialize_project();
    PATH_FORM_UPDATED = false;
    EXTRA_DATA = Some(Default::default());
    read_txt(&path, |k, v| {
        match k {
            "gm82_version" => {
                if v.parse::<u8>()? > 4 {
                    return Err(Error::OldGM82)
                }
            },
            "gameid" => ide::GAME_ID.write(v.parse()?),
            "info_author" => ide::settings::INFO_AUTHOR.asg(v),
            "info_version" => ide::settings::INFO_VERSION.asg(v),
            "info_timestamp" => ide::settings::INFO_TIMESTAMP.write(v.parse()?), // legacy
            "info_information" => ide::settings::INFO_INFORMATION.asg_undelimit(v),
            "exe_company" => ide::settings::EXE_COMPANY.asg(v),
            "exe_product" => ide::settings::EXE_PRODUCT.asg(v),
            "exe_copyright" => ide::settings::EXE_COPYRIGHT.asg(v),
            "exe_description" => ide::settings::EXE_DESCRIPTION.asg(v),
            "exe_version" => {
                let err = || Error::InvalidVersion(v.to_string());
                let mut iter = v.split('.');
                ide::settings::VERSION_MAJOR.write(iter.next().ok_or_else(err)?.parse()?);
                ide::settings::VERSION_MINOR.write(iter.next().ok_or_else(err)?.parse()?);
                ide::settings::VERSION_RELEASE.write(iter.next().ok_or_else(err)?.parse()?);
                ide::settings::VERSION_BUILD.write(iter.next().ok_or_else(err)?.parse()?);
                if iter.next() != None {
                    return Err(Error::InvalidVersion(v.to_string()))
                }
            },
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        }
        Ok(())
    })?;
    path.pop();
    advance_progress_form(5);
    load_settings(&mut path)?;
    advance_progress_form(10);
    let asset_maps = load_asset_maps(&mut path)?;
    advance_progress_form(15);
    load_triggers(&asset_maps, &mut path)?;
    advance_progress_form(20);
    load_assets(
        "sounds",
        load_sound,
        ide::get_sounds_mut,
        ide::get_sound_names_mut,
        ide::alloc_sounds,
        &asset_maps.sounds,
        20,
        30,
        &mut path,
        &asset_maps,
    )?;
    advance_progress_form(30);
    load_assets(
        "sprites",
        load_sprite,
        ide::get_sprites_mut,
        ide::get_sprite_names_mut,
        ide::alloc_sprites,
        &asset_maps.sprites,
        30,
        40,
        &mut path,
        &asset_maps,
    )?;
    advance_progress_form(40);
    load_assets(
        "backgrounds",
        load_background,
        ide::get_backgrounds_mut,
        ide::get_background_names_mut,
        ide::alloc_backgrounds,
        &asset_maps.backgrounds,
        55,
        60,
        &mut path,
        &asset_maps,
    )?;
    advance_progress_form(45);
    // register sprite icons
    let bg_col = (*ide::RESOURCE_TREE.read()).color;
    let mut last_refresh = std::time::Instant::now();
    let sprites_len = ide::get_sprites().len();
    for (i, (sp, thumb)) in ide::get_sprites().iter().zip(ide::get_sprite_thumbs_mut()).enumerate() {
        if let Some(frame) =
            sp.and_then(|sp| sp.get_frames().get(0)).and_then(|f| f.as_ref()).filter(|f| f.width != 0 && f.height != 0)
        {
            *thumb = frame.register_thumb(bg_col);
            if last_refresh.elapsed() > std::time::Duration::from_secs(1) {
                advance_progress_form((i * 15 / sprites_len + 45) as u32);
                last_refresh = std::time::Instant::now();
            }
        } else {
            *thumb = -1;
        }
    }
    advance_progress_form(60);
    // register background icons
    let mut last_refresh = std::time::Instant::now();
    let bg_len = ide::get_backgrounds().len();
    for (i, (bg, thumb)) in ide::get_backgrounds().iter().zip(ide::get_background_thumbs_mut()).enumerate() {
        if let Some(frame) = bg.and_then(|bg| bg.frame.as_ref()).filter(|f| f.width != 0 && f.height != 0) {
            *thumb = frame.register_thumb(bg_col);
            if last_refresh.elapsed() > std::time::Duration::from_secs(1) {
                advance_progress_form((i * 5 / bg_len + 60) as u32);
                last_refresh = std::time::Instant::now();
            }
        } else {
            *thumb = -1;
        }
    }
    // image list OnChange
    let _: u32 = delphi_call!(0x5081b8, *(0x789b38 as *const usize));
    advance_progress_form(65);
    load_assets(
        "paths",
        load_path,
        ide::get_paths_mut,
        ide::get_path_names_mut,
        ide::alloc_paths,
        &asset_maps.paths,
        65,
        70,
        &mut path,
        &asset_maps,
    )?;
    advance_progress_form(70);
    load_assets(
        "scripts",
        load_script,
        ide::get_scripts_mut,
        ide::get_script_names_mut,
        ide::alloc_scripts,
        &asset_maps.scripts,
        70,
        75,
        &mut path,
        &asset_maps,
    )?;
    advance_progress_form(75);
    load_assets(
        "fonts",
        load_font,
        ide::get_fonts_mut,
        ide::get_font_names_mut,
        ide::alloc_fonts,
        &asset_maps.fonts,
        75,
        80,
        &mut path,
        &asset_maps,
    )?;
    advance_progress_form(80);
    load_assets(
        "timelines",
        load_timeline,
        ide::get_timelines_mut,
        ide::get_timeline_names_mut,
        ide::alloc_timelines,
        &asset_maps.timelines,
        80,
        85,
        &mut path,
        &asset_maps,
    )?;
    advance_progress_form(85);
    load_assets(
        "objects",
        load_object,
        ide::get_objects_mut,
        ide::get_object_names_mut,
        ide::alloc_objects,
        &asset_maps.objects,
        85,
        90,
        &mut path,
        &asset_maps,
    )?;
    advance_progress_form(90);
    load_assets(
        "rooms",
        load_room,
        ide::get_rooms_mut,
        ide::get_room_names_mut,
        ide::alloc_rooms,
        &asset_maps.rooms,
        90,
        95,
        &mut path,
        &asset_maps,
    )?;
    advance_progress_form(95);
    load_included_files(&mut path)?;

    read_resource_tree(ide::RT_SOUNDS, 3, "sounds", &asset_maps.sounds.map, true, &mut path)?;
    read_resource_tree(ide::RT_SPRITES, 2, "sprites", &asset_maps.sprites.map, true, &mut path)?;
    read_resource_tree(ide::RT_BACKGROUNDS, 6, "backgrounds", &asset_maps.backgrounds.map, true, &mut path)?;
    read_resource_tree(ide::RT_PATHS, 8, "paths", &asset_maps.paths.map, true, &mut path)?;
    read_resource_tree(ide::RT_SCRIPTS, 7, "scripts", &asset_maps.scripts.map, true, &mut path)?;
    read_resource_tree(ide::RT_FONTS, 9, "fonts", &asset_maps.fonts.map, true, &mut path)?;
    read_resource_tree(ide::RT_TIMELINES, 12, "timelines", &asset_maps.timelines.map, true, &mut path)?;
    read_resource_tree(ide::RT_OBJECTS, 1, "objects", &asset_maps.objects.map, true, &mut path)?;
    read_resource_tree(ide::RT_ROOMS, 4, "rooms", &asset_maps.rooms.map, true, &mut path)?;

    // this is the part where i set all the updated flags to false
    // i don't feel like doing it nicely so enjoy
    for ptr in [
        0x77f5f4, // resource tree
        0x78a0d4, // objects
        0x78a1b0, // sounds
        0x790170, // sprites
        0x78a1f8, // rooms
        0x78a168, // backgrounds
        0x7a4658, // paths
        0x78a1b8, // scripts
        0x790190, // fonts
        0x790188, // timelines
        0x78895c, // gameinfo
        0x790824, // settings
        0x790a0c, // extensions
        0x7900a0, // included files
        0x790058, // triggers
        0x78c154, // constants
    ]
    .iter()
    .copied()
    {
        (ptr as *mut bool).write(false);
    }
    advance_progress_form(100);

    update_timestamp();

    Ok(())
}
