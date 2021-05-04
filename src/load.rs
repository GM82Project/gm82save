use crate::{asset::*, delphi, delphi::UStr, events, ide, Error, Result, ACTION_TOKEN};
use itertools::izip;
use rayon::prelude::*;
use std::{
    collections::HashMap,
    ffi::OsStr,
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
    slice,
};

trait UStrPtr {
    fn asg(self, s: &str);
    fn asg_undelimit(self, s: &str);
}

impl UStrPtr for *mut UStr {
    fn asg(self, s: &str) {
        unsafe {
            delphi::UStrAsg(self.as_mut().unwrap(), &UStr::new(s.as_ref()));
        }
    }

    fn asg_undelimit(self, s: &str) {
        self.asg(&s.replace("*\\/", "*/").replace("\\n", "\n").replace("\\r", "\r").replace("\\\\", "\\"));
    }
}

struct Assets {
    index: Vec<String>,
    map: HashMap<String, usize>,
}
struct AssetMaps {
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
    Ok(BufReader::new(File::open(path)?))
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
    base: *const delphi::TTreeNode,
    path: &std::path::Path,
    kind: u32,
    names: &HashMap<String, usize>,
    visible: bool,
) -> Result<()> {
    let f = open_file(path)?;
    let nodes = &*((&**if visible { ide::RESOURCE_TREE } else { ide::RESOURCE_TREE_HIDDEN }).nodes);
    let mut stack = vec![base];
    for line in f.lines() {
        let line = line?;
        if line.is_empty() {
            continue
        }
        let trimmed = line.trim_start();
        let level = trimmed.len() - line.len();
        stack.truncate(level + 1);
        let rtype = match trimmed.chars().next() {
            Some('+') => 2,
            Some('|') => 3,
            _ => return Err(Error::SyntaxError(path.to_path_buf())),
        };
        let name = &trimmed[1..];
        let index =
            if rtype == 3 { *names.get(name).ok_or_else(|| Error::AssetNotFound(name.to_string()))? } else { 0 };

        let node = &*nodes.AddChild(*stack.last().unwrap(), &UStr::new(name.as_ref()));
        node.SetData(delphi::TreeNodeData::new(rtype, kind, index as u32));
        node.SetImageIndex(1);
        if rtype == 2 {
            stack.push(node);
        }
    }
    Ok(())
}

unsafe fn load_triggers(maps: &AssetMaps, path: &mut PathBuf) -> Result<()> {
    path.push("triggers");
    let names = &maps.triggers.index;
    ide::alloc_triggers(names.len());
    for (name, trig_p) in names.iter().zip(ide::get_triggers_mut()) {
        let trig = &mut *Trigger::new();
        trig.name = UStr::new(name.as_ref());
        path.push(name);
        path.set_extension("txt");
        read_txt(&path, |k, v| {
            Ok(match k {
                "constant" => trig.constant_name = UStr::new(v.as_ref()),
                "kind" => trig.kind = v.parse()?,
                _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
            })
        })?;
        path.set_extension("gml");
        trig.condition = UStr::new(std::fs::read_to_string(&path)?.as_ref());
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
        Err(Error::IoError(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("file {} not found", path.to_string_lossy()),
        )))
    }
}

unsafe fn load_sound(path: &mut PathBuf, _asset_maps: &AssetMaps) -> Result<*const Sound> {
    let snd = &mut *Sound::new();
    path.set_extension("txt");
    let mut extension = String::new();
    let mut exists = false;
    read_txt(&path, |k, v| {
        Ok(match k {
            "extension" => {
                extension = v.to_string();
                snd.extension = UStr::new(v.as_ref())
            },
            "exists" => exists = v.parse::<u8>()? != 0,
            "kind" => snd.kind = v.parse()?,
            "effects" => snd.effects = v.parse()?,
            "volume" => snd.volume = v.parse()?,
            "pan" => snd.pan = v.parse()?,
            "preload" => snd.preload = v.parse::<u8>()? != 0,
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        })
    })?;
    if exists {
        path.set_extension(extension.trim_matches('.'));
        verify_path(&path)?;
        let data = &mut *delphi::TMemoryStream::new();
        data.load(&UStr::new(path.as_ref()));
        snd.data = data;
    }
    Ok(snd)
}

unsafe fn load_frame(path: &std::path::Path, frame: &mut Frame) -> Result<()> {
    let im = image::open(path)?.into_rgba8();
    frame.width = im.width();
    frame.height = im.height();
    let data = im.as_raw();
    let data_ptr = delphi::GetMem(data.len());
    data.as_ptr().copy_to_nonoverlapping(data_ptr, data.len());
    frame.data = data_ptr;
    Ok(())
}

unsafe fn load_background(path: &mut PathBuf, _asset_maps: &AssetMaps) -> Result<*const Background> {
    let bg = &mut *Background::new();
    path.set_extension("txt");
    let mut bg_exists = false;
    read_txt(path, |k, v| {
        Ok(match k {
            "exists" => bg_exists = v.parse::<u8>()? != 0,
            "tileset" => bg.is_tileset = v.parse::<u8>()? != 0,
            "tile_width" => bg.tile_width = v.parse()?,
            "tile_height" => bg.tile_height = v.parse()?,
            "tile_hoffset" => bg.h_offset = v.parse()?,
            "tile_voffset" => bg.v_offset = v.parse()?,
            "tile_hsep" => bg.h_sep = v.parse()?,
            "tile_vsep" => bg.v_sep = v.parse()?,
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        })
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
        Ok(match k {
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
        })
    })?;
    path.pop();
    delphi::DynArraySetLength(&mut sp.frames, 0x5b2754 as *const u8, 1, sp.frame_count as _);
    let frames = slice::from_raw_parts_mut(sp.frames, sp.frame_count as _);
    for (i, f) in frames.iter_mut().enumerate() {
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
    (&mut *s).source = UStr::new(std::fs::read_to_string(path)?.as_ref());
    Ok(s)
}

unsafe fn load_font(path: &mut PathBuf, _asset_maps: &AssetMaps) -> Result<*const Font> {
    let f = &mut *Font::new();
    path.set_extension("txt");
    read_txt(path, |k, v| {
        Ok(match k {
            "name" => f.sys_name = UStr::new(v.as_ref()),
            "size" => f.size = v.parse()?,
            "bold" => f.bold = v.parse::<u8>()? != 0,
            "italic" => f.italic = v.parse::<u8>()? != 0,
            "charset" => f.charset = v.parse()?,
            "aa_level" => f.aa_level = v.parse()?,
            "range_start" => f.range_start = v.parse()?,
            "range_end" => f.range_end = v.parse()?,
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        })
    })?;
    Ok(f)
}

unsafe fn load_event(
    path: &std::path::Path,
    event: &mut Event,
    event_code: &str,
    objs: &HashMap<String, usize>,
) -> Result<()> {
    for action_code in event_code.split(ACTION_TOKEN) {
        if action_code.trim().is_empty() {
            continue
        }
        let (params, code) = action_code.split_once("*/").ok_or_else(|| Error::SyntaxError(path.to_path_buf()))?;
        let action = &mut *event.add_action(0, 0);
        for line in params.lines() {
            decode_line(path, line, &mut |k, v| {
                Ok(match k {
                    "lib_id" => action.lib_id = v.parse()?,
                    "action_id" => action.id = v.parse()?,
                    "relative" => action.is_relative = v.parse::<u8>()? != 0,
                    "applies_to" => {
                        action.applies_to = match v {
                            "other" => -2,
                            "self" => -1,
                            name => *objs.get(name).ok_or_else(|| Error::AssetNotFound(name.to_string()))? as _,
                        }
                    },
                    "invert" => action.invert_condition = v.parse::<u8>()? != 0,
                    "arg0" | "var_name" | "repeats" => action.param_strings[0] = UStr::new(v.as_ref()),
                    "arg1" | "var_value" => action.param_strings[1] = UStr::new(v.as_ref()),
                    "arg2" => action.param_strings[2] = UStr::new(v.as_ref()),
                    "arg3" => action.param_strings[3] = UStr::new(v.as_ref()),
                    "arg4" => action.param_strings[4] = UStr::new(v.as_ref()),
                    "arg5" => action.param_strings[5] = UStr::new(v.as_ref()),
                    "arg6" => action.param_strings[6] = UStr::new(v.as_ref()),
                    "arg7" => action.param_strings[7] = UStr::new(v.as_ref()),
                    _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
                })
            })?;
        }
        // manually check if action exists so we can throw an error
        if ide::get_action_libraries()
            .iter()
            .find(|&l| {
                l.id == action.lib_id
                    && slice::from_raw_parts(l.actions, l.action_count).iter().find(|&a| a.id == action.id).is_some()
            })
            .is_some()
        {
            action.fill_in(action.lib_id, action.id);
        } else {
            return Err(Error::UnknownAction(action.lib_id, action.id))
        }
        if action.action_kind == 7 {
            // first character will always be LF because it doesn't cut the newline when searching for */
            // so skip it
            action.param_strings[0] = UStr::new(code[1..].as_ref());
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
        Ok(match k {
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
        })
    })?;
    path.set_extension("gml");
    let code = std::fs::read_to_string(&path)?;
    for event in code.trim_start_matches("#define ").split("\n#define ") {
        if event.trim().is_empty() {
            continue
        }
        let err = || Error::SyntaxError(path.to_path_buf());
        let (name, actions) = event.split_once("\n").ok_or_else(err)?;
        let (ev_type_s, ev_numb_s) = name.trim().split_once("_").ok_or_else(err)?;
        let ev_type = *events::EVENT_NAME_TO_ID.get(ev_type_s).ok_or_else(err)?;
        let ev_numb = match ev_type {
            events::EV_COLLISION => *object_map.get(ev_numb_s).ok_or_else(err)?,
            events::EV_TRIGGER => *trigger_map.get(ev_numb_s).ok_or_else(err)?,
            _ => ev_numb_s.parse()?,
        };
        let event = &mut *obj.get_event(ev_type, ev_numb);
        load_event(&path, event, actions, object_map)?;
    }
    Ok(obj)
}

unsafe fn load_timeline(path: &mut PathBuf, asset_maps: &AssetMaps) -> Result<*const Timeline> {
    let object_map = &asset_maps.objects.map;
    let tl = &mut *Timeline::new();
    path.set_extension("gml");
    let code = std::fs::read_to_string(&path)?;
    let iter = code.trim_start_matches("#define ").split("\n#define ");
    tl.alloc(iter.clone().count());
    let times = slice::from_raw_parts_mut(tl.moment_times, tl.moment_count);
    let events = slice::from_raw_parts_mut(tl.moment_events, tl.moment_count);
    for (code, time_p, event_p) in izip!(iter, times, events) {
        if code.trim().is_empty() {
            continue
        }
        let err = || Error::SyntaxError(path.to_path_buf());
        let (name, actions) = code.split_once("\n").ok_or_else(err)?;
        *time_p = name.trim().parse()?;
        let event = Event::new();
        load_event(&path, &mut *event, actions, object_map)?;
        *event_p = event;
    }
    Ok(tl)
}

unsafe fn load_constants(path: &mut PathBuf) -> Result<()> {
    path.push("constants.txt");
    let s = std::fs::read_to_string(&path)?;
    path.pop();
    let lines: Vec<_> = s.par_lines().collect();
    ide::alloc_constants(lines.len());
    for (line, name_p, value_p) in izip!(lines, ide::get_constant_names_mut(), ide::get_constants_mut()) {
        decode_line(&path, line, &mut |name, value| {
            *name_p = UStr::new(name.as_ref());
            *value_p = UStr::new(value.as_ref());
            Ok(())
        })?;
    }
    Ok(())
}

unsafe fn load_included_files(path: &mut PathBuf) -> Result<()> {
    path.push("datafiles");
    path.push("index.yyd");
    let index = std::fs::read_to_string(&path)?;
    path.pop();
    let files: Vec<_> = index.par_lines().collect();
    ide::alloc_included_files(files.len());
    for (fname, file_p) in files.iter().zip(ide::get_included_files_mut()) {
        let file = &mut *IncludedFile::new();
        file.file_name = UStr::new(fname.as_ref());
        path.push(fname.to_string() + ".txt");
        read_txt(&path, |k, v| {
            Ok(match k {
                "store" => file.stored_in_gmk = v.parse::<u8>()? != 0,
                "free" => file.free_memory = v.parse::<u8>()? != 0,
                "overwrite" => file.overwrite_file = v.parse::<u8>()? != 0,
                "remove" => file.remove_at_end = v.parse::<u8>()? != 0,
                "export" => file.export_setting = v.parse()?,
                "export_folder" => file.export_custom_folder = UStr::new(v.as_ref()),
                _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
            })
        })?;
        path.pop();
        path.push("include");
        path.push(fname);
        file.source_path = UStr::new(path.as_ref());
        file.source_length = std::fs::metadata(&path)?.len() as _;
        if file.stored_in_gmk {
            verify_path(&path)?;
            file.data_exists = true;
            let data = &mut *delphi::TMemoryStream::new();
            data.load(&UStr::new(path.as_ref()));
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
    let editor = &mut *(&**FORM).editor;
    path.push("game_information.txt");
    read_txt(&path, |k, v| {
        Ok(match k {
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
        })
    })?;
    path.set_extension("rtf");
    verify_path(&path)?;
    (&mut *editor.rich_edit_strings).LoadFromFile(&UStr::new(path.as_ref()));
    path.pop();
    Ok(())
}

unsafe fn load_settings(path: &mut PathBuf) -> Result<()> {
    use ide::settings::*;
    path.push("settings");
    load_constants(path)?;
    let mut custom_load_bar = false;
    let mut custom_load_bg = false;
    path.push("settings.txt");
    // TODO
    read_txt(&path, |k, v| {
        Ok(match k {
            "fullscreen" => FULLSCREEN.write(v.parse::<u8>()? != 0),
            "interpolate_pixels" => INTERPOLATE_PIXELS.write(v.parse::<u8>()? != 0),
            "dont_draw_border" => DONT_DRAW_BORDER.write(v.parse::<u8>()? != 0),
            "display_cursor" => DISPLAY_CURSOR.write(v.parse::<u8>()? != 0),
            "scaling" => SCALING.write(v.parse()?),
            "allow_resize" => ALLOW_RESIZE.write(v.parse::<u8>()? != 0),
            "window_on_top" => WINDOW_ON_TOP.write(v.parse::<u8>()? != 0),
            "clear_color" => CLEAR_COLOUR.write(v.parse()?),
            "set_resolution" => SET_RESOLUTION.write(v.parse::<u8>()? != 0),
            "color_depth" => COLOUR_DEPTH.write(v.parse()?),
            "resolution" => RESOLUTION.write(v.parse()?),
            "frequency" => FREQUENCY.write(v.parse()?),
            "dont_show_buttons" => DONT_SHOW_BUTTONS.write(v.parse::<u8>()? != 0),
            "vsync" => *VSYNC_AND_FORCE_CPU |= 1,
            "force_cpu_render" => *VSYNC_AND_FORCE_CPU |= 1 << 7,
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
                custom_load_bg = true;
                HAS_CUSTOM_LOAD_IMAGE.write(v.parse::<u8>()? != 0);
            },
            "custom_bar" => {
                let bar = v.parse()?;
                custom_load_bar = bar == 2;
                LOADING_BAR.write(bar);
            },
            "transparent" => LOADING_TRANSPARENT.write(v.parse::<u8>()? != 0),
            "translucency" => LOADING_TRANSLUCENCY.write(v.parse()?),
            "scale_progress_bar" => LOADING_PROGRESS_BAR_SCALE.write(v.parse::<u8>()? != 0),
            "show_error_messages" => SHOW_ERROR_MESSAGES.write(v.parse::<u8>()? != 0),
            "log_errors" => LOG_ERRORS.write(v.parse::<u8>()? != 0),
            "always_abort" => ALWAYS_ABORT.write(v.parse::<u8>()? != 0),
            "zero_uninitialized_vars" => ZERO_UNINITIALIZED_VARS.write(v.parse::<u8>()? != 0),
            "error_on_uninitialized_args" => ERROR_ON_UNINITIALIZED_ARGS.write(v.parse::<u8>()? != 0),
            _ => return Err(Error::UnknownKey(path.to_path_buf(), k.to_string())),
        })
    })?;
    path.pop();
    if custom_load_bar {
        path.push("back.bmp");
        verify_path(&path)?;
        let bg = &mut *delphi::TBitmap::new();
        bg.LoadFromFile(&UStr::new(path.as_ref()));
        *LOADING_BACKGROUND = bg;
        path.pop();
        path.push("front.bmp");
        verify_path(&path)?;
        let fg = &mut *delphi::TBitmap::new();
        fg.LoadFromFile(&UStr::new(path.as_ref()));
        *LOADING_FOREGROUND = fg;
        path.pop();
    }
    if custom_load_bg {
        path.push("loader.bmp");
        let im = &mut *delphi::TBitmap::new();
        im.LoadFromFile(&UStr::new(path.as_ref()));
        *CUSTOM_LOAD_IMAGE = im;
        path.pop();
    }
    path.push("icon.ico");
    verify_path(&path)?;
    (&mut **ICON).LoadFromFile(&UStr::new(path.as_ref()));
    path.pop();
    {
        path.push("extensions.txt");
        let f = open_file(&path)?;
        for line in f.lines() {
            let name = line?;
            if let Some((_, loaded)) = ide::get_extensions()
                .iter()
                .zip(ide::get_extensions_loaded_mut())
                .find(|(ex, _)| ex.name.to_os_string() == OsStr::new(&name))
            {
                *loaded = true;
            } else {
                crate::show_message(&format!("Cannot find extension package: {}", name));
            }
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
    let text = std::fs::read_to_string(&path)?;
    let index: Vec<_> = text.par_lines().map(String::from).collect();
    let map = index.par_iter().enumerate().filter_map(|(i, s)| (!s.is_empty()).then(|| (s.to_string(), i))).collect();
    path.pop();
    path.pop();
    Ok(Assets { index, map })
}

unsafe fn load_assets<'a, T: Sync>(
    name: &str,
    kind: u32,
    node: *const *const delphi::TTreeNode,
    load_asset: unsafe fn(&mut PathBuf, &AssetMaps) -> Result<*const T>,
    get_assets: fn() -> &'a mut [Option<&'a T>],
    get_names: fn() -> &'a mut [UStr],
    alloc: fn(usize),
    assets: &Assets,
    path: &mut PathBuf,
    asset_maps: &AssetMaps,
) -> Result<()> {
    path.push(name);
    let names = &assets.index;
    alloc(names.len());
    for (name, asset, name_p) in izip!(names, get_assets(), get_names()) {
        if !name.is_empty() {
            *name_p = UStr::new(name.as_ref());
            path.push(name);
            *asset = load_asset(path, asset_maps)?.as_ref();
            path.pop();
        }
    }
    path.push("tree.yyd");
    read_resource_tree(node.read(), &path, kind, &assets.map, true)?;
    path.pop();
    path.pop();
    Ok(())
}

pub unsafe fn load_gmk(mut path: PathBuf) -> Result<()> {
    ide::initialize_project();
    read_txt(&path, |k, v| {
        Ok(match k {
            "gameid" => ide::GAME_ID.write(v.parse()?),
            "info_author" => ide::settings::INFO_AUTHOR.asg(v),
            "info_version" => ide::settings::INFO_VERSION.asg(v),
            "info_timestamp" => ide::settings::INFO_TIMESTAMP.write(v.parse()?),
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
        })
    })?;
    path.pop();
    load_settings(&mut path)?;
    let asset_maps = AssetMaps {
        triggers: load_index("triggers", &mut path)?,
        sprites: load_index("sprites", &mut path)?,
        sounds: load_index("sounds", &mut path)?,
        backgrounds: load_index("backgrounds", &mut path)?,
        paths: load_index("paths", &mut path)?,
        scripts: load_index("scripts", &mut path)?,
        objects: load_index("objects", &mut path)?,
        rooms: load_index("rooms", &mut path)?,
        fonts: load_index("fonts", &mut path)?,
        timelines: load_index("timelines", &mut path)?,
    };
    load_triggers(&asset_maps, &mut path)?;
    load_assets(
        "sounds",
        3,
        ide::RT_SOUNDS,
        load_sound,
        ide::get_sounds_mut,
        ide::get_sound_names_mut,
        ide::alloc_sounds,
        &asset_maps.sounds,
        &mut path,
        &asset_maps,
    )?;
    load_assets(
        "sprites",
        2,
        ide::RT_SPRITES,
        load_sprite,
        ide::get_sprites_mut,
        ide::get_sprite_names_mut,
        ide::alloc_sprites,
        &asset_maps.sprites,
        &mut path,
        &asset_maps,
    )?;
    for (sp, thumb) in ide::get_sprites().iter().zip(ide::get_sprite_thumbs_mut()) {
        if let Some(sp) = sp {
            *thumb = delphi_call!(0x5a9c14, sp.get_icon());
        } else {
            *thumb = -1;
        }
    }
    load_assets(
        "backgrounds",
        6,
        ide::RT_BACKGROUNDS,
        load_background,
        ide::get_backgrounds_mut,
        ide::get_background_names_mut,
        ide::alloc_backgrounds,
        &asset_maps.backgrounds,
        &mut path,
        &asset_maps,
    )?;
    for (bg, thumb) in ide::get_backgrounds().iter().zip(ide::get_background_thumbs_mut()) {
        if let Some(bg) = bg {
            *thumb = delphi_call!(0x5a9c14, bg.get_icon());
        } else {
            *thumb = -1;
        }
    }
    load_assets(
        "scripts",
        7,
        ide::RT_SCRIPTS,
        load_script,
        ide::get_scripts_mut,
        ide::get_script_names_mut,
        ide::alloc_scripts,
        &asset_maps.scripts,
        &mut path,
        &asset_maps,
    )?;
    load_assets(
        "fonts",
        9,
        ide::RT_FONTS,
        load_font,
        ide::get_fonts_mut,
        ide::get_font_names_mut,
        ide::alloc_fonts,
        &asset_maps.fonts,
        &mut path,
        &asset_maps,
    )?;
    load_assets(
        "objects",
        1,
        ide::RT_OBJECTS,
        load_object,
        ide::get_objects_mut,
        ide::get_object_names_mut,
        ide::alloc_objects,
        &asset_maps.objects,
        &mut path,
        &asset_maps,
    )?;
    load_assets(
        "timelines",
        12,
        ide::RT_TIMELINES,
        load_timeline,
        ide::get_timelines_mut,
        ide::get_timeline_names_mut,
        ide::alloc_timelines,
        &asset_maps.timelines,
        &mut path,
        &asset_maps,
    )?;
    load_included_files(&mut path)?;
    Ok(())
}
