use crate::{asset::*, delphi, delphi::UStr, ide, Error, Result};
use itertools::izip;
use rayon::prelude::*;
use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
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

#[derive(Default)]
struct AssetMaps {
    sprites: Option<HashMap<String, usize>>,     // req for objects
    backgrounds: Option<HashMap<String, usize>>, // req for rooms
    objects: Option<HashMap<String, usize>>,     // req for rooms
    rooms: Option<HashMap<String, usize>>,       // req for paths
}

fn open_file(path: &std::path::Path) -> Result<BufReader<File>> {
    Ok(BufReader::new(File::open(path)?))
}

fn read_txt<F: FnMut(&str, &str) -> Result<()>>(path: &std::path::Path, mut func: F) -> Result<()> {
    let f = open_file(path)?;
    for line in f.lines() {
        let line = line?;
        if !line.is_empty() {
            let (key, value) = line.split_once('=').ok_or_else(|| Error::SyntaxError(path.to_path_buf()))?;
            func(key, value)?;
        }
    }
    Ok(())
}

unsafe fn read_resource_tree(
    base: *const delphi::TTreeNode,
    path: &std::path::Path,
    kind: u32,
    names: Vec<&str>,
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
        let index = if rtype == 3 {
            names.par_iter().position_any(|&s| s == name).ok_or_else(|| Error::AssetNotFound(name.to_string()))?
        } else {
            0
        };

        let node = &*nodes.AddChild(*stack.last().unwrap(), &UStr::new(name.as_ref()));
        node.SetData(delphi::TreeNodeData::new(rtype, kind, index as u32));
        node.SetImageIndex(1);
        if rtype == 2 {
            stack.push(node);
        }
    }
    Ok(())
}

unsafe fn load_script(path: &mut PathBuf) -> Result<*const Script> {
    path.set_extension("gml");
    let s = Script::new();
    (&mut *s).source = UStr::new(std::fs::read_to_string(path)?.as_ref());
    Ok(s)
}

unsafe fn load_constants(path: &mut PathBuf) -> Result<()> {
    let s = std::fs::read_to_string(&path)?;
    let lines: Vec<_> = s.par_lines().collect();
    ide::alloc_constants(lines.len());
    for (line, name_p, value_p) in izip!(lines, ide::get_constant_names_mut(), ide::get_constants_mut()) {
        if !line.is_empty() {
            let (name, value) = line.split_once('=').ok_or_else(|| Error::SyntaxError(path.to_path_buf()))?;
            *name_p = UStr::new(name.as_ref());
            *value_p = UStr::new(value.as_ref());
        }
    }
    Ok(())
}

unsafe fn load_assets<'a, T: Sync>(
    name: &str,
    kind: u32,
    node: *const *const delphi::TTreeNode,
    load_asset: unsafe fn(&mut PathBuf) -> Result<*const T>,
    get_assets: fn() -> &'a mut [Option<&'a T>],
    get_names: fn() -> &'a mut [UStr],
    alloc: fn(usize),
    path: &mut PathBuf,
    asset_maps: &AssetMaps,
) -> Result<HashMap<String, usize>> {
    path.push(name);
    path.push("index.yyd");
    let index = std::fs::read_to_string(&path)?;
    path.pop();
    let mut names = vec![""];
    names.par_extend(index.par_lines());
    let name_map: HashMap<String, usize> =
        names.par_iter().enumerate().filter_map(|(i, s)| (!s.is_empty()).then(|| (s.to_string(), i))).collect();
    alloc(names.len());
    for (name, asset, name_p) in izip!(&names, get_assets(), get_names()) {
        if !name.is_empty() {
            *name_p = UStr::new(name.as_ref());
            path.push(name);
            *asset = load_asset(path)?.as_ref();
            path.pop();
        }
    }
    path.push("tree.yyd");
    read_resource_tree(node.read(), &path, kind, names, true)?;
    path.pop();
    path.pop();
    Ok(name_map)
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
    let mut asset_maps = AssetMaps::default();
    load_assets(
        "scripts",
        7,
        ide::RT_SCRIPTS,
        load_script,
        ide::get_scripts_mut,
        ide::get_script_names_mut,
        ide::alloc_scripts,
        &mut path,
        &asset_maps,
    )?;
    Ok(())
}
