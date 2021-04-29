use crate::{asset::*, delphi, delphi::UStr, ide, Error, Result};
use rayon::prelude::*;
use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
};

fn open_file(path: &std::path::Path) -> Result<BufReader<File>> {
    Ok(BufReader::new(File::open(path)?))
}

unsafe fn read_resource_tree<F: BufRead>(
    base: *const delphi::TTreeNode,
    f: F,
    kind: u32,
    names: Vec<&str>,
    visible: bool,
) -> Result<()> {
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
            _ => panic!("invalid resource tree"),
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

unsafe fn load_assets<'a, T: Sync>(
    name: &str, // capitalized cos this is used in resource list and win32 is case insensitive
    kind: u32,
    node: *const *const delphi::TTreeNode,
    load_asset: unsafe fn(&mut PathBuf) -> Result<*const T>,
    get_assets: fn() -> &'a mut [Option<&'a T>],
    get_names: fn() -> &'a mut [UStr],
    alloc: fn(usize),
    path: &mut PathBuf,
) -> Result<()> {
    path.push(name);
    path.push("index.yyd");
    let index = std::fs::read_to_string(&path)?;
    path.pop();
    let mut names = vec![""];
    names.par_extend(index.par_lines());
    alloc(names.len());
    for (name, asset, name_p) in itertools::izip!(&names, get_assets(), get_names()) {
        if !name.is_empty() {
            *name_p = UStr::new(name.as_ref());
            path.push(name);
            *asset = load_asset(path)?.as_ref();
            path.pop();
        }
    }
    path.push("tree.yyd");
    read_resource_tree(node.read(), open_file(&path)?, kind, names, true)?;
    path.pop();
    path.pop();
    Ok(())
}

pub unsafe fn load_gmk(mut path: PathBuf) -> Result<()> {
    path.pop();
    load_assets(
        "scripts",
        7,
        ide::RT_SCRIPTS,
        load_script,
        ide::get_scripts_mut,
        ide::get_script_names_mut,
        ide::alloc_scripts,
        &mut path,
    )?;
    Ok(())
}
