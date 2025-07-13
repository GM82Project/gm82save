use super::{EXTRA_DATA, InstanceExtra, TileExtra, patch, patch_call};
use crate::{UStr, ide, ide::AssetListTrait};
use lazy_static::lazy_static;
use rayon::prelude::*;
use regex::Regex;
use std::{
    arch::{asm, naked_asm},
    collections::HashSet,
};

#[unsafe(naked)]
unsafe extern "C" fn compile_constants_inj() {
    naked_asm!(
        "mov ecx, eax",
        "jmp {}",
        sym compile_constants,
    );
}

lazy_static! {
    static ref INSTANCE_ID_REGEX: Regex = Regex::new(r"[A-Za-z0-9_]+_([0-9A-F]{8})").unwrap();
}

unsafe extern "fastcall" fn compile_constants(stream: usize) -> bool {
    if EXTRA_DATA.is_none() {
        let res: usize = delphi_call!(0x696744, stream, 1);
        return res as u8 != 0;
    }

    let constant_names = ide::get_constant_names();
    let constant_values = ide::get_constants();
    // this is just so my ide will give me autocomplete
    let re = &INSTANCE_ID_REGEX;

    // we want to collect instance names that are actually used
    // iterate over all code
    let cnv = |s: &UStr| s.to_os_string().into_string().unwrap_or_else(|s| s.to_string_lossy().into_owned());
    let room_iter = ide::ROOMS.assets().into_par_iter().flatten().flat_map(|room| {
        room.get_instances()
            .into_par_iter()
            .map(|i| cnv(&i.creation_code))
            .chain(rayon::iter::once(cnv(&room.creation_code)))
    });
    let object_iter = ide::OBJECTS.assets().into_par_iter().flatten().flat_map_iter(|o| {
        o.events.iter().flatten().flat_map(|e| e.get_actions()).flat_map(|a| &a.param_strings).map(cnv)
    });
    let timeline_iter =
        ide::TIMELINES.assets().into_par_iter().flatten().flat_map_iter(|t| {
            t.get_events().iter().flat_map(|e| e.get_actions()).flat_map(|a| &a.param_strings).map(cnv)
        });
    let script_iter = ide::SCRIPTS.assets().into_par_iter().flatten().map(|s| cnv(&s.source));
    let trigger_iter = ide::get_triggers().into_par_iter().flatten().map(|t| cnv(&t.condition));
    let constant_iter = constant_values.par_iter().map(cnv);

    // find instance names in code
    let instance_names: HashSet<u32> = room_iter
        .chain(object_iter)
        .chain(timeline_iter)
        .chain(script_iter)
        .chain(trigger_iter)
        .chain(constant_iter)
        .flat_map(|s| {
            // gotta collect into vec because otherwise string reference is lost
            re.captures_iter(&s)
                .filter_map(|c| c.get(1))
                .flat_map(|m| u32::from_str_radix(m.as_str(), 16))
                .collect::<Vec<_>>()
        })
        .collect();

    // collect data for referenced instances
    let instances = ide::ROOMS
        .assets()
        .into_par_iter()
        .zip(ide::ROOMS.names())
        .filter_map(|(room, name)| Some((room.as_ref()?, name)))
        .flat_map(|(room, name)| {
            let instance_names = &instance_names;
            room.get_instances().par_iter().filter_map(move |inst| {
                EXTRA_DATA
                    .as_ref()
                    .and_then(|(extra, _)| extra.get(&inst.id))
                    .filter(|extra| instance_names.contains(&extra.name))
                    .map(|extra| (name, inst.id, extra.name))
            })
        })
        .filter(|(_, id, _)| *id != 0)
        .collect::<Vec<_>>();

    // write version
    let _: u32 = delphi_call!(0x52f12c, stream, 800);
    // write count
    let _: u32 = delphi_call!(0x52f12c, stream, constant_names.len() + instances.len());
    // write instance ids
    for (room_name, id, name) in instances {
        // write constant name
        let _: u32 = delphi_call!(
            0x52f168,
            stream,
            UStr::new(format!("{}_{:08X}", room_name.to_os_string().to_str().unwrap(), name)).as_ptr()
        );
        // write constant value
        let _: u32 = delphi_call!(0x52f168, stream, UStr::new(id.to_string()).as_ptr());
    }
    // write original constants
    for (name, value) in constant_names.iter().zip(constant_values) {
        let _: u32 = delphi_call!(0x52f168, stream, name.as_ptr());
        let _: u32 = delphi_call!(0x52f168, stream, value.as_ptr());
    }
    true
}

#[unsafe(naked)]
unsafe extern "C" fn save_82_if_exe() {
    // only saves settings version 825 when saving an exe with the creation code flag set
    naked_asm!(
        "mov edx, 825",
        "mov ecx, 800",
        "test bl, bl", // if exe
        "cmovz edx, ecx",
        "bt word ptr [0x77f54e], 15", // if force cpu
        "cmovnc edx, ecx",
        "ret",
    );
}

#[unsafe(naked)]
unsafe extern "C" fn save_bool_if_exe() {
    naked_asm!(
        "push esi",
        "mov esi, 0x52f240", // WriteBoolean
        "mov ecx, 0x52f12c", // WriteInteger
        "test bl, bl",       // if exe
        "cmovnz ecx, esi",
        "call ecx",
        "pop esi",
        "ret",
    );
}

#[unsafe(naked)]
unsafe extern "C" fn save_creation_code_flag() {
    naked_asm!(
        "mov ecx, 0x52f12c", // WriteInteger (for uninitialized args)
        "call ecx",
        "test bl, bl", // if exe
        "jz 2f",
        "bt word ptr [0x77f54e], 15", // if force cpu
        "jnc 2f",
        // write webgl
        "mov eax, esi",      // gmk stream
        "xor edx, edx",      // 0 (webgl)
        "mov ecx, 0x52f12c", // WriteInteger
        "call ecx",
        // write creation code flag
        "mov eax, esi",      // gmk stream
        "mov dl, 1",         // true (creation code)
        "mov ecx, 0x52f240", // WriteBoolean
        "call ecx",
        // exit
        "2: ret",
    );
}

#[unsafe(naked)]
unsafe extern "C" fn save_room_version_inj() {
    naked_asm!(
        "mov cl, byte ptr [esp]",
        "call {}",
        "mov edx, eax",
        "mov eax, 0x658372",
        "jmp eax",
        sym save_room_version,
    );
}

unsafe extern "fastcall" fn save_room_version(exe: bool) -> u32 {
    if exe && EXTRA_DATA.is_some() { 811 } else { 541 }
}

#[unsafe(naked)]
unsafe extern "C" fn save_instance_extra_inj() {
    naked_asm!(
        "mov ecx, ebx", // file
        "mov eax, dword ptr [edi + 0x2f4]", // instance list
        "mov edx, dword ptr [eax + ebp*0x8 + 0xc]", // instance id
        "xor eax, eax",
        "mov al, byte ptr [esp]", // are we exe?
        "push eax",
        "call {}",
        "inc esi",
        "mov eax, 0x658600", // jnz of loop
        "dec dword ptr [esp + 0x4]",
        "jmp eax",
        sym save_instance_extra,
    );
}

#[unsafe(naked)]
unsafe extern "C" fn save_tile_extra_inj() {
    naked_asm!(
        "mov ecx, ebx", // file
        "mov eax, dword ptr [edi + 0x2fc]", // tile list
        "mov edx, dword ptr [eax + ebp*0x8 + 0x20]", // tile id
        "xor eax, eax",
        "mov al, byte ptr [esp]", // are we exe?
        "push eax",
        "call {}",
        "inc esi",
        "mov eax, 0x6586dd", // jnz of loop
        "dec dword ptr [esp + 0x4]",
        "jmp eax",
        sym save_tile_extra,
    );
}

unsafe fn save_real(file: usize, real: &f64) {
    asm!(
        "push dword ptr [{real} + 0x4]",
        "push dword ptr [{real}]",
        "call {call}",
        call = in(reg) 0x52f140,
        real = in(reg) real,
        in("eax") file,
        clobber_abi("C"),
    );
}

unsafe extern "fastcall" fn save_instance_extra(file: usize, id: usize, exe: bool) {
    if exe {
        if let Some(data) = EXTRA_DATA.as_ref().map(|(insts, _)| insts.get(&id).unwrap_or(&InstanceExtra::DEFAULT)) {
            save_real(file, &data.xscale);
            save_real(file, &data.yscale);
            let _: u32 = delphi_call!(0x52f12c, file, data.blend);
            save_real(file, &data.angle);
        }
    }
}

unsafe extern "fastcall" fn save_tile_extra(file: usize, id: usize, exe: bool) {
    if exe {
        if let Some(data) = EXTRA_DATA.as_ref().map(|(_, tiles)| tiles.get(&id).unwrap_or(&TileExtra::DEFAULT)) {
            save_real(file, &data.xscale);
            save_real(file, &data.yscale);
            let _: u32 = delphi_call!(0x52f12c, file, data.blend);
        }
    }
}

pub unsafe fn inject() {
    // add instance ids to constants
    patch_call(0x6cd90d as _, compile_constants_inj as _);

    // save creation code flag (reusing the software vertex processing flag)
    // write 825 instead of 800 for settings version if saving exe
    patch(0x70997c as _, &[0xe8]);
    patch_call(0x70997c as _, save_82_if_exe as _);
    // call WriteBoolean instead of WriteInteger if saving exe
    patch_call(0x709a4f as _, save_bool_if_exe as _);
    // save extra info if saving exe
    patch_call(0x709c99 as _, save_creation_code_flag as _);

    // save extra data on instances and tiles
    // write 811 instead of 541 for room version if saving exe
    patch(0x65836d as _, &[0xe9]);
    patch_call(0x65836d as _, save_room_version_inj as _);
    // instance stuff
    patch(0x6585fb as _, &[0xe9]);
    patch_call(0x6585fb as _, save_instance_extra_inj as _);
    // tile stuff
    patch(0x6586d8 as _, &[0xe9]);
    patch_call(0x6586d8 as _, save_tile_extra_inj as _);
}
