use crate::{
    asset::{Action, Room, Trigger},
    delphi::UStr,
    patch, patch_call,
};
use std::{
    arch::asm,
    collections::{hash_map::Entry, HashMap},
};

#[derive(PartialEq, Eq, Hash)]
enum CodeHolder {
    Action(*const Action),
    Room(*const Room),
    Trigger(*const Trigger),
}

static mut CODE_FORMS: Option<HashMap<CodeHolder, (UStr, usize)>> = None;
static mut INSTANCE_FORMS: Option<HashMap<*const Room, HashMap<usize, (UStr, usize)>>> = None;

#[naked]
unsafe extern "C" fn update_code() {
    unsafe extern "fastcall" fn inj(object: *mut usize) {
        let (holder, code) = match *object {
            0x6564cc => (CodeHolder::Room(object.cast()), &mut (*object.cast::<Room>()).creation_code),
            0x62cf48 => (CodeHolder::Trigger(object.cast()), &mut (*object.cast::<Trigger>()).condition),
            0x70fd70 => (CodeHolder::Action(object.cast()), &mut (*object.cast::<Action>()).param_strings[0]),
            _ => return,
        };
        if let Some((_, form)) = CODE_FORMS.as_ref().and_then(|forms| forms.get(&holder)) {
            // mark as changed
            (*form as *mut bool).add(0x440).write(true);
            // save text
            let _: u32 = delphi_call!(0x6b8444, (*form as *const usize).add(0x43c / 4).read(), code);
            if *object == 0x70fd70 {
                // it's an action, save the "applies to"
                let action = object.cast::<Action>();
                let self_check = (*form as *const *const *const usize).add(0x3c0 / 4).read();
                let other_check = (*form as *const *const *const usize).add(0x3c4 / 4).read();
                let self_checked: u32;
                let other_checked: u32;
                asm!(
                    "call {}",
                    in(reg) self_check.read().add(0xec / 4).read(),
                    inlateout("eax") self_check => self_checked,
                    in("edx") 1,
                    clobber_abi("C"),
                );
                asm!(
                    "call {}",
                    in(reg) other_check.read().add(0xec / 4).read(),
                    inlateout("eax") other_check => other_checked,
                    in("edx") 1,
                    clobber_abi("C"),
                );
                (*action).applies_to = if self_checked != 0 {
                    -1
                } else if other_checked != 0 {
                    -2
                } else {
                    (*form as *const *const i32).add(0x3cc / 4).read().add(0xc / 4).read()
                };
            }
        }
    }
    asm!(
        "mov ecx, eax",
        "jmp {}",
        sym inj,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn close_code() {
    unsafe extern "fastcall" fn inj(form: usize, response: i32) {
        let revert = response != 6;
        let mut found_regular_form = false;
        if let Some(forms) = CODE_FORMS.as_mut() {
            forms.retain(|holder, f| {
                if f.1 == form {
                    found_regular_form = true;
                    let code = match holder {
                        CodeHolder::Trigger(trigger) => &mut (*trigger.cast_mut()).condition,
                        CodeHolder::Room(room) => &mut (*room.cast_mut()).creation_code,
                        CodeHolder::Action(action) => &mut (*action.cast_mut()).param_strings[0],
                    };
                    if revert {
                        *code = f.0.clone();
                    }
                    false
                } else {
                    true
                }
            });
        }
        if !found_regular_form {
            if let Some(forms) = INSTANCE_FORMS.as_mut() {
                for (room, map) in forms.iter_mut() {
                    map.retain(|&id, f| {
                        if f.1 == form {
                            if revert {
                                if let Some(inst) =
                                    (*room.cast_mut()).get_instances_mut().iter_mut().filter(|i| i.id == id).next()
                                {
                                    inst.creation_code = f.0.clone();
                                }
                            }
                            false
                        } else {
                            true
                        }
                    })
                }
                forms.retain(|_, m| !m.is_empty());
            }
        }
    }
    asm!(
        "mov edx, eax",
        "mov ecx, ebx",
        "jmp {}",
        sym inj,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn update_instance_code() {
    unsafe extern "fastcall" fn inj(room: *mut Room) {
        if let Some(forms) = INSTANCE_FORMS.as_ref().and_then(|forms| forms.get(&room.cast_const())) {
            for inst in (*room).get_instances_mut() {
                if let Some(form) = forms.get(&inst.id) {
                    // honestly fuckin sure just mark all of them as changed idc
                    (form.1 as *mut bool).add(0x440).write(true);
                    let _: u32 =
                        delphi_call!(0x6b8444, (form.1 as *const usize).add(0x43c / 4).read(), &mut inst.creation_code);
                }
            }
        }
    }
    asm!(
        "mov ecx, eax",
        "jmp {}",
        sym inj,
        options(noreturn),
    );
}

unsafe fn create_code_form(
    code: *const u16,
    applies_to: Option<i32>,
    title: *const u16,
    is_code: bool,
    holder: usize,
    is_instance: bool,
) -> (UStr, usize) {
    // TCodeForm.Create
    let form: *mut u32 = delphi_call!(0x514e78, 0x68050c, 1, 0);
    // SetText
    let _: u32 = delphi_call!(0x4ee6d8, form, title);
    // SetVisible applies_to
    let _: u32 = delphi_call!(0x4ee5c0, form.add(0x3b8 / 4).read(), u32::from(applies_to.is_some()));
    if let Some(applies_to) = applies_to {
        // SetVisible WhoName
        let _: u32 = delphi_call!(0x4ee5c0, form.add(0x3cc / 4).read(), u32::from(applies_to >= 0));
        // SetVisible WhoMenuBtn
        let _: u32 = delphi_call!(0x4ee5c0, form.add(0x3bc / 4).read(), u32::from(applies_to >= 0));
        // get object name
        let applies_to_name = UStr::default();
        let _: u32 = delphi_call!(0x62cabc, applies_to, &applies_to_name);
        // WhoName.SetText
        let _: u32 = delphi_call!(0x4ee6d8, form.add(0x3cc / 4).read(), applies_to_name.0);
        let check_offset = match applies_to {
            -1 => 0x3c0,
            -2 => 0x3c4,
            _ => 0x3c8,
        };
        let check = form.add(check_offset / 4).read() as *const *const u32;
        // SetChecked on whatever radio button
        asm!(
            "call {}",
            in(reg) check.read().add(0xf0 / 4).read(),
            in("eax") check,
            in("edx") 1,
            clobber_abi("C"),
        );
    }
    // hide check button if not code (lmao)
    let _: u32 = delphi_call!(0x4ee5c0, form.add(0x394 / 4).read(), u32::from(is_code));
    // ischanged, savechanges
    form.add(0x440 / 4).write(0);
    // set up editor
    let editor = form.add(0x43c / 4).read() as *mut u32;
    // TEditor.SetTheText
    let _: u32 = delphi_call!(0x6b83b8, editor, code);
    // editor OnChange event
    editor.add(0x2d4 / 4).write(holder as _);
    editor.add(0x2d0 / 4).write(if is_instance { update_instance_code as _ } else { update_code as _ });
    // set up find box
    let _: u32 = delphi_call!(0x681d00, form);
    (UStr::from_ptr(&code).clone(), form as usize)
}

unsafe fn open_or_insert(holder: CodeHolder, create: impl FnOnce() -> (UStr, usize)) {
    match CODE_FORMS.get_or_insert_default().entry(holder) {
        Entry::Occupied(entry) => {
            let _: u32 = delphi_call!(0x4ee948, entry.get().1);
        },
        Entry::Vacant(entry) => {
            entry.insert(create());
        },
    }
}

#[naked]
unsafe extern "C" fn open_code_action() {
    unsafe extern "fastcall" fn inj(title: *const u16, action: &Action) {
        open_or_insert(CodeHolder::Action(action), || {
            create_code_form(
                action.param_strings[0].0,
                Some(action.applies_to),
                title,
                action.action_kind != 6,
                action as *const _ as _,
                false,
            )
        });
    }
    asm!(
        "mov edx, dword ptr [esi]",
        "call {}",
        "mov al, 1",
        "ret 8",
        sym inj,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn open_room_code() {
    unsafe extern "fastcall" fn inj(title: *const u16, room: &Room) {
        open_or_insert(CodeHolder::Room(room), || {
            create_code_form(room.creation_code.0, None, title, true, room as *const _ as _, false)
        });
    }
    asm!(
        "mov edx, [ebx + 0x61c]",
        "call {}",
        "ret 8",
        sym inj,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn open_trigger_code() {
    unsafe extern "fastcall" fn inj(title: *const u16, trigger: &Trigger) {
        open_or_insert(CodeHolder::Trigger(trigger), || {
            create_code_form(trigger.condition.0, None, title, true, trigger as *const _ as _, false)
        });
    }
    asm!(
        "mov edx, edi",
        "call {}",
        "ret 8",
        sym inj,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn open_instance_code() {
    unsafe extern "fastcall" fn inj(title: *const u16, room: &Room, code: *const u16, id: usize) {
        match INSTANCE_FORMS.get_or_insert_default().entry(room).or_default().entry(id) {
            Entry::Occupied(entry) => {
                let _: u32 = delphi_call!(0x4ee948, entry.get().1);
            },
            Entry::Vacant(entry) => {
                entry.insert(create_code_form(code, None, title, true, room as *const _ as _, true));
            },
        }
    }
    asm!(
        "mov edx, [ebx + 0x61c]",
        "push dword ptr [ebp - 0x10]",
        "push dword ptr [ebp - 4]",
        "call {}",
        "ret 8",
        sym inj,
        options(noreturn),
    );
}

unsafe extern "C" fn destroy_action(_: u32, action: &Action) {
    if let Some(forms) = CODE_FORMS.as_mut() {
        if let Some(form) = forms.remove(&CodeHolder::Action(action)) {
            let _: u32 = delphi_call!(0x51a7e8, form.1);
            let _: u32 = delphi_call!(0x405a7c, form.1);
        }
    }
}

unsafe extern "fastcall" fn clear_all_instances_in_room(room: *const Room) {
    if let Some(forms) = INSTANCE_FORMS.as_mut().and_then(|forms| forms.remove(&room)) {
        for (_, form) in forms {
            let _: u32 = delphi_call!(0x51a7e8, form.1);
            let _: u32 = delphi_call!(0x405a7c, form.1);
        }
    }
}

#[naked]
unsafe extern "C" fn room_delete_instance() {
    unsafe extern "fastcall" fn inj(room: *const Room, inst_number: usize) {
        let inst_id = (*room).get_instances()[inst_number].id;
        if let Some(form) =
            INSTANCE_FORMS.as_mut().and_then(|forms| forms.get_mut(&room)).and_then(|forms| forms.remove(&inst_id))
        {
            let _: u32 = delphi_call!(0x51a7e8, form.1);
            let _: u32 = delphi_call!(0x405a7c, form.1);
        }
    }
    asm!(
        "push esi",
        "push edi",
        "push ebp",
        "mov ebx, eax",
        "push edx",
        "mov ecx, eax",
        "call {}",
        "pop edx",
        "mov eax, 0x658ab6",
        "jmp eax",
        sym inj,
        options(noreturn),
    )
}

#[naked]
unsafe extern "C" fn room_delete_all() {
    asm!(
        "push eax",
        "mov ecx, eax",
        "call {}",
        "pop eax",
        "mov edx, 0x657b48",
        "jmp edx",
        sym clear_all_instances_in_room,
        options(noreturn),
    )
}

unsafe extern "C" fn destroy_room(_: u32, room: &Room) {
    if let Some(forms) = CODE_FORMS.as_mut() {
        if let Some(form) = forms.remove(&CodeHolder::Room(room)) {
            let _: u32 = delphi_call!(0x51a7e8, form.1);
            let _: u32 = delphi_call!(0x405a7c, form.1);
        }
    }
    clear_all_instances_in_room(room);
}

unsafe extern "C" fn destroy_trigger(_: u32, trigger: &Trigger) {
    if let Some(forms) = CODE_FORMS.as_mut() {
        if let Some(form) = forms.remove(&CodeHolder::Trigger(trigger)) {
            let _: u32 = delphi_call!(0x51a7e8, form.1);
            let _: u32 = delphi_call!(0x405a7c, form.1);
        }
    }
}

pub unsafe fn inject() {
    // nop out modal creation
    patch(0x7724af, &[0x90; 5]);

    // open in appropriate places
    patch_call(0x6bc877, open_trigger_code as _);
    patch_call(0x689f36, open_room_code as _);
    patch_call(0x6fefdf, open_code_action as _);
    patch_call(0x68aea0, open_instance_code as _);

    // close form when deleting instances
    patch(0x658ab1, &[0xe9]);
    patch_call(0x658ab1, room_delete_instance as _);
    patch_call(0x658b14, room_delete_all as _);

    // close form when deleting assets
    patch(0x70fd58, &(destroy_action as usize).to_le_bytes());
    patch(0x62cf30, &(destroy_trigger as usize).to_le_bytes());
    patch(0x6564b4, &(destroy_room as usize).to_le_bytes());

    // close form
    patch(0x682a42, &[0x2f]);
    patch(0x682a4b, &[0x26]);
    patch(0x682a6b, &[0x0b]);
    patch(0x682a72, &[
        0x33, 0xc0, // xor eax, eax
        0xb0, 0x06, // mov al, 6
        0xe8, 0, 0, 0, 0, // call <...>
        0xc6, 0x06, 0x02, // mov dword ptr [esi], 2
    ]);
    patch_call(0x682a76, close_code as _);
}
