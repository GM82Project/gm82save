use crate::{
    asset::{Action, Event, Room, Timeline, Trigger},
    delphi::UStr,
    ide,
    ide::AssetListTrait,
    patch, patch_call,
};
use itertools::Itertools;
use std::{
    arch::asm,
    collections::{hash_map::Entry, HashMap},
};

type AssetId = usize;
type InstanceId = usize;
type CodeFormPointer = usize;

#[derive(PartialEq, Eq, Hash)]
enum CodeHolder {
    Action(*const Action),
    Room(*const Room),
    Trigger(*const Trigger),
}

// asset id is bitwise not'd for timelines
static mut CODE_FORMS: Option<HashMap<CodeHolder, (UStr, CodeFormPointer, AssetId)>> = None;

static mut INSTANCE_FORMS: Option<HashMap<*const Room, (AssetId, HashMap<InstanceId, (UStr, CodeFormPointer)>)>> = None;

#[naked]
unsafe extern "fastcall" fn trigger_disable_condition_memo() {
    unsafe extern "fastcall" fn inj(memo: *const *const usize, trigger: *const Trigger) {
        asm!(
            "call [{}]",
            in(reg) memo.read().add(0x74 / 4),
            in("eax") memo,
            in("dl") u8::from(
                CODE_FORMS.as_ref().map(|forms| !forms.contains_key(&CodeHolder::Trigger(trigger))).unwrap_or(true),
            ),
        );
    }
    asm!(
        "push eax",
        "mov ecx, 0x4ee6d8",
        "call ecx",
        "pop ecx",
        "mov edx, esi",
        "jmp {}",
        sym inj,
        options(noreturn),
    );
}

unsafe fn update_trigger_form(trigger: *const Trigger) {
    let trigger_form = (0x77f3fc as *const *const *const *const usize).read();
    if !trigger_form.is_null() {
        let list_box = trigger_form.add(0x388 / 4).read();
        let mut list_id: usize;
        asm!(
            "call {}",
            in(reg) list_box.read().add(0xec / 4).read(),
            inlateout("eax") list_box => list_id,
            clobber_abi("C"),
        );
        let trigger_id = list_box.add(0x3dc / 4 + list_id).cast::<usize>().read();
        if ide::get_triggers()[trigger_id].as_ref().map(|t| t.as_ptr()) == Some(trigger) {
            let _: u32 = delphi_call!(0x6bc118, trigger_form, list_id);
        }
    }
}

unsafe extern "fastcall" fn update_code(object: *mut usize) {
    let (holder, code) = match *object {
        0x6564cc => (CodeHolder::Room(object.cast()), &mut (*object.cast::<Room>()).creation_code),
        0x62cf48 => (CodeHolder::Trigger(object.cast()), &mut (*object.cast::<Trigger>()).condition),
        0x70fd70 => (CodeHolder::Action(object.cast()), &mut (*object.cast::<Action>()).param_strings[0]),
        _ => return,
    };
    if let Some((_, form, asset_id)) = CODE_FORMS.as_ref().and_then(|forms| forms.get(&holder)) {
        // mark as changed
        (*form as *mut bool).add(0x440).write(true);
        // save text
        let _: u32 = delphi_call!(0x6b8444, (*form as *const usize).add(0x43c / 4).read(), code);
        if *object == 0x62cf48 {
            // it's a trigger
            let _: u32 = delphi_call!(0x6bcb60);
        } else if *object == 0x6564cc {
            // it's a room
            let _: u32 = delphi_call!(0x6930cc, *asset_id);
        } else if *object == 0x70fd70 {
            // it's an action
            // mark the object/timeline as updated
            if (*asset_id as isize) >= 0 {
                // it's an object
                let _: u32 = delphi_call!(0x62cd2c, *asset_id);
            } else {
                // it's a timeline
                let _: u32 = delphi_call!(0x6fa7b0, *asset_id);
            }
            // save the "applies to"
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

#[naked]
unsafe extern "C" fn update_code_inj() {
    asm!(
        "mov ecx, eax",
        "jmp {}",
        sym update_code,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn check_all_closing() {
    asm!(
        "mov eax, 0x682a4a",
        // original check: form.savechanges
        "cmp byte ptr [ebx + 0x441], 0",
        "jnz 2f",
        // new check: closing all?
        "mov ecx, 0x77f448",
        "cmp byte ptr [ecx], 0",
        // return
        "2: jmp eax",
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn close_code() {
    unsafe extern "fastcall" fn inj(form: usize, response: i32) {
        let revert = response != 6;
        let mut found_regular_form = false;
        if let Some(forms) = CODE_FORMS.as_mut() {
            let mut my_trigger = None;
            forms.retain(|holder, (original_code, f, asset_id)| {
                if *f == form {
                    found_regular_form = true;
                    let code = match holder {
                        CodeHolder::Trigger(trigger) => {
                            my_trigger = Some(*trigger);
                            &mut (*trigger.cast_mut()).condition
                        },
                        CodeHolder::Room(room) => &mut (*room.cast_mut()).creation_code,
                        CodeHolder::Action(action) => &mut (*action.cast_mut()).param_strings[0],
                    };
                    if revert {
                        *code = original_code.clone();
                        match holder {
                            CodeHolder::Action(_) => {
                                if (*asset_id as isize) >= 0 {
                                    // it's an object
                                    let _: u32 = delphi_call!(0x62cd2c, *asset_id);
                                } else {
                                    // it's a timeline
                                    let _: u32 = delphi_call!(0x6fa7b0, *asset_id);
                                }
                            },
                            CodeHolder::Room(_) => {
                                let _: u32 = delphi_call!(0x6930cc, *asset_id);
                            },
                            CodeHolder::Trigger(_) => {
                                let _: u32 = delphi_call!(0x6bcb60);
                            },
                        }
                    }
                    false
                } else {
                    true
                }
            });
            if let Some(trigger) = my_trigger {
                update_trigger_form(trigger);
            }
        }
        if !found_regular_form {
            if let Some(forms) = INSTANCE_FORMS.as_mut() {
                for (room, (_, map)) in forms.iter_mut() {
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
                forms.retain(|_, (_, m)| !m.is_empty());
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
        if let Some((room_id, forms)) = INSTANCE_FORMS.as_ref().and_then(|forms| forms.get(&room.cast_const())) {
            for inst in (*room).get_instances_mut() {
                if let Some(form) = forms.get(&inst.id) {
                    // honestly fuckin sure just mark all of them as changed idc
                    (form.1 as *mut bool).add(0x440).write(true);
                    let _: u32 =
                        delphi_call!(0x6b8444, (form.1 as *const usize).add(0x43c / 4).read(), &mut inst.creation_code);
                }
            }
            // update room
            let _: u32 = delphi_call!(0x6930cc, *room_id);
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
    editor.add(0x2d0 / 4).write(if is_instance { update_instance_code as _ } else { update_code_inj as _ });
    // set up find box
    let _: u32 = delphi_call!(0x681d00, form);
    (UStr::from_ptr(&code).clone(), form as usize)
}

unsafe fn open_or_insert(holder: CodeHolder, create: impl FnOnce() -> (UStr, CodeFormPointer, AssetId)) {
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
    unsafe extern "fastcall" fn inj(
        title: *const u16,
        action: &Action,
        form_ebx: *const usize,
        form_esi: *const usize,
    ) {
        enum ThingForm {
            Object(*const usize),
            Timeline(*const usize),
        }
        let form = match *form_esi {
            0x6c3530 => ThingForm::Object(form_esi),
            0x6f6a40 => ThingForm::Timeline(form_esi),
            _ => match *form_ebx {
                0x6c3530 => ThingForm::Object(form_ebx),
                0x6f6a40 => ThingForm::Timeline(form_ebx),
                _ => unreachable!(),
            },
        };
        let (title, asset_id) = match form {
            ThingForm::Object(form) => {
                let object_index = form.add(0x46c / 4).read();
                let object_name = ide::OBJECTS.names()[object_index].clone();
                let event_type = form.add(0x8180 / 4).read();
                let event_number = form.add(0x8184 / 4).read();
                let event = form.add(0x817c / 4).cast::<&Event>().read();
                let action_id =
                    event.get_actions().iter().find_position(|act| act.as_ptr() == action as *const _).unwrap().0;
                let mut event_name = UStr::default();
                let _: u32 = delphi_call!(0x6d0df0, event_type, event_number, &mut event_name);
                (
                    object_name
                        + UStr::new(" - ")
                        + event_name
                        + UStr::new(format!(" - Action {} - ", action_id + 1))
                        + UStr::from_ptr(&title),
                    object_index,
                )
            },
            ThingForm::Timeline(form) => {
                let timeline_index = form.add(0x430 / 4).read();
                let timeline_name = ide::TIMELINES.names()[timeline_index].clone();
                let timeline = form.add(0x420 / 4).cast::<&Timeline>().read();
                let event = form.add(0x434 / 4).cast::<&Event>().read();
                let moment_id =
                    timeline.moment_events.iter().find_position(|e| e.as_ptr() == event as *const _).unwrap().0;
                let action_id =
                    event.get_actions().iter().find_position(|a| a.as_ptr() == action as *const _).unwrap().0;
                (
                    timeline_name
                        + UStr::new(format!(
                            " - Step {} - Action {} - ",
                            timeline.moment_times[moment_id],
                            action_id + 1
                        ))
                        + UStr::from_ptr(&title),
                    !timeline_index,
                )
            },
        };
        open_or_insert(CodeHolder::Action(action), || {
            let (code, form) = create_code_form(
                action.param_strings[0].0,
                Some(action.applies_to),
                title.0,
                action.action_kind != 6,
                action as *const _ as _,
                false,
            );
            (code, form, asset_id)
        });
    }
    asm!(
        "mov edx, dword ptr [esi]",
        "push dword ptr [ebp - 0x1c]",
        "push dword ptr [ebp + 0xc]",
        "call {}",
        "mov al, 1",
        "ret 8",
        sym inj,
        options(noreturn),
    );
}

#[naked]
unsafe extern "C" fn open_room_code() {
    unsafe extern "fastcall" fn inj(room_id: usize, room: &Room) {
        let title = ide::ROOMS.names()[room_id].clone() + UStr::new(" - Room Creation Code");
        open_or_insert(CodeHolder::Room(room), || {
            let (code, form) =
                create_code_form(room.creation_code.0, None, title.0, true, room as *const _ as _, false);
            (code, form, room_id)
        });
    }
    asm!(
        "mov ecx, [ebx + 0x630]",
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
            let (code, form) =
                create_code_form(trigger.condition.0, None, title, true, trigger as *const _ as _, false);
            (code, form, 0)
        });
        update_trigger_form(trigger);
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
    unsafe extern "fastcall" fn inj(title: *const u16, room: &Room, code: *const u16, inst_id: usize, room_id: usize) {
        match INSTANCE_FORMS
            .get_or_insert_default()
            .entry(room)
            .or_insert_with(|| (room_id, HashMap::new()))
            .1
            .entry(inst_id)
        {
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
        "push dword ptr [ebx + 0x630]", // room id
        "push dword ptr [ebp - 0x10]", // instance id
        "push dword ptr [ebp - 4]", // code
        "call {}",
        "ret 8",
        sym inj,
        options(noreturn),
    );
}

unsafe extern "C" fn destroy_action(_: u32, action: &Action) {
    if let Some(forms) = CODE_FORMS.as_mut() {
        if let Some(form) = forms.remove(&CodeHolder::Action(action)) {
            let _: u32 = delphi_call!(0x405a7c, form.1);
        }
    }
}

unsafe extern "fastcall" fn clear_all_instances_in_room(room: *const Room) {
    if let Some(forms) = INSTANCE_FORMS.as_mut().and_then(|forms| forms.remove(&room)) {
        for (_, form) in forms.1 {
            let _: u32 = delphi_call!(0x405a7c, form.1);
        }
    }
}

#[naked]
unsafe extern "C" fn room_delete_instance() {
    unsafe extern "fastcall" fn inj(room: *const Room, inst_number: usize) {
        let inst_id = (*room).get_instances()[inst_number].id;
        if let Some(form) =
            INSTANCE_FORMS.as_mut().and_then(|forms| forms.get_mut(&room)).and_then(|(_, forms)| forms.remove(&inst_id))
        {
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
unsafe extern "C" fn room_safe_undo() {
    unsafe extern "fastcall" fn inj(room: *mut Room) {
        update_code(room.cast());
        if let Some(all_forms) = INSTANCE_FORMS.as_mut() {
            if let Some((_, forms)) = all_forms.get_mut(&room.cast_const()) {
                forms.retain(|&id, _| (*room).get_instances().iter().any(|i| i.id == id));
            }
        }
    }
    asm!(
        "mov edx, 0x405a7c",
        "call edx",
        "mov ecx, [ebx + 0x61c]",
        "jmp {}",
        sym inj,
        options(noreturn),
    );
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
            let _: u32 = delphi_call!(0x405a7c, form.1);
        }
    }
    clear_all_instances_in_room(room);
}

unsafe extern "C" fn destroy_trigger(_: u32, trigger: &Trigger) {
    if let Some(forms) = CODE_FORMS.as_mut() {
        if let Some(form) = forms.remove(&CodeHolder::Trigger(trigger)) {
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

    // double check when undoing room
    patch_call(0x6889b0, room_safe_undo as _);

    // close form when deleting assets
    patch(0x70fd58, &(destroy_action as usize).to_le_bytes());
    patch(0x62cf30, &(destroy_trigger as usize).to_le_bytes());
    patch(0x6564b4, &(destroy_room as usize).to_le_bytes());

    // disable condition memo when code form is open
    patch_call(0x6bc183, trigger_disable_condition_memo as _);

    // close form
    patch(0x682a42, &[0x2f]);
    patch(0x682a43, &[0xe9, 0, 0, 0, 0, 0x90, 0x90]);
    patch_call(0x682a43, check_all_closing as _);
    patch(0x682a4b, &[0x26]);
    patch(0x682a6b, &[0x0a]);
    patch(0x682a72, &[
        0x33, 0xc0, // xor eax, eax
        0xb0, 0x06, // mov al, 6
        0xe8, 0, 0, 0, 0, // call <...>
        0xc6, 0x06, 0x02, // mov dword ptr [esi], 2
    ]);
    patch_call(0x682a76, close_code as _);
}
