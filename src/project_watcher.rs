use crate::{ide, show_message, UStr, SAVE_END};
use notify::{watcher, DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher};
use once_cell::unsync::Lazy;
use parking_lot::Mutex;
use std::{
    arch::asm,
    path::PathBuf,
    sync::{
        mpsc::{channel, Receiver, Sender},
        Once,
    },
    time::Duration,
};

static WATCHER_CHANNEL: Mutex<Lazy<(Sender<DebouncedEvent>, Receiver<DebouncedEvent>)>> =
    Mutex::new(Lazy::new(|| channel()));
static mut WATCHER: Option<RecommendedWatcher> = None;

static WATCHER_ERROR: Once = Once::new();

unsafe extern "fastcall" fn show_message_and_reload() {
    const SCREEN: *const *const *mut *const usize = 0x7882f0 as _;
    // allow user to finish setting preferences
    {
        let screen = *SCREEN;
        let current_modal = screen.add(0x74 / 4).read();
        if !current_modal.is_null() {
            let vmt = current_modal.read() as usize;
            // is this TPreferencesForm?
            if vmt == 0x7153c4 {
                // call real TApplication.Idle and return
                let _: u32 = delphi_call!(0x520418, *(0x7882ec as *const usize));
                return
            }
        }
    }

    // reset TApplication.Idle
    crate::patch_call(0x51f74b, 0x520418);
    let message = UStr::new(format!(
        "Project files have been modified outside Game Maker. Reload project? \
                   Unsaved changes will be lost.\r\n\
                   If you click \"No\", saving will overwrite any foreign changes.",
    ));
    let mut answer: i32;
    asm! {
        "push 0",  // HelpFileName
        "push -1", // Y
        "push -1", // X
        "push 0",  // HelpCtx
        "call {}",
        in(reg) 0x4d437c, // MessageDlgPosHelp
        inlateout("eax") message.0 => answer,
        in("edx") 3, // DlgType
        in("ecx") 3, // Buttons
        clobber_abi("C"),
    }
    if answer == 6 {
        // yes -> reload project
        // but first, close all modals
        // this is done by patching the TApplication.Idle call in TApplication.HandleMessage
        // so that instead of idling after each form is closed, it closes the next form
        static mut OLD_ONCLOSE: usize = 0;
        static mut OLD_ONCLOSE_SENDER: usize = 0;
        #[naked]
        unsafe extern "fastcall" fn put_old_onclose_back() {
            asm! {
                "mov edx, {}",
                "mov [eax + 0x2f0], edx",
                "mov edx, {}",
                "mov [eax + 0x2f4], edx",
                "ret",
                sym OLD_ONCLOSE,
                sym OLD_ONCLOSE_SENDER,
                options(noreturn),
            }
        }
        unsafe extern "fastcall" fn instead_of_idle() {
            let screen = *SCREEN;
            // get TScreen.FSaveFocusedList
            let list = screen.add(0x78 / 4).read();
            // check if that list's Count is 0 (i.e. no modals open)
            if !list.add(2).read().is_null() {
                // there are more modals
                // get
                let current_modal = screen.add(0x74 / 4).read();
                // back up current modal's OnClose and replace with our own
                let onclose_ptr = current_modal.add(0x2f0 / 4);
                OLD_ONCLOSE = *onclose_ptr as _;
                OLD_ONCLOSE_SENDER = *onclose_ptr.add(1) as _;
                *onclose_ptr = put_old_onclose_back as _;
                *onclose_ptr.add(1) = current_modal as _;
                // let the modal know it should close
                current_modal.add(0x2b8 / 4).write(2 as _);
            } else {
                // put original TApplication.Idle back
                crate::patch_call(0x51f74b, 0x520418);
                // reload
                let _: u32 = delphi_call!(0x7059d8, (*ide::PROJECT_PATH).0);
            }
        }
        // patch TApplication.Idle call in TApplication.HandleMessage to close modals instead
        crate::patch_call(0x51f74b, instead_of_idle as _);
    } else {
        // no -> mark project as modified
        ide::SETTINGS_UPDATED.write(true);
    }
}

extern "fastcall" fn on_notify() {
    let lock = WATCHER_CHANNEL.lock();
    while let Some(_event) = lock.1.try_recv().ok().filter(|event| match event {
        DebouncedEvent::Chmod(_) | DebouncedEvent::Create(_) => false,
        DebouncedEvent::NoticeWrite(p) | DebouncedEvent::Write(p) => {
            if let Ok(modified) = p.metadata().and_then(|m| m.modified()) {
                unsafe {
                    // it's foreign if modified time is after save end
                    // if someone edited a file in notepad, we'll get one of these
                    // if someone dragged in an older copy, we'll get a NoticeRemove instead
                    SAVE_END < modified
                }
            } else {
                true
            }
        },
        _ => true,
    }) {
        drop(lock);
        unwatch();
        unsafe {
            // patch TApplication.Idle so it only pops the question after any dialogs are done
            // (note: not modals, i actually have some level of control over those)
            crate::patch_call(0x51f74b, show_message_and_reload as _);
        }
        break
    }
}

unsafe fn enable_timer() {
    let timer = (0x790100 as *const *const *mut usize).read().add(0x65c / 4).read();
    timer.add(0x34 / 4).write(1000); // interval (ms)
    timer.add(0x40 / 4).write(on_notify as _); // event
    timer.add(0x48 / 4).write(1); // enabled
    let _: u32 = delphi_call!(0x48e12c, timer);
}

pub fn watching() -> bool {
    unsafe { WATCHER.is_some() }
}

pub fn setup_watcher(path: &mut PathBuf) {
    unwatch();
    let lock = WATCHER_CHANNEL.lock();
    let tx = lock.0.clone();
    drop(lock);
    match watcher(tx, Duration::from_secs(1)) {
        Ok(mut watcher) => {
            let mut watch_all = || -> notify::Result<()> {
                path.push("backgrounds");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("datafiles");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("fonts");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("objects");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("paths");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("rooms");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("scripts");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("settings");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("sounds");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("sprites");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("timelines");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("triggers");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                watcher.watch(unsafe { &*ide::PROJECT_PATH }.to_os_string(), RecursiveMode::Recursive)?;
                Ok(())
            };
            if let Err(e) = watch_all() {
                show_message(format!("Couldn't initialize directory watch: {e}"));
            } else {
                // Great Success!
                unsafe {
                    WATCHER = Some(watcher);
                    enable_timer();
                }
            }
        },
        Err(e) => WATCHER_ERROR.call_once(|| show_message(format!("Couldn't create directory watcher: {e}"))),
    }
}

unsafe fn disable_timer() {
    let main_form = (0x790100 as *const *mut *mut usize).read();
    let timer_ptr = main_form.add(0x65c / 4);
    if timer_ptr.read().is_null() {
        // create timer if needed
        *timer_ptr = delphi_call!(0x48e048, 0x48ab50, 1, main_form);
    }
    let timer = timer_ptr.read();
    timer.add(0x12).write(0); // enabled
    let _: u32 = delphi_call!(0x48e12c, timer); // update timer
}

pub fn unwatch() {
    WATCHER_CHANNEL.lock().1.try_iter().for_each(|_| ()); // clear channel
    unsafe {
        WATCHER = None;
        disable_timer();
    }
}
