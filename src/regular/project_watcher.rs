use crate::{SAVE_END, UStr, ide, show_message};
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher, recommended_watcher};
use once_cell::unsync::Lazy;
use parking_lot::Mutex;
use std::{
    arch::naked_asm,
    path::PathBuf,
    sync::{
        Once,
        mpsc::{Receiver, Sender, channel},
    },
};

static WATCHER_CHANNEL: Mutex<Lazy<(Sender<notify::Result<Event>>, Receiver<notify::Result<Event>>)>> =
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
    let answer = crate::show_question(&message);
    if answer == 6 {
        // yes -> reload project
        // but first, close all modals
        // this is done by patching the TApplication.Idle call in TApplication.HandleMessage
        // so that instead of idling after each form is closed, it closes the next form
        static mut OLD_ONCLOSE: usize = 0;
        static mut OLD_ONCLOSE_SENDER: usize = 0;
        #[unsafe(naked)]
        unsafe extern "C" fn put_old_onclose_back() {
            naked_asm!(
                "mov edx, {}",
                "mov [eax + 0x2f0], edx",
                "mov edx, {}",
                "mov [eax + 0x2f4], edx",
                "ret",
                sym OLD_ONCLOSE,
                sym OLD_ONCLOSE_SENDER,
            );
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

pub fn on_notify() {
    let lock = WATCHER_CHANNEL.lock();
    // note: no need to check needs_rescan because no windows watcher uses it
    while let Some(_event) = lock.1.try_recv().ok().map(|e| e.unwrap()).filter(|event| match event.kind {
        EventKind::Create(_) => false,
        EventKind::Modify(_) => {
            event.paths.iter().any(|p| {
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
            })
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

pub fn watching() -> bool {
    unsafe { WATCHER.is_some() }
}

pub fn setup_watcher(path: &mut PathBuf) {
    unwatch();
    let lock = WATCHER_CHANNEL.lock();
    let tx = lock.0.clone();
    drop(lock);
    match recommended_watcher(tx) {
        Ok(mut watcher) => {
            let mut watch_all = || -> notify::Result<()> {
                path.push("backgrounds");
                if path.exists() {
                    watcher.watch(&path, RecursiveMode::Recursive)?;
                }
                path.pop();
                path.push("datafiles");
                if path.exists() {
                    watcher.watch(&path, RecursiveMode::Recursive)?;
                }
                path.pop();
                path.push("fonts");
                if path.exists() {
                    watcher.watch(&path, RecursiveMode::Recursive)?;
                }
                path.pop();
                path.push("objects");
                if path.exists() {
                    watcher.watch(&path, RecursiveMode::Recursive)?;
                }
                path.pop();
                path.push("paths");
                if path.exists() {
                    watcher.watch(&path, RecursiveMode::Recursive)?;
                }
                path.pop();
                path.push("rooms");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("scripts");
                if path.exists() {
                    watcher.watch(&path, RecursiveMode::Recursive)?;
                }
                path.pop();
                path.push("settings");
                watcher.watch(&path, RecursiveMode::Recursive)?;
                path.pop();
                path.push("sounds");
                if path.exists() {
                    watcher.watch(&path, RecursiveMode::Recursive)?;
                }
                path.pop();
                path.push("sprites");
                if path.exists() {
                    watcher.watch(&path, RecursiveMode::Recursive)?;
                }
                path.pop();
                path.push("timelines");
                if path.exists() {
                    watcher.watch(&path, RecursiveMode::Recursive)?;
                }
                path.pop();
                path.push("triggers");
                if path.exists() {
                    watcher.watch(&path, RecursiveMode::Recursive)?;
                }
                path.pop();
                watcher.watch(unsafe { &*ide::PROJECT_PATH }.to_os_string().as_ref(), RecursiveMode::Recursive)?;
                Ok(())
            };
            if let Err(e) = watch_all() {
                show_message(format!("Couldn't initialize directory watch: {e}"));
            } else {
                // Great Success!
                unsafe {
                    WATCHER = Some(watcher);
                }
            }
        },
        Err(e) => WATCHER_ERROR.call_once(|| show_message(format!("Couldn't create directory watcher: {e}"))),
    }
}

pub extern "C" fn unwatch() {
    WATCHER_CHANNEL.lock().1.try_iter().for_each(|_| ()); // clear channel
    unsafe {
        WATCHER = None;
    }
}
