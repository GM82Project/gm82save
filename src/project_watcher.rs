use crate::{ide, show_message, UStr, LAST_SAVE, SAVE_START};
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

extern "fastcall" fn on_notify() {
    let lock = WATCHER_CHANNEL.lock();
    while lock
        .1
        .try_recv()
        .ok()
        .filter(|event| match event {
            DebouncedEvent::Chmod(_) | DebouncedEvent::Create(_) => false,
            DebouncedEvent::NoticeWrite(p) | DebouncedEvent::Write(p) => {
                if let Ok(modified) = p.metadata().and_then(|m| m.modified()) {
                    unsafe {
                        if SAVE_START == LAST_SAVE {
                            // from load, most likely not ours but give 2 seconds of leeway anyway
                            LAST_SAVE.duration_since(modified).unwrap_or_else(|e| e.duration()).as_secs() > 2
                        } else {
                            // it's ours if it's between save start and save end, give 2 secs of leeway
                            let earlier = modified
                                .checked_add(Duration::from_secs(2))
                                .and_then(|t| t.duration_since(SAVE_START).ok())
                                .is_none();
                            let later = LAST_SAVE
                                .checked_add(Duration::from_secs(2))
                                .and_then(|t| t.duration_since(modified).ok())
                                .is_none();
                            earlier || later
                        }
                    }
                } else {
                    true
                }
            },
            _ => true,
        })
        .is_some()
    {
        unwatch();
        lock.1.try_iter().for_each(|_| ()); // clear channel
        drop(lock);
        unsafe {
            let message = UStr::new(
                "Project files have been modified outside Game Maker. Reload project? \
                   Unsaved changes will be lost.",
            );
            let mut answer: i32;
            asm! {
            "push 0",  // HelpFileName
            "push -1", // Y
            "push -1", // X
            "push 0",  // HelpCtx
            "call {}",
            in(reg) 0x4d437c, // MessageDlgPosHelp
            inlateout("eax") message.0 => answer,
            inlateout("edx") 3 => _, // DlgType
            inlateout("ecx") 3 => _, // Buttons
            }
            if answer == 6 {
                // yes -> reload project
                let _: u32 = delphi_call!(0x7059d8, (*ide::PROJECT_PATH).0);
            } else {
                // no -> mark project as modified
                ide::SETTINGS_UPDATED.write(true);
            }
            break
        };
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
    unsafe {
        WATCHER = None;
        disable_timer();
    }
}
