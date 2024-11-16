use crate::delphi::UStr;
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher, recommended_watcher};
use once_cell::unsync::Lazy;
use parking_lot::Mutex;
use std::{
    env::args,
    path::PathBuf,
    sync::mpsc::{Receiver, Sender, channel},
    time::{Duration, SystemTime},
};

static WATCHER_CHANNEL: Mutex<Lazy<(Sender<notify::Result<Event>>, Receiver<notify::Result<Event>>)>> =
    Mutex::new(Lazy::new(|| channel()));
static mut WATCHER: Option<RecommendedWatcher> = None;

pub fn setup_watcher() -> notify::Result<()> {
    unsafe {
        LAST_UPDATE = SystemTime::now();
    }
    let lock = WATCHER_CHANNEL.lock();
    let tx = lock.0.clone();
    drop(lock);
    let watcher = unsafe { WATCHER.insert(recommended_watcher(tx)?) };
    let mut exe_path = PathBuf::from(args().next().unwrap());
    exe_path.pop();
    exe_path.push("extensions");
    watcher.watch(&exe_path, RecursiveMode::NonRecursive)?;
    let appdata_path = unsafe { UStr::from_ptr(&*(0x78898c as *const *const u16)) };
    let appdata_path = PathBuf::from(appdata_path.to_os_string());
    watcher.watch(&appdata_path, RecursiveMode::NonRecursive)?;
    Ok(())
}

static mut LAST_UPDATE: SystemTime = SystemTime::UNIX_EPOCH;
static mut NEEDS_UPDATE: bool = false;

pub fn on_notify() {
    let lock = WATCHER_CHANNEL.lock();
    while let Some(_event) = lock.1.try_recv().ok().map(|e| e.unwrap()).filter(|event| match event.kind {
        EventKind::Create(_) => false,
        EventKind::Modify(_) => {
            event.paths.iter().filter(|p| p.extension() == Some("ged".as_ref())).any(|p| {
                if let Ok(modified) = p.metadata().and_then(|m| m.modified()) {
                    unsafe {
                        // it's foreign if modified time is after save end
                        // if someone edited a file in notepad, we'll get one of these
                        // if someone dragged in an older copy, we'll get a NoticeRemove instead
                        LAST_UPDATE < modified
                    }
                } else {
                    true
                }
            })
        },
        _ => true,
    }) {
        unsafe {
            LAST_UPDATE = SystemTime::now();
            NEEDS_UPDATE = true;
        }
    }
}

pub extern "fastcall" fn update_extensions() {
    unsafe {
        if NEEDS_UPDATE {
            if LAST_UPDATE.elapsed().map(|t| t >= Duration::from_secs(1)).unwrap_or_default() {
                NEEDS_UPDATE = false;
                let _: u32 = delphi_call!(0x713994);
                let _: u32 = delphi_call!(0x712a44);
                let _: u32 = delphi_call!(0x713a14);
            }
        }
    }
}
