pub mod extension_watcher;
pub mod project_watcher;

use crate::TIME_SPENT;
use crate::delphi::UStr;
use std::fmt::Write;

fn has_focus() -> bool {
    #[link(name = "user32")]
    unsafe extern "system" {
        fn GetForegroundWindow() -> usize;
    }
    // FindControl
    let control: u32 = unsafe { delphi_call!(0x4eab9c, GetForegroundWindow()) };
    control != 0
}

fn update_game_settings_time() {
    fn format_time_spent() -> String {
        let mut s = String::new();
        let seconds_total = unsafe { TIME_SPENT };
        let seconds = seconds_total % 60;
        let minutes_total = seconds_total / 60;
        let minutes = minutes_total % 60;
        let hours_total = minutes_total / 60;
        let hours = hours_total % 24;
        let days_total = hours_total / 24;
        let plural = |x| if x > 1 { "s" } else { "" };
        if days_total > 0 {
            let _ = write!(s, "{} day{}", days_total, plural(days_total));
            if seconds_total % (60 * 60 * 24) != 0 {
                s += ", "
            }
        }
        if hours > 0 {
            let _ = write!(s, "{} hour{}", hours, plural(hours));
            if seconds_total % (60 * 60) != 0 {
                s += ", "
            };
        }
        if minutes > 0 {
            let _ = write!(s, "{} minute{}", minutes, plural(minutes));
            if seconds != 0 {
                s += ", "
            };
        }
        if seconds > 0 {
            let _ = write!(s, "{} second{}", seconds, plural(seconds));
        }
        s
    }
    unsafe {
        let option_form = (0x790808 as *const usize).read();
        let spent_control = ((option_form + 0x49c) as *const usize).read();
        let spent_string = UStr::new(format_time_spent());
        let _: u32 = delphi_call!(0x4ee6d8, spent_control, spent_string.as_ptr());
    }
}

extern "fastcall" fn every_second() {
    if has_focus() {
        unsafe {
            TIME_SPENT += 1;
            update_game_settings_time();
        }
    }
    if project_watcher::watching() {
        project_watcher::on_notify();
    }
    extension_watcher::on_notify();
}

pub fn init() {
    enable_timer();
    let _ = extension_watcher::setup_watcher();
}

fn enable_timer() {
    unsafe {
        let main_form = (0x790100 as *const *mut *mut usize).read();
        let timer_ptr = main_form.add(0x65c / 4);
        if timer_ptr.read().is_null() {
            // create timer if needed
            *timer_ptr = delphi_call!(0x48e048, 0x48ab50, 1, main_form);
        }
        let timer = timer_ptr.read();
        timer.add(0x34 / 4).write(1000); // interval (ms)
        timer.add(0x40 / 4).write(every_second as *const () as usize); // event
        timer.add(0x48 / 4).write(1); // enabled
        let _: u32 = delphi_call!(0x48e12c, timer);
    }
}
