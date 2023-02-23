pub mod extension_watcher;
pub mod project_watcher;

extern "fastcall" fn on_notify() {
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
        timer.add(0x40 / 4).write(on_notify as _); // event
        timer.add(0x48 / 4).write(1); // enabled
        let _: u32 = delphi_call!(0x48e12c, timer);
    }
}
