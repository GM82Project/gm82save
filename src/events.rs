pub const EV_CREATE: usize = 0;
pub const EV_DESTROY: usize = 1;
pub const EV_ALARM: usize = 2;
pub const EV_STEP: usize = 3;
pub const EV_COLLISION: usize = 4;
pub const EV_KEYBOARD: usize = 5;
pub const EV_MOUSE: usize = 6;
pub const EV_OTHER: usize = 7;
pub const EV_DRAW: usize = 8;
pub const EV_KEYPRESS: usize = 9;
pub const EV_KEYRELEASE: usize = 10;
pub const EV_TRIGGER: usize = 11;

pub const EVENT_ID_TO_NAME: [&str; 12] = [
    "Create",
    "Destroy",
    "Alarm",
    "Step",
    "Collision",
    "Keyboard",
    "Mouse",
    "Other",
    "Draw",
    "KeyPress",
    "KeyRelease",
    "Trigger",
];

pub const EVENT_NAME_TO_ID: phf::Map<&'static str, usize> = phf::phf_map! {
    "Create" => EV_CREATE,
    "Destroy" => EV_DESTROY,
    "Step" => EV_STEP,
    "Alarm" => EV_ALARM,
    "Keyboard" => EV_KEYBOARD,
    "Mouse" => EV_MOUSE,
    "Collision" => EV_COLLISION,
    "Other" => EV_OTHER,
    "Draw" => EV_DRAW,
    "KeyPress" => EV_KEYPRESS,
    "KeyRelease" => EV_KEYRELEASE,
    "Trigger" => EV_TRIGGER,
};
