#![allow(dead_code)]

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

pub const EVENT_NAMES: [&str; 12] = [
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
