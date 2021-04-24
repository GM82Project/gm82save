#![allow(dead_code)]

type WStr = *const u16;

#[repr(C)]
pub struct Trigger {
    exists: u32,
    pub name: WStr,
    pub condition: WStr,
    pub constant_name: WStr,
    pub kind: u32,
}

#[repr(C)]
pub struct Sound {
    exists: u32,
    pub kind: u32,
    pub extension: WStr,
    pub effects: u32,
    pub source: WStr,
    pub padding: u32, // ???
    pub volume: f64,
    pub pan: f64,
    pub preload: bool,
    pub data: *const u8,
}

#[repr(C)]
pub struct Frame {
    exists: u32,
    pub width: u32,
    pub height: u32,
    pub data: *const u8,
}

#[repr(C)]
pub struct Sprite {
    exists: u32,
    pub frame_count: u32,
    pub origin_x: i32,
    pub origin_y: i32,
    pub collision_shape: u32,
    pub alpha_tolerance: u32,
    pub per_frame_colliders: bool,
    pub bbox_type: u32,
    pub bbox_left: i32,
    pub bbox_bottom: i32,
    pub bbox_right: i32,
    pub bbox_top: i32,
    pub frames: *const *const Frame,
}

#[repr(C)]
pub struct Background {
    exists: u32,
    pub frame: *const Frame,
    pub is_tileset: bool,
    pub tile_width: u32,
    pub tile_height: u32,
    pub h_offset: u32,
    pub v_offset: u32,
    pub h_sep: u32,
    pub v_sep: u32,
}

#[repr(C)]
pub struct PathPoint {
    pub x: f64,
    pub y: f64,
    pub speed: f64,
}

#[repr(C)]
pub struct Path {
    exists: u32,
    pub points: *const PathPoint,
    pub point_count: u32,
    pub connection: u32,
    pub closed: bool,
    pub precision: u32,
    pub padding: u128, // ???
    pub path_editor_room_background: i32,
    pub snap_x: u32,
    pub snap_y: u32,
}

#[repr(C)]
pub struct Script {
    exists: u32,
    pub source: WStr,
}

#[repr(C)]
pub struct Font {
    exists: u32,
    pub sys_name: WStr,
    pub size: u32,
    pub bold: bool,
    pub italic: bool,
    pub range_start: u32,
    pub range_end: u32,
    pub charset: u32,
    pub aa_level: u32,
}

#[repr(C)]
pub struct Action {
    exists: u32,
    pub lib_id: u32,
    pub id: u32,
    pub action_kind: u32,
    pub can_be_relative: bool,
    pub is_condition: bool,
    pub applies_to_something: bool,
    pub execution_type: u32,
    pub fn_name: WStr,
    pub fn_code: WStr,
    pub param_count: u32,
    pub param_types: [u32; 8],
    pub applies_to: i32,
    pub is_relative: bool,
    pub param_strings: [WStr; 8],
    pub invert_condition: bool,
}

#[repr(C)]
pub struct Event {
    exists: u32,
    pub actions: *const *const Action,
    pub action_count: u32,
}

#[repr(C)]
pub struct Timeline {
    exists: u32,
    pub moment_events: *const *const Event,
    pub moment_times: *const u32,
    pub moment_count: u32,
}

#[repr(C)]
pub struct Object {
    exists: u32,
    pub sprite_index: i32,
    pub solid: bool,
    pub visible: bool,
    pub depth: i32,
    pub persistent: bool,
    pub parent_index: i32,
    pub mask_index: i32,
    pub events: [*const *const Event; 12],
}

#[repr(C)]
pub struct RoomBackground {
    pub visible_on_start: bool,
    pub is_foreground: bool,
    pub source_bg: i32,
    pub xoffset: i32,
    pub yoffset: i32,
    pub tile_horz: bool,
    pub tile_vert: bool,
    pub hspeed: i32,
    pub vspeed: i32,
    pub stretch: bool,
}

#[repr(C)]
pub struct View {
    pub visible: bool,
    pub source_x: i32,
    pub source_y: i32,
    pub source_w: u32,
    pub source_h: u32,
    pub port_x: u32,
    pub port_y: u32,
    pub port_w: u32,
    pub port_h: u32,
    pub following_hborder: u32,
    pub following_vborder: u32,
    pub following_hspeed: u32,
    pub following_vspeed: u32,
    pub following_target: i32,
}

#[repr(C)]
pub struct Instance {
    pub x: i32,
    pub y: i32,
    pub object: i32,
    pub id: u32,
    pub creation_code: WStr,
    pub locked: bool,
}

#[repr(C)]
pub struct Tile {
    pub x: i32,
    pub y: i32,
    pub source_bg: i32,
    pub tile_x: u32,
    pub tile_y: u32,
    pub width: u32,
    pub height: u32,
    pub depth: i32,
    pub id: i32,
    pub locked: bool,
}

#[repr(C)]
pub struct Room {
    exists: u32,
    pub caption: WStr,
    pub speed: u32,
    pub width: u32,
    pub height: u32,
    pub snap_x: u32,
    pub snap_y: u32,
    pub isometric: bool,
    pub persistent: bool,
    pub bg_colour: i32,
    pub clear_screen: bool,
    pub backgrounds: [RoomBackground; 8],
    pub views_enabled: bool,
    pub clear_region: bool,
    pub views: [View; 8],
    pub creation_code: WStr,
    pub instance_count: u32,
    pub instances: *const Instance,
    pub tile_count: u32,
    pub tiles: *const Tile,
    pub remember_room_editor_info: bool,
    pub editor_width: u32,
    pub editor_height: u32,
    pub show_grid: bool,
    pub show_objects: bool,
    pub show_tiles: bool,
    pub show_backgrounds: bool,
    pub show_foregrounds: bool,
    pub show_views: bool,
    pub delete_underlying_objects: bool,
    pub delete_underlying_tiles: bool,
    pub tab: u32,
    pub x_position_scroll: u32,
    pub y_position_scroll: u32,
}

#[repr(C)]
pub struct IncludedFile {
    exists: u32,
    pub file_name: WStr,
    pub source_path: WStr,
    pub data_exists: bool,
    pub source_length: u32,
    pub stored_in_gmk: bool,
    pub data: *const u8,
    pub export_setting: u32,
    pub export_custom_folder: WStr,
    pub overwrite_file: bool,
    pub free_memory: bool,
    pub remove_at_end: bool,
}

#[repr(C)]
pub struct Extension {
    exists: u32,
    pub name: WStr,
    // other stuff that doesn't get written to the gmk
}

#[repr(C)]
pub struct Library {
    stuff: [u8; 0x24], // ???
    pub init_code: WStr,
}
