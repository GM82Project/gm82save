#![allow(dead_code)]

use crate::{
    delphi::{TBitmap, TMemoryStream, UStr},
    delphi_call,
    list::DelphiList,
};
use std::slice;

#[repr(C)]
pub struct Trigger {
    vmt: u32,
    pub name: UStr,
    pub condition: UStr,
    pub constant_name: UStr,
    pub kind: u32,
}

impl Trigger {
    pub unsafe fn new() -> *mut Self {
        delphi_call!(0x62d200, 0x62cf48, 1)
    }
}

#[repr(C)]
pub struct Sound {
    vmt: u32,
    pub kind: u32,
    pub extension: UStr,
    pub effects: u32,
    pub source: UStr,
    pub padding: u32, // align f64 to 8 bytes
    pub volume: f64,
    pub pan: f64,
    pub preload: bool,
    pub data: *const TMemoryStream,
}

impl Sound {
    pub unsafe fn new() -> *mut Self {
        delphi_call!(0x64fb70, 0x64f674, 1)
    }
}

unsafe impl Sync for Sound {}

#[repr(C)]
pub struct Frame {
    vmt: u32,
    pub width: u32,
    pub height: u32,
    pub data: *const u8,
}

impl Frame {
    pub unsafe fn new() -> *mut Self {
        delphi_call!(0x701bf8, 0x700d94, 1)
    }

    pub unsafe fn load_from_file(&mut self, file: &UStr) {
        let _: u32 = delphi_call!(0x7026A4, self, file);
    }

    pub unsafe fn thumb(&self, out: &mut [u8], flip: bool) {
        // note: this assumes output format == input format
        // these are stored as BGRA8 so make sure to double check what the output should be
        use itertools::Itertools;
        let data = slice::from_raw_parts(self.data, (self.width * self.height * 4) as usize);
        let (width, height) = if self.width > self.height {
            (16, (self.height * 16 / self.width) as usize)
        } else {
            ((self.width * 16 / self.height) as usize, 16)
        };
        let (hoffset, voffset) = (8 - width / 2, 8 - height / 2);
        for (y, row) in out.chunks_exact_mut(16 * 4).enumerate() {
            // vertical flip for BMP
            let y = if flip { 15 - y } else { y };
            if y < voffset || y >= voffset + height {
                row.fill(0);
                continue
            }
            let y = y - voffset;
            for (x, px) in row.chunks_exact_mut(4).enumerate() {
                if x < hoffset || x >= hoffset + width {
                    px.fill(0);
                    continue
                }
                let x = x - hoffset;
                // get sample points
                let ox = (x as f64) / (width as f64) * f64::from(self.width);
                let ox2 = ox + 0.5 / (width as f64) * f64::from(self.width);
                let oy = (y as f64) / (height as f64) * f64::from(self.height);
                let oy2 = oy + 0.5 / (height as f64) * f64::from(self.height);
                // sum all pixels
                let mut px_count = 0.0;
                let sum_px = [ox, ox2]
                    .iter()
                    .map(|x| x.floor() as usize)
                    .cartesian_product([oy, oy2].iter().map(|x| x.floor() as usize))
                    .fold([0.0f64; 4], |mut px, (ox, oy)| {
                        let offset = (oy * self.width as usize + ox) * 4;
                        let in_px = &data[offset..offset + 4];
                        if in_px[3] != 0 {
                            px_count += 1.0;
                            px.iter_mut().zip(in_px).for_each(|(o, i)| *o += f64::from(*i));
                        }
                        px
                    });
                if px_count != 0.0 {
                    // average and place into output
                    px[..3].iter_mut().zip(sum_px).for_each(|(o, i)| *o = (i / px_count).floor() as u8);
                    // blend semi-transparent to white
                    let alpha = sum_px[3] / 4.0 / 255.0;
                    if alpha != 1.0 {
                        px[..3].iter_mut().for_each(|c| *c += (f64::from(255 - *c) * (1.0 - alpha)) as u8);
                    }
                    px[3] = 255;
                } else {
                    // fully transparent
                    px.fill(0);
                }
            }
        }
    }

    pub unsafe fn register_thumb(&self) -> i32 {
        let mut icon: [u8; 16 * 16 * 4] = std::mem::MaybeUninit::uninit().assume_init();
        self.thumb(&mut icon, false);
        let mut mask: [u8; 16 * 16 / 8] = std::mem::MaybeUninit::uninit().assume_init();
        for (dst, src) in mask.iter_mut().zip(icon.chunks_exact(8 * 4)) {
            *dst = u8::from(src[3] == 0) << 7
                | u8::from(src[3 + 4] == 0) << 6
                | u8::from(src[3 + 4 * 2] == 0) << 5
                | u8::from(src[3 + 4 * 3] == 0) << 4
                | u8::from(src[3 + 4 * 4] == 0) << 3
                | u8::from(src[3 + 4 * 5] == 0) << 2
                | u8::from(src[3 + 4 * 6] == 0) << 1
                | u8::from(src[3 + 4 * 7] == 0);
        }
        let CreateBitmap: extern "stdcall" fn(u32, u32, u32, u32, *const u8) -> usize = std::mem::transmute(0x40e008);
        let ImageList_Add: extern "stdcall" fn(usize, usize, usize) -> i32 = std::mem::transmute(0x40f8ec);
        let DeleteObject: extern "stdcall" fn(usize) -> bool = std::mem::transmute(0x40e098);
        let bitmap = CreateBitmap(16, 16, 1, 32, icon.as_ptr());
        let mask_bitmap = CreateBitmap(16, 16, 1, 1, mask.as_ptr());
        let thumb = ImageList_Add((0x789b38 as *const *const usize).read().add(16).read(), bitmap, mask_bitmap);
        DeleteObject(bitmap);
        DeleteObject(mask_bitmap);
        thumb
    }
}

#[repr(C)]
pub struct Sprite {
    vmt: u32,
    pub frame_count: u32,
    pub origin_x: i32,
    pub origin_y: i32,
    pub collision_shape: u32,
    pub alpha_tolerance: u32,
    pub per_frame_colliders: bool,
    pub bbox_type: u32,
    pub bbox_left: i32,
    pub bbox_top: i32,
    pub bbox_right: i32,
    pub bbox_bottom: i32,
    frames: DelphiList<*mut Frame, 0x5b2754>,
}

impl Sprite {
    pub unsafe fn new() -> *mut Sprite {
        delphi_call!(0x5b325c, 0x5b27dc, 1)
    }

    pub unsafe fn get_icon(&self) -> *const TBitmap {
        delphi_call!(0x5b401c, self)
    }

    pub fn alloc_frames(&mut self, count: usize) -> &mut [*mut Frame] {
        self.frames.alloc(count);
        self.frame_count = count as u32;
        unsafe { self.frames.get_unchecked_mut(..self.frame_count as usize) }
    }

    pub fn get_frames(&self) -> &[*mut Frame] {
        unsafe { self.frames.get_unchecked(..self.frame_count as usize) }
    }
}

#[repr(C)]
pub struct Background {
    vmt: u32,
    pub frame: *mut Frame,
    pub is_tileset: bool,
    pub tile_width: u32,
    pub tile_height: u32,
    pub h_offset: u32,
    pub v_offset: u32,
    pub h_sep: u32,
    pub v_sep: u32,
}

impl Background {
    pub unsafe fn new() -> *mut Background {
        delphi_call!(0x062dba4, 0x62d408, 1)
    }

    pub unsafe fn get_icon(&self) -> *const TBitmap {
        delphi_call!(0x62e5e8, self)
    }
}

unsafe impl Sync for Background {}

#[repr(C)]
pub struct PathPoint {
    pub x: f64,
    pub y: f64,
    pub speed: f64,
}

#[repr(C)]
pub struct Path {
    vmt: u32,
    points: DelphiList<PathPoint, 0x534870>,
    pub point_count: usize,
    pub connection: u32,
    pub closed: bool,
    pub precision: u32,
    padding: u128,
    pub path_editor_room_background: i32,
    pub snap_x: u32,
    pub snap_y: u32,
}

impl Path {
    pub unsafe fn new() -> *mut Self {
        delphi_call!(0x5357b0, 0x534924, 1)
    }

    pub unsafe fn commit(&mut self) {
        let _: u32 = delphi_call!(0x53578c, self);
    }

    pub unsafe fn alloc_points(&mut self, count: usize) -> &mut [PathPoint] {
        self.point_count = count;
        self.points.alloc(count);
        self.points.get_unchecked_mut(..count)
    }

    pub fn get_points(&self) -> &[PathPoint] {
        unsafe { self.points.get_unchecked(..self.point_count) }
    }
}

#[repr(C)]
pub struct Script {
    vmt: u32,
    pub source: UStr,
}

impl Script {
    pub unsafe fn new() -> *mut Self {
        delphi_call!(0x652860, 0x65267c, 1)
    }
}

#[repr(C)]
pub struct Font {
    vmt: u32,
    pub sys_name: UStr,
    pub size: u32,
    pub bold: bool,
    pub italic: bool,
    pub range_start: u32,
    pub range_end: u32,
    pub charset: u32,
    /// This is 1 less than what you'll see saved in .gmk or .exe or .gm81 or whatever
    pub aa_level: u32,
}

impl Font {
    pub unsafe fn new() -> *mut Self {
        delphi_call!(0x5a8760, 0x5a6628, 1)
    }
}

#[repr(C)]
pub struct Action {
    vmt: u32,
    pub lib_id: u32,
    pub id: u32,
    pub action_kind: u32,
    pub can_be_relative: bool,
    pub is_condition: bool,
    pub applies_to_something: bool,
    pub execution_type: u32,
    pub fn_name: UStr,
    pub fn_code: UStr,
    pub param_count: u32,
    pub param_types: [u32; 8],
    pub applies_to: i32,
    pub is_relative: bool,
    pub param_strings: [UStr; 8],
    pub invert_condition: bool,
}

impl Action {
    pub unsafe fn fill_in(&mut self, lib_id: u32, act_id: u32) {
        let _: u32 = delphi_call!(0x710544, self, lib_id, act_id);
    }
}

#[repr(C)]
pub struct Event {
    vmt: u32,
    actions: DelphiList<*const Action, 0x5a4be4>,
    pub action_count: u32,
}

impl Event {
    pub unsafe fn new() -> *mut Self {
        delphi_call!(0x5a5048, 0x5a4c6c, 1)
    }

    pub unsafe fn add_action(&mut self, libid: u32, actid: u32) -> *mut Action {
        delphi_call!(0x5a51d4, self, libid, actid)
    }

    pub fn get_actions(&self) -> &[*const Action] {
        unsafe { self.actions.get_unchecked(..self.action_count as usize) }
    }
}

#[repr(C)]
pub struct Timeline {
    vmt: u32,
    pub moment_events: DelphiList<*mut Event, 0x5ad8c8>,
    pub moment_times: DelphiList<u32, 0x5ad900>,
    pub moment_count: usize,
}

impl Timeline {
    pub unsafe fn new() -> *mut Self {
        delphi_call!(0x5adf3c, 0x5ad98c, 1)
    }

    pub unsafe fn alloc(&mut self, count: usize) -> (&mut [*mut Event], &mut [u32]) {
        self.moment_count = count;
        self.moment_events.alloc(count);
        self.moment_times.alloc(count);
        (self.moment_events.get_unchecked_mut(..count), self.moment_times.get_unchecked_mut(..count))
    }

    pub fn get_events(&self) -> &[*mut Event] {
        unsafe { self.moment_events.get_unchecked(..self.moment_count) }
    }

    pub fn get_times(&self) -> &[u32] {
        unsafe { self.moment_times.get_unchecked(..self.moment_count) }
    }
}

#[repr(C)]
pub struct Object {
    vmt: u32,
    pub sprite_index: i32,
    pub solid: bool,
    pub visible: bool,
    pub depth: i32,
    pub persistent: bool,
    pub parent_index: i32,
    pub mask_index: i32,
    pub events: [DelphiList<*mut Event, 0x5ad8c8>; 12],
}

impl Object {
    pub unsafe fn new() -> *mut Object {
        delphi_call!(0x7049a8, 0x704428, 1)
    }

    pub unsafe fn get_event(&mut self, ev_type: usize, ev_numb: usize) -> *mut Event {
        delphi_call!(0x704d74, self, ev_type, ev_numb)
    }
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
    pub port_x: i32,
    pub port_y: i32,
    pub port_w: u32,
    pub port_h: u32,
    pub following_hborder: i32,
    pub following_vborder: i32,
    pub following_hspeed: i32,
    pub following_vspeed: i32,
    pub following_target: i32,
}

#[repr(C)]
pub struct Instance {
    pub x: i32,
    pub y: i32,
    pub object: i32,
    pub id: usize,
    pub creation_code: UStr,
    pub locked: bool,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Tile {
    pub x: i32,
    pub y: i32,
    pub source_bg: i32,
    pub u: i32,
    pub v: i32,
    pub width: i32,
    pub height: i32,
    pub depth: i32,
    pub id: usize,
    pub locked: bool,
}

#[repr(C)]
pub struct Room {
    vmt: u32,
    pub caption: UStr,
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
    pub clear_view: bool,
    pub views: [View; 8],
    pub creation_code: UStr,
    pub instance_count: usize,
    instances: DelphiList<Instance, 0x656418>,
    pub tile_count: usize,
    tiles: DelphiList<Tile, 0x656448>,
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

impl Room {
    pub unsafe fn new() -> *mut Self {
        delphi_call!(0x6577b8, 0x6564cc, 1)
    }

    pub unsafe fn calc_extents(&mut self) {
        let _: u32 = delphi_call!(0x657b48, self);
    }

    pub unsafe fn alloc_instances(&mut self, count: usize) -> &mut [Instance] {
        self.instance_count = count;
        self.instances.alloc(count);
        &mut self.instances[..count]
    }

    pub fn get_instances(&self) -> &[Instance] {
        unsafe { self.instances.get_unchecked(..self.instance_count) }
    }

    pub fn get_instances_mut(&mut self) -> &mut [Instance] {
        unsafe { self.instances.get_unchecked_mut(..self.instance_count) }
    }

    pub unsafe fn put_tiles(&mut self, tiles: Vec<Tile>) {
        let count = tiles.len();
        self.tile_count = count;
        self.tiles.alloc(count);
        self.tiles[..count].copy_from_slice(&tiles);
    }

    pub fn get_tiles(&self) -> &[Tile] {
        unsafe { self.tiles.get_unchecked(..self.tile_count) }
    }

    pub fn get_tiles_mut(&mut self) -> &mut [Tile] {
        unsafe { self.tiles.get_unchecked_mut(..self.tile_count) }
    }
}

#[repr(C)]
pub struct IncludedFile {
    vmt: u32,
    pub file_name: UStr,
    pub source_path: UStr,
    pub data_exists: bool,
    pub source_length: u32,
    pub stored_in_gmk: bool,
    pub data: *const TMemoryStream,
    pub export_setting: u32,
    pub export_custom_folder: UStr,
    pub overwrite_file: bool,
    pub free_memory: bool,
    pub remove_at_end: bool,
}

impl IncludedFile {
    pub unsafe fn new() -> *mut Self {
        delphi_call!(0x6ca800, 0x6ca360, 1)
    }
}

#[repr(C)]
pub struct Extension {
    vmt: u32,
    pub name: UStr,
    // other stuff that doesn't get written to the gmk
}

#[repr(C)]
pub struct ActionDefinition {
    vmt: u32,
    name: UStr,
    pub id: u32,
    image: u32,      // pointer
    image_list: u32, // also pointer
    image_index: u32,
    hidden: bool,
    advanced: bool,
    pro_only: bool,
    short_desc: UStr,
    list_text: UStr,
    hint_text: UStr,
    kind: u32,
    interface: u32,
    question: bool,
    apply_to: bool,
    relative: bool,
    arg_count: u32,
    arg_captions: [UStr; 8],
    arg_types: [u32; 8],
    arg_defaults: [UStr; 8],
    arg_menu_lens: [UStr; 8],
    execution_type: u32,
    function_name: UStr,
    code_string: UStr,
}

#[repr(C)]
pub struct ActionLibrary {
    vmt: u32,
    caption: UStr,
    pub id: u32,
    author: UStr,
    version: u32,
    padding: u32,
    last_changed: f64,
    information: UStr,
    pub init_code: UStr,
    advanced: bool,
    pub action_count: usize,
    pub actions: *const &'static ActionDefinition,
    max_id: u32,
    // also an image list but who cares
}

pub type Form = u32; // pointer but eh
