use crate::{asset, delphi, ide, AssetListTrait, InstanceExtra, TileExtra, UStr, DEFLATE_LEVEL, EXTRA_DATA};
use flate2::{write::ZlibEncoder, Compression};
use rayon::prelude::*;
use std::{arch::asm, io::Write, ptr, slice};

pub trait GetAssetList: Sync + 'static {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self>;
    fn save(&self, exe: bool, out: impl Write);
    fn write_additional(_stream: usize) {}
}

impl GetAssetList for asset::Sprite {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self> {
        &ide::SPRITES
    }

    fn save(&self, exe: bool, mut out: impl Write) {
        out.write_all(&800u32.to_le_bytes()).unwrap();
        out.write_all(&self.origin_x.to_le_bytes()).unwrap();
        out.write_all(&self.origin_y.to_le_bytes()).unwrap();
        out.write_all(&self.frame_count.to_le_bytes()).unwrap();
        for frame in self.get_frames().iter().filter_map(|f| unsafe { f.as_ref() }) {
            if !exe {
                save_frame(frame, &mut out);
            } else {
                unsafe {
                    let f: *mut asset::Frame = delphi_call!(0x701cf4, 0x700d94, 1, frame);
                    let _: u32 = delphi_call!(0x5b0c74, f);
                    save_frame(&*frame, &mut out);
                    let _: u32 = delphi_call!(0x405a7c, f);
                }
            }
        }
        if !exe {
            out.write_all(&self.collision_shape.to_le_bytes()).unwrap();
            out.write_all(&self.alpha_tolerance.to_le_bytes()).unwrap();
        }
        out.write_all(&u32::to_le_bytes(self.per_frame_colliders.into())).unwrap();
        if !exe {
            out.write_all(&self.bbox_type.to_le_bytes()).unwrap();
            out.write_all(&self.bbox_left.to_le_bytes()).unwrap();
            out.write_all(&self.bbox_right.to_le_bytes()).unwrap();
            out.write_all(&self.bbox_bottom.to_le_bytes()).unwrap();
            out.write_all(&self.bbox_top.to_le_bytes()).unwrap();
        } else if self.frame_count > 0 {
            unsafe {
                #[repr(C)]
                struct Mask {
                    _padding: u32,
                    i_w: u32,
                    i_h: u32,
                    i_mask: *const *const u8,
                    i_bbox: [u32; 4],
                }
                unsafe fn write_mask(mask: &Mask, mut out: impl Write) {
                    write_int(800, &mut out);
                    write_int(mask.i_w, &mut out);
                    write_int(mask.i_h, &mut out);
                    write_int(mask.i_bbox[0], &mut out);
                    write_int(mask.i_bbox[2], &mut out);
                    write_int(mask.i_bbox[3], &mut out);
                    write_int(mask.i_bbox[1], &mut out);
                    for y in 0..mask.i_h as usize {
                        for x in 0..mask.i_w as usize {
                            write_int(mask.i_mask.add(x).read().add(y).read().into(), &mut out);
                        }
                    }
                }
                // TODO speed up
                if !self.per_frame_colliders {
                    // create mask
                    let mut mask: *const Mask;
                    asm! {
                        "push dword ptr [{sprite}+0x1c]",
                        "push {bbox}",
                        "push dword ptr [{sprite}+0x10]",
                        "push dword ptr [{sprite}+0x14]",
                        "call {call}",
                        sprite = in(reg) self,
                        call = in(reg) 0x5aecac,
                        bbox = in(reg) &self.bbox_left,
                        inlateout("eax") 0x5ae848 => mask,
                        inlateout("edx") 1 => _,
                        inlateout("ecx") self.get_frames()[0] => _,
                    }
                    // merge mask
                    for &f in &self.get_frames()[1..] {
                        asm! {
                            "push {frame}",
                            "push dword ptr [{sprite}+0x10]",
                            "push dword ptr [{sprite}+0x14]",
                            "call {call}",
                            call = in(reg) 0x5af188,
                            sprite = in(reg) self,
                            frame = in(reg) &self.bbox_left,
                            inlateout("eax") mask => _,
                            inlateout("edx") f => _,
                            inlateout("ecx") self.bbox_type => _,
                        }
                    }
                    write_mask(&*mask, &mut out);
                    // free mask
                    let _: u32 = delphi_call!(0x405a7c, mask);
                } else {
                    // TODO
                    for f in self.get_frames() {
                        // create mask
                        let mut mask: *const Mask;
                        asm! {
                            "push dword ptr [{sprite}+0x1c]",
                            "lea eax, [{sprite}+0x20]",
                            "push eax",
                            "push dword ptr [{sprite}+0x10]",
                            "push dword ptr [{sprite}+0x14]",
                            "mov edx, 1",
                            "mov eax, 0x5ae848",
                            "call {call}",
                            sprite = in(reg) self,
                            call = in(reg) 0x5aecac,
                            inlateout("eax") 0x5ae848 => mask,
                            inlateout("edx") 1 => _,
                            inlateout("ecx") *f => _,
                        }
                        write_mask(&*mask, &mut out);
                        // free mask
                        let _: u32 = delphi_call!(0x405a7c, mask);
                    }
                }
            }
        }
    }
}

impl GetAssetList for asset::Background {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self> {
        &ide::BACKGROUNDS
    }

    fn save(&self, exe: bool, mut out: impl Write) {
        out.write_all(&710u32.to_le_bytes()).unwrap();
        if !exe {
            out.write_all(&u32::to_le_bytes(self.is_tileset.into())).unwrap();
            out.write_all(&self.tile_width.to_le_bytes()).unwrap();
            out.write_all(&self.tile_height.to_le_bytes()).unwrap();
            out.write_all(&self.h_offset.to_le_bytes()).unwrap();
            out.write_all(&self.v_offset.to_le_bytes()).unwrap();
            out.write_all(&self.h_sep.to_le_bytes()).unwrap();
            out.write_all(&self.v_sep.to_le_bytes()).unwrap();
            unsafe { save_frame(&*self.frame, out) };
        } else {
            unsafe {
                let f: *const asset::Frame = delphi_call!(0x701cf4, 0x700d94, 1, self.frame);
                let _: u32 = delphi_call!(0x5b0c74, f);
                save_frame(&*f, out);
                let _: u32 = delphi_call!(0x405a7c, f);
            }
        }
    }
}

impl GetAssetList for asset::Path {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self> {
        &ide::PATHS
    }

    fn save(&self, exe: bool, mut out: impl Write) {
        write_int(530, &mut out);
        write_int(self.connection, &mut out);
        write_int(self.closed.into(), &mut out);
        write_int(self.precision, &mut out);
        if !exe {
            write_int(self.path_editor_room_background as _, &mut out);
            write_int(self.snap_x, &mut out);
            write_int(self.snap_y, &mut out);
        }
        write_int(self.point_count as _, &mut out);
        for p in self.get_points() {
            write_f64(p.x, &mut out);
            write_f64(p.y, &mut out);
            write_f64(p.speed, &mut out);
        }
    }
}

impl GetAssetList for asset::Script {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self> {
        &ide::SCRIPTS
    }

    fn save(&self, _exe: bool, mut out: impl Write) {
        out.write_all(&800u32.to_le_bytes()).unwrap();
        write_string(&self.source, out);
    }
}

static mut OLD_DPI: u32 = 96;

impl GetAssetList for asset::Font {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self> {
        unsafe {
            OLD_DPI = *delphi::DPI;
            *delphi::DPI = 96;
        }
        &ide::FONTS
    }

    fn save(&self, exe: bool, mut out: impl Write) {
        write_int(800, &mut out);
        write_string(&self.sys_name, &mut out);
        write_int(self.size, &mut out);
        write_int(self.bold.into(), &mut out);
        write_int(self.italic.into(), &mut out);
        let charset = if self.charset == 1 { 0 } else { self.charset };
        write_int((self.range_start & 0xffff) | (charset << 16) | ((self.aa_level + 1) << 24), &mut out);
        write_int(self.range_end, &mut out);
        if exe {
            unsafe {
                let _: u32 = delphi_call!(0x5a809c, self);
            }
            for i in 0..256 {
                write_int(self.s_x[i], &mut out);
                write_int(self.s_y[i], &mut out);
                write_int(self.s_w[i], &mut out);
                write_int(self.s_h[i], &mut out);
                write_int(self.s_shift[i], &mut out);
                write_int(self.s_offset[i], &mut out);
            }
            write_int(self.s_bw, &mut out);
            write_int(self.s_bh, &mut out);
            write_int(self.s_bw * self.s_bh, &mut out);
            unsafe {
                out.write_all(slice::from_raw_parts(self.s_bytes, (self.s_bw * self.s_bh) as usize)).unwrap();
                delphi::DynArraySetLength((&self.s_bytes) as *const *mut u8 as *mut *mut u8, 0x5a65a4 as _, 1, 0);
            }
        }
    }

    fn write_additional(_stream: usize) {
        unsafe {
            *delphi::DPI = OLD_DPI;
        }
    }
}

impl GetAssetList for asset::Room {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self> {
        &ide::ROOMS
    }

    fn save(&self, exe: bool, mut out: impl Write) {
        unsafe {
            let _: u32 = delphi_call!(0x6576fc, self); // clean unused assets
        }
        let extra_data = unsafe { EXTRA_DATA.as_ref() };
        let version: u32 = if exe && extra_data.is_some() { 811 } else { 541 };
        write_int(version, &mut out);
        write_string(&self.caption, &mut out);
        write_int(self.width, &mut out);
        write_int(self.height, &mut out);
        if !exe {
            write_int(self.snap_x, &mut out);
            write_int(self.snap_y, &mut out);
            write_int(self.isometric.into(), &mut out);
        }
        write_int(self.speed, &mut out);
        write_int(self.persistent.into(), &mut out);
        write_int(self.bg_colour as _, &mut out);
        write_int(u32::from(self.clear_screen) | (u32::from(self.clear_view) << 1), &mut out);
        write_string(&self.creation_code, &mut out);
        write_int(8, &mut out);
        for b in &self.backgrounds {
            write_int(b.visible_on_start.into(), &mut out);
            write_int(b.is_foreground.into(), &mut out);
            write_int(b.source_bg as _, &mut out);
            write_int(b.xoffset as _, &mut out);
            write_int(b.yoffset as _, &mut out);
            write_int(b.tile_horz as _, &mut out);
            write_int(b.tile_vert as _, &mut out);
            write_int(b.hspeed as _, &mut out);
            write_int(b.vspeed as _, &mut out);
            write_int(b.stretch as _, &mut out);
        }
        write_int(self.views_enabled.into(), &mut out);
        write_int(8, &mut out);
        for v in &self.views {
            write_int(v.visible as _, &mut out);
            write_int(v.source_x as _, &mut out);
            write_int(v.source_y as _, &mut out);
            write_int(v.source_w as _, &mut out);
            write_int(v.source_h as _, &mut out);
            write_int(v.port_x as _, &mut out);
            write_int(v.port_y as _, &mut out);
            write_int(v.port_w as _, &mut out);
            write_int(v.port_h as _, &mut out);
            write_int(v.following_hborder as _, &mut out);
            write_int(v.following_vborder as _, &mut out);
            write_int(v.following_hspeed as _, &mut out);
            write_int(v.following_vspeed as _, &mut out);
            write_int(v.following_target as _, &mut out);
        }
        write_int(self.instance_count as _, &mut out);
        for i in self.get_instances() {
            write_int(i.x as _, &mut out);
            write_int(i.y as _, &mut out);
            write_int(i.object as _, &mut out);
            write_int(i.id as _, &mut out);
            write_string(&i.creation_code, &mut out);
            if !exe {
                write_int(i.locked as _, &mut out);
            } else if let Some(data) = extra_data.map(|(insts, _)| insts.get(&i.id).unwrap_or(&InstanceExtra::DEFAULT))
            {
                write_f64(data.xscale, &mut out);
                write_f64(data.yscale, &mut out);
                write_int(data.blend as _, &mut out);
                write_f64(data.angle, &mut out);
            }
        }
        write_int(self.tile_count as _, &mut out);
        for t in self.get_tiles() {
            write_int(t.x as _, &mut out);
            write_int(t.y as _, &mut out);
            write_int(t.source_bg as _, &mut out);
            write_int(t.u as _, &mut out);
            write_int(t.v as _, &mut out);
            write_int(t.width as _, &mut out);
            write_int(t.height as _, &mut out);
            write_int(t.depth as _, &mut out);
            write_int(t.id as _, &mut out);
            if !exe {
                write_int(t.locked as _, &mut out);
            } else if let Some(data) = extra_data.map(|(_, tiles)| tiles.get(&t.id).unwrap_or(&TileExtra::DEFAULT)) {
                write_f64(data.xscale, &mut out);
                write_f64(data.yscale, &mut out);
                write_int(data.blend as _, &mut out);
            }
        }
        if !exe {
            write_int(self.remember_room_editor_info as _, &mut out);
            write_int(self.editor_width as _, &mut out);
            write_int(self.editor_height as _, &mut out);
            write_int(self.show_grid as _, &mut out);
            write_int(self.show_objects as _, &mut out);
            write_int(self.show_tiles as _, &mut out);
            write_int(self.show_backgrounds as _, &mut out);
            write_int(self.show_foregrounds as _, &mut out);
            write_int(self.show_views as _, &mut out);
            write_int(self.delete_underlying_objects as _, &mut out);
            write_int(self.delete_underlying_tiles as _, &mut out);
            write_int(self.tab as _, &mut out);
            write_int(self.x_position_scroll as _, &mut out);
            write_int(self.y_position_scroll as _, &mut out);
        }
    }

    fn write_additional(stream: usize) {
        unsafe {
            let _: u32 = delphi_call!(0x52f12c, stream, *ide::LAST_INSTANCE_ID);
            let _: u32 = delphi_call!(0x52f12c, stream, *ide::LAST_TILE_ID);
        }
    }
}

fn write_int(i: u32, mut out: impl Write) {
    out.write_all(&i.to_le_bytes()).unwrap();
}

fn write_f64(f: f64, mut out: impl Write) {
    out.write_all(&f.to_le_bytes()).unwrap();
}

fn write_string(s_wide: &UStr, mut out: impl Write) {
    unsafe {
        let mut s_utf8: *const usize = ptr::null();
        let _: u32 = delphi_call!(0x40810c, &mut s_utf8, s_wide.0, 0xfde9);
        if s_utf8.is_null() {
            write_int(0, out);
        } else {
            let len = s_utf8.sub(1).read();
            out.write_all(&len.to_le_bytes()).unwrap();
            out.write_all(slice::from_raw_parts(s_utf8.cast::<u8>(), len)).unwrap();
            let _: u32 = delphi_call!(0x406e5c, &mut s_utf8);
        }
    }
}

fn save_frame(frame: &asset::Frame, mut out: impl Write) {
    let image_size = (frame.width * frame.height * 4) as usize;
    out.write_all(&800u32.to_le_bytes()).unwrap();
    out.write_all(&frame.width.to_le_bytes()).unwrap();
    out.write_all(&frame.height.to_le_bytes()).unwrap();
    if image_size > 0 {
        out.write_all(&image_size.to_le_bytes()).unwrap();
        out.write_all(unsafe { slice::from_raw_parts(frame.data, image_size) }).unwrap();
    }
}

extern "fastcall" fn save_assets<T: GetAssetList>(stream: usize, exe: bool) -> bool {
    let asset_list = T::get_asset_list();
    unsafe {
        let _: u32 = delphi_call!(0x52f12c, stream, 800);
    }
    let assets = asset_list.assets();
    unsafe {
        let _: u32 = delphi_call!(0x52f12c, stream, assets.len());
    }
    (assets, asset_list.names(), asset_list.timestamps())
        .into_par_iter()
        .map(|(asset, name, timestamp)| {
            let mut out = ZlibEncoder::new(Vec::new(), Compression::new(unsafe { DEFLATE_LEVEL }));
            out.write_all(&u32::to_le_bytes(asset.is_some().into())).unwrap();
            if let Some(asset) = asset {
                write_string(name, &mut out);
                if !exe {
                    out.write_all(&timestamp.to_le_bytes()).unwrap();
                }
                asset.save(exe, &mut out);
            }
            out.finish().unwrap()
        })
        .collect::<Vec<_>>()
        .into_iter()
        .for_each(|buf| unsafe {
            let _: u32 = delphi_call!(0x52f12c, stream, buf.len());
            let _: u32 = delphi_call!(0x43f4c0, stream, buf.as_ptr(), buf.len());
        });
    T::write_additional(stream);
    true
}

#[naked]
pub unsafe extern "C" fn save_assets_inj<T: GetAssetList>() {
    asm! {
        "mov ecx, eax",
        "jmp {save_assets}",
        save_assets = sym save_assets::<T>,
        options(noreturn),
    }
}
