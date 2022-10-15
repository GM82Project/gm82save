use crate::{
    asset, delphi, delphi::TMemoryStream, ide, AssetListTrait, InstanceExtra, TileExtra, UStr, DEFLATE_LEVEL,
    EXTRA_DATA,
};
use byteorder::{WriteBytesExt, LE};
use flate2::{write::ZlibEncoder, Compression};
use rayon::prelude::*;
use std::{arch::asm, io, io::Write, ptr, slice};

pub trait GetAssetList: Sync + 'static {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self>;
    fn save(&self, exe: bool, out: impl Write) -> io::Result<()>;
    fn write_additional(_stream: &mut TMemoryStream) -> io::Result<()> {
        Ok(())
    }
}

impl GetAssetList for asset::Sprite {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self> {
        &ide::SPRITES
    }

    fn save(&self, exe: bool, mut out: impl Write) -> io::Result<()> {
        out.write_u32::<LE>(800)?;
        out.write_i32::<LE>(self.origin_x)?;
        out.write_i32::<LE>(self.origin_y)?;
        out.write_u32::<LE>(self.frame_count as u32)?;
        for frame in self.get_frames() {
            if !exe {
                save_frame(frame, &mut out)?;
            } else {
                let mut f = frame.duplicate();
                f.prepare_for_export();
                save_frame(&f, &mut out)?;
            }
        }
        if !exe {
            out.write_u32::<LE>(self.collision_shape)?;
            out.write_u32::<LE>(self.alpha_tolerance)?;
        }
        out.write_u32::<LE>(self.per_frame_colliders.into())?;
        if !exe {
            out.write_u32::<LE>(self.bbox_type)?;
            out.write_i32::<LE>(self.bbox_left)?;
            out.write_i32::<LE>(self.bbox_right)?;
            out.write_i32::<LE>(self.bbox_bottom)?;
            out.write_i32::<LE>(self.bbox_top)?;
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
                unsafe fn write_mask(mask: &Mask, mut out: impl Write) -> io::Result<()> {
                    out.write_u32::<LE>(800)?;
                    out.write_u32::<LE>(mask.i_w)?;
                    out.write_u32::<LE>(mask.i_h)?;
                    out.write_u32::<LE>(mask.i_bbox[0])?;
                    out.write_u32::<LE>(mask.i_bbox[2])?;
                    out.write_u32::<LE>(mask.i_bbox[3])?;
                    out.write_u32::<LE>(mask.i_bbox[1])?;
                    for y in 0..mask.i_h as usize {
                        for x in 0..mask.i_w as usize {
                            out.write_u32::<LE>(mask.i_mask.add(x).read().add(y).read().into())?;
                        }
                    }
                    Ok(())
                }
                // TODO speed up
                if !self.per_frame_colliders {
                    // create mask
                    let f: &asset::Frame = &self.get_frames()[0];
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
                        in("edx") 1,
                        in("ecx") f,
                        clobber_abi("C"),
                    }
                    // merge mask
                    for f in &self.get_frames()[1..] {
                        let f: &asset::Frame = f;
                        asm! {
                            "push {frame}",
                            "push dword ptr [{sprite}+0x10]",
                            "push dword ptr [{sprite}+0x14]",
                            "call {call}",
                            call = in(reg) 0x5af188,
                            sprite = in(reg) self,
                            frame = in(reg) &self.bbox_left,
                            in("eax") mask,
                            in("edx") f,
                            in("ecx") self.bbox_type,
                            clobber_abi("C"),
                        }
                    }
                    write_mask(&*mask, &mut out)?;
                    // free mask
                    let _: u32 = delphi_call!(0x405a7c, mask);
                } else {
                    // TODO
                    for f in self.get_frames() {
                        let f: &asset::Frame = f;
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
                            in("edx") 1,
                            in("ecx") f,
                            clobber_abi("C"),
                        }
                        write_mask(&*mask, &mut out)?;
                        // free mask
                        let _: u32 = delphi_call!(0x405a7c, mask);
                    }
                }
            }
        }
        Ok(())
    }
}

impl GetAssetList for asset::Background {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self> {
        &ide::BACKGROUNDS
    }

    fn save(&self, exe: bool, mut out: impl Write) -> io::Result<()> {
        out.write_u32::<LE>(710)?;
        if !exe {
            out.write_u32::<LE>(self.is_tileset.into())?;
            out.write_u32::<LE>(self.tile_width)?;
            out.write_u32::<LE>(self.tile_height)?;
            out.write_u32::<LE>(self.h_offset)?;
            out.write_u32::<LE>(self.v_offset)?;
            out.write_u32::<LE>(self.h_sep)?;
            out.write_u32::<LE>(self.v_sep)?;
            save_frame(&self.frame, out)?;
        } else {
            let mut f = self.frame.duplicate();
            f.prepare_for_export();
            save_frame(&f, out)?;
        }
        Ok(())
    }
}

impl GetAssetList for asset::Path {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self> {
        &ide::PATHS
    }

    fn save(&self, exe: bool, mut out: impl Write) -> io::Result<()> {
        out.write_u32::<LE>(530)?;
        out.write_u32::<LE>(self.connection)?;
        out.write_u32::<LE>(self.closed.into())?;
        out.write_u32::<LE>(self.precision)?;
        if !exe {
            out.write_u32::<LE>(self.path_editor_room_background as _)?;
            out.write_u32::<LE>(self.snap_x)?;
            out.write_u32::<LE>(self.snap_y)?;
        }
        out.write_u32::<LE>(self.point_count as _)?;
        for p in self.get_points() {
            out.write_f64::<LE>(p.x)?;
            out.write_f64::<LE>(p.y)?;
            out.write_f64::<LE>(p.speed)?;
        }
        Ok(())
    }
}

impl GetAssetList for asset::Script {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self> {
        &ide::SCRIPTS
    }

    fn save(&self, _exe: bool, mut out: impl Write) -> io::Result<()> {
        out.write_u32::<LE>(800)?;
        write_string(&self.source, out)?;
        Ok(())
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

    fn save(&self, exe: bool, mut out: impl Write) -> io::Result<()> {
        out.write_u32::<LE>(800)?;
        write_string(&self.sys_name, &mut out)?;
        out.write_u32::<LE>(self.size)?;
        out.write_u32::<LE>(self.bold.into())?;
        out.write_u32::<LE>(self.italic.into())?;
        let charset = if self.charset == 1 { 0 } else { self.charset };
        out.write_u32::<LE>((self.range_start & 0xffff) | (charset << 16) | ((self.aa_level + 1) << 24))?;
        out.write_u32::<LE>(self.range_end)?;
        if exe {
            unsafe {
                // oh no
                (*(self as *const Self as *mut Self)).render();
            }
            for i in 0..256 {
                out.write_u32::<LE>(self.s_x[i])?;
                out.write_u32::<LE>(self.s_y[i])?;
                out.write_u32::<LE>(self.s_w[i])?;
                out.write_u32::<LE>(self.s_h[i])?;
                out.write_u32::<LE>(self.s_shift[i])?;
                out.write_u32::<LE>(self.s_offset[i])?;
            }
            out.write_u32::<LE>(self.s_bw)?;
            out.write_u32::<LE>(self.s_bh)?;
            unsafe {
                write_buffer(&self.s_bytes.get_unchecked(..(self.s_bw * self.s_bh) as usize), out)?;
                self.s_bytes.alloc_evil(0);
            }
        }
        Ok(())
    }

    fn write_additional(_stream: &mut TMemoryStream) -> io::Result<()> {
        unsafe {
            *delphi::DPI = OLD_DPI;
        }
        Ok(())
    }
}

impl GetAssetList for asset::Room {
    fn get_asset_list() -> &'static dyn AssetListTrait<Self> {
        &ide::ROOMS
    }

    fn save(&self, exe: bool, mut out: impl Write) -> io::Result<()> {
        unsafe {
            let _: u32 = delphi_call!(0x6576fc, self); // clean unused assets
        }
        let extra_data = unsafe { EXTRA_DATA.as_ref() };
        let version: u32 = if exe && extra_data.is_some() { 811 } else { 541 };
        out.write_u32::<LE>(version)?;
        write_string(&self.caption, &mut out)?;
        out.write_u32::<LE>(self.width)?;
        out.write_u32::<LE>(self.height)?;
        if !exe {
            out.write_u32::<LE>(self.snap_x)?;
            out.write_u32::<LE>(self.snap_y)?;
            out.write_u32::<LE>(self.isometric.into())?;
        }
        out.write_u32::<LE>(self.speed)?;
        out.write_u32::<LE>(self.persistent.into())?;
        out.write_u32::<LE>(self.bg_colour as _)?;
        out.write_u32::<LE>(u32::from(self.clear_screen) | (u32::from(self.clear_view) << 1))?;
        write_string(&self.creation_code, &mut out)?;
        out.write_u32::<LE>(8)?;
        for b in &self.backgrounds {
            out.write_u32::<LE>(b.visible_on_start.into())?;
            out.write_u32::<LE>(b.is_foreground.into())?;
            out.write_u32::<LE>(b.source_bg as _)?;
            out.write_u32::<LE>(b.xoffset as _)?;
            out.write_u32::<LE>(b.yoffset as _)?;
            out.write_u32::<LE>(b.tile_horz as _)?;
            out.write_u32::<LE>(b.tile_vert as _)?;
            out.write_u32::<LE>(b.hspeed as _)?;
            out.write_u32::<LE>(b.vspeed as _)?;
            out.write_u32::<LE>(b.stretch as _)?;
        }
        out.write_u32::<LE>(self.views_enabled.into())?;
        out.write_u32::<LE>(8)?;
        for v in &self.views {
            out.write_u32::<LE>(v.visible as _)?;
            out.write_u32::<LE>(v.source_x as _)?;
            out.write_u32::<LE>(v.source_y as _)?;
            out.write_u32::<LE>(v.source_w as _)?;
            out.write_u32::<LE>(v.source_h as _)?;
            out.write_u32::<LE>(v.port_x as _)?;
            out.write_u32::<LE>(v.port_y as _)?;
            out.write_u32::<LE>(v.port_w as _)?;
            out.write_u32::<LE>(v.port_h as _)?;
            out.write_u32::<LE>(v.following_hborder as _)?;
            out.write_u32::<LE>(v.following_vborder as _)?;
            out.write_u32::<LE>(v.following_hspeed as _)?;
            out.write_u32::<LE>(v.following_vspeed as _)?;
            out.write_u32::<LE>(v.following_target as _)?;
        }
        out.write_u32::<LE>(self.instance_count as _)?;
        for i in self.get_instances() {
            out.write_u32::<LE>(i.x as _)?;
            out.write_u32::<LE>(i.y as _)?;
            out.write_u32::<LE>(i.object as _)?;
            out.write_u32::<LE>(i.id as _)?;
            write_string(&i.creation_code, &mut out)?;
            if !exe {
                out.write_u32::<LE>(i.locked as _)?;
            } else if let Some(data) = extra_data.map(|(insts, _)| insts.get(&i.id).unwrap_or(&InstanceExtra::DEFAULT))
            {
                out.write_f64::<LE>(data.xscale)?;
                out.write_f64::<LE>(data.yscale)?;
                out.write_u32::<LE>(data.blend as _)?;
                out.write_f64::<LE>(data.angle)?;
            }
        }
        out.write_u32::<LE>(self.tile_count as _)?;
        for t in self.get_tiles() {
            out.write_u32::<LE>(t.x as _)?;
            out.write_u32::<LE>(t.y as _)?;
            out.write_u32::<LE>(t.source_bg as _)?;
            out.write_u32::<LE>(t.u as _)?;
            out.write_u32::<LE>(t.v as _)?;
            out.write_u32::<LE>(t.width as _)?;
            out.write_u32::<LE>(t.height as _)?;
            out.write_u32::<LE>(t.depth as _)?;
            out.write_u32::<LE>(t.id as _)?;
            if !exe {
                out.write_u32::<LE>(t.locked as _)?;
            } else if let Some(data) = extra_data.map(|(_, tiles)| tiles.get(&t.id).unwrap_or(&TileExtra::DEFAULT)) {
                out.write_f64::<LE>(data.xscale)?;
                out.write_f64::<LE>(data.yscale)?;
                out.write_u32::<LE>(data.blend as _)?;
            }
        }
        if !exe {
            out.write_u32::<LE>(self.remember_room_editor_info as _)?;
            out.write_u32::<LE>(self.editor_width as _)?;
            out.write_u32::<LE>(self.editor_height as _)?;
            out.write_u32::<LE>(self.show_grid as _)?;
            out.write_u32::<LE>(self.show_objects as _)?;
            out.write_u32::<LE>(self.show_tiles as _)?;
            out.write_u32::<LE>(self.show_backgrounds as _)?;
            out.write_u32::<LE>(self.show_foregrounds as _)?;
            out.write_u32::<LE>(self.show_views as _)?;
            out.write_u32::<LE>(self.delete_underlying_objects as _)?;
            out.write_u32::<LE>(self.delete_underlying_tiles as _)?;
            out.write_u32::<LE>(self.tab as _)?;
            out.write_u32::<LE>(self.x_position_scroll as _)?;
            out.write_u32::<LE>(self.y_position_scroll as _)?;
        }
        Ok(())
    }

    fn write_additional(stream: &mut TMemoryStream) -> io::Result<()> {
        unsafe {
            stream.write_u32::<LE>(*ide::LAST_INSTANCE_ID as _)?;
            stream.write_u32::<LE>(*ide::LAST_TILE_ID as _)?;
        }
        Ok(())
    }
}

fn write_string(s_wide: &UStr, mut out: impl Write) -> io::Result<()> {
    unsafe {
        let mut s_utf8: *const usize = ptr::null();
        let _: u32 = delphi_call!(0x40810c, &mut s_utf8, s_wide.0, 0xfde9);
        if s_utf8.is_null() {
            out.write_u32::<LE>(0)?;
        } else {
            let len = s_utf8.sub(1).read();
            write_buffer(slice::from_raw_parts(s_utf8.cast::<u8>(), len), out)?;
            let _: u32 = delphi_call!(0x406e5c, &mut s_utf8);
        }
    }
    Ok(())
}

fn write_buffer(buf: &[u8], mut out: impl Write) -> io::Result<()> {
    out.write_u32::<LE>(buf.len() as u32)?;
    out.write_all(buf)
}

fn save_frame(frame: &asset::Frame, mut out: impl Write) -> io::Result<()> {
    out.write_u32::<LE>(800)?;
    out.write_u32::<LE>(frame.width)?;
    out.write_u32::<LE>(frame.height)?;
    let data = frame.get_data();
    if !data.is_empty() {
        write_buffer(data, &mut out)?;
    }
    Ok(())
}

extern "fastcall" fn save_assets<T: GetAssetList>(mut stream: &mut TMemoryStream, exe: bool) -> bool {
    let asset_list = T::get_asset_list();
    stream.write_u32::<LE>(800).unwrap();
    let assets = asset_list.assets();
    stream.write_u32::<LE>(assets.len() as _).unwrap();
    (assets, asset_list.names(), asset_list.timestamps())
        .into_par_iter()
        .map(|(asset, name, timestamp)| {
            let mut out = ZlibEncoder::new(Vec::new(), Compression::new(unsafe { DEFLATE_LEVEL }));
            out.write_u32::<LE>(asset.is_some().into()).unwrap();
            if let Some(asset) = asset {
                write_string(name, &mut out).unwrap();
                if !exe {
                    out.write_f64::<LE>(*timestamp).unwrap();
                }
                asset.save(exe, &mut out).unwrap();
            }
            out.finish().unwrap()
        })
        .collect::<Vec<_>>()
        .into_iter()
        .for_each(|buf| {
            write_buffer(&buf, &mut stream).unwrap();
        });
    T::write_additional(stream).unwrap();
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
