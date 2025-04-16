use crate::{UStr, asset::Font, delphi::TBitmap};
use std::{ptr, slice};

#[repr(C)]
#[derive(Default)]
#[allow(non_snake_case)]
struct TEXTMETRICSW {
    tmHeight: u32,
    tmAscent: u32,
    tmDescent: u32,
    tmInternalLeading: u32,
    tmExternalLeading: u32,
    tmAveCharWidth: u32,
    tmMaxCharWidth: u32,
    tmWeight: u32,
    tmOverhang: u32,
    tmDigitizedAspectX: u32,
    tmDigitizedAspectY: u32,
    tmFirstChar: u16,
    tmLastChar: u16,
    tmDefaultChar: u16,
    tmBreakChar: u16,
    tmItalic: u8,
    tmUnderlined: u8,
    tmStruckOut: u8,
    tmPitchAndFamily: u8,
    tmCharSet: u8,
}
#[repr(C)]
#[derive(Default)]
#[allow(non_snake_case)]
struct GLYPHMETRICS {
    gmBlackBoxX: u32,
    gmBlackBoxY: u32,
    gmptGlyphOrigin: [i32; 2],
    gmCellIncX: u32,
    gmCellIncY: u32,
}
#[repr(C)]
#[derive(Default)]
#[allow(non_snake_case)]
struct ABC {
    abcA: i32,
    abcB: u32,
    abcC: i32,
}

#[link(name = "gdi32")]
unsafe extern "system" {
    fn GetTextMetricsW(hdc: usize, text_metrics: &mut TEXTMETRICSW) -> u32;
    fn GetGlyphOutlineW(
        hdc: usize,
        uChar: u32,
        fuFormat: u32,
        lpgm: &mut GLYPHMETRICS,
        cjBuffer: u32,
        pvBuffer: *mut u8,
        lpmat2: &[u16; 8],
    ) -> u32;
    fn GetCharABCWidthsW(hdc: usize, wFirst: u32, wLast: u32, lpABC: &mut ABC) -> u32;
}

#[link(name = "kernel32")]
unsafe extern "system" {
    fn GetACP() -> u32;
    fn MultiByteToWideChar(
        CodePage: u32,
        dwFlags: u32,
        lpMultiByteStr: *const u8,
        cbMultiByte: i32,
        lpWideCharStr: *mut u16,
        cchWideChar: i32,
    ) -> i32;
}

fn charset_to_codepage(charset: u32) -> u32 {
    match charset {
        0xa1 => 0x4e5,
        0x80 => 0x3a4,
        0x00 => 0x4e4,
        0x81 => 0x3b5,
        0x82 => 0x551,
        0x86 => 0x3a8,
        0x88 => 0x3b6,
        0xba => 0x4e9,
        0xa2 => 0x4e6,
        0xa3 => 0x4ea,
        0xb1 => 0x4e7,
        0xb2 => 0x4e8,
        0xcc => 0x4e3,
        0xde => 0x36a,
        0xee => 0x4e2,
        _ => unsafe { GetACP() },
    }
}

impl Font {
    fn calculate_bitmap_size(&mut self, chars: &[u32]) {
        let hspacing = 2;
        let vspacing = 2;
        let mut width = 0x800;
        let mut height = 0x800;
        let mut shrink_h = false;
        loop {
            let mut x = 0;
            let mut y = 0;
            let mut row_height = 0;
            for &c in chars {
                if x + self.s_w[c as usize] >= width {
                    x = 0;
                    y += row_height + vspacing;
                    row_height = 0;
                }
                row_height = row_height.max(self.s_h[c as usize]);
                x += self.s_w[c as usize] + hspacing;
            }
            if y + row_height > height {
                break
            }
            self.s_bw = width;
            self.s_bh = height;
            shrink_h = !shrink_h;
            if shrink_h {
                height >>= 1;
            } else {
                width >>= 1;
            }
        }
    }

    #[unsafe(naked)]
    unsafe extern "fastcall" fn copy_glyph_onto_atlas(
        &mut self,
        format: u32,
        y: u32,
        glyph_buf: *mut u8,
        text_metrics: *const TEXTMETRICSW,
        glyph_metrics: *const GLYPHMETRICS,
        c: u32,
        x: u32,
    ) {
        std::arch::naked_asm!(
            "mov eax, ecx",
            "pop ecx",
            "xchg ecx, [esp]",
            "push 0x5a7b54", // copy glyph onto font atlas
            "ret",
        );
    }

    pub fn render(&mut self) {
        unsafe {
            let mut bitmap = TBitmap::new();
            bitmap.SetPixelFormat(6);
            let canvas = bitmap.GetCanvas();
            let font = self.make_tfont();
            canvas.FFont.Assign(&font);
            drop(font);
            let hdc = canvas.GetHandle();
            let matrix = [0u16, 1, 0, 0, 0, 0, 0, 1];
            let mut text_metrics = std::mem::zeroed();
            if GetTextMetricsW(hdc, &mut text_metrics) == 0 {
                return
            }
            let codepage = charset_to_codepage(self.charset);
            self.s_bw = 0;
            self.s_bh = 0;
            let vector_font = text_metrics.tmPitchAndFamily & 0x2 != 0;
            let format = [1, 4, 5, 6].get(self.aa_level as usize).copied().unwrap_or(1);
            let mut max_buffer_size = 0;
            let mut glyph_metrics = std::mem::zeroed();
            // calculate character sizes and required buffer size
            let w_height = canvas.TextHeight(&UStr::new("W"));
            for c in self.range_start..=self.range_end {
                let c = c as usize;
                self.s_chr[c] = if c == 0 {
                    0
                } else {
                    let mut wc = 1;
                    let cc = c as u8;
                    MultiByteToWideChar(codepage, 0, &cc, 1, &mut wc, 1);
                    wc.into()
                };
                if vector_font {
                    // vector font, use GetGlyphOutlineW to get character dimensions
                    let out =
                        GetGlyphOutlineW(hdc, self.s_chr[c], format, &mut glyph_metrics, 0, ptr::null_mut(), &matrix);
                    if out != u32::MAX {
                        max_buffer_size = max_buffer_size.max(out);
                        self.s_w[c] = glyph_metrics.gmBlackBoxX;
                        self.s_h[c] = (text_metrics.tmAscent as i32 + glyph_metrics.gmBlackBoxY as i32
                            - glyph_metrics.gmptGlyphOrigin[1]) as u32;
                        self.s_shift[c] =
                            (glyph_metrics.gmCellIncX as i32 - glyph_metrics.gmptGlyphOrigin[0].min(0)) as u32;
                        self.s_offset[c] = glyph_metrics.gmptGlyphOrigin[0] as u32;
                    } else {
                        self.s_w[c] = 0;
                        self.s_h[c] = 0;
                        self.s_shift[c] = 0;
                        self.s_offset[c] = 0;
                    }
                } else {
                    // bitmap font, use ABC
                    let mut abc = Default::default();
                    if GetCharABCWidthsW(hdc, self.s_chr[c], self.s_chr[c], &mut abc) != 0 {
                        self.s_w[c] = abc.abcB + 1;
                        self.s_shift[c] = (abc.abcA + abc.abcB as i32 + abc.abcC) as u32;
                        self.s_offset[c] = abc.abcA as _;
                    } else {
                        let width = canvas.TextWidth(&UStr::from_char(c as u16));
                        self.s_shift[c] = width;
                        self.s_w[c] = width + 1;
                        if self.italic {
                            self.s_w[c] += w_height / 2 + 1;
                        }
                    }
                    self.s_h[c] = w_height;
                }
                self.s_x[c] = 0;
                self.s_y[c] = 0;
            }
            if self.s_h[0x20] == 0 {
                self.s_h[0x20] = text_metrics.tmHeight;
            }
            if self.s_w[0x20] == 0 {
                let out = GetGlyphOutlineW(hdc, 0x20, format, &mut glyph_metrics, 0, ptr::null_mut(), &matrix);
                self.s_w[0x20] = if out != u32::MAX && glyph_metrics.gmCellIncX != 0 {
                    glyph_metrics.gmCellIncX
                } else {
                    text_metrics.tmAveCharWidth.min(text_metrics.tmMaxCharWidth)
                };
                self.s_shift[0x20] = self.s_w[0x20];
            }
            // setup
            let mut chars = (self.range_start..=self.range_end).collect::<Vec<_>>();
            chars.sort_unstable_by_key(|&c| -((self.s_w[c as usize] * self.s_h[c as usize]) as i32));
            self.calculate_bitmap_size(&chars);
            if !vector_font {
                bitmap.SetSize(self.s_bw, self.s_bh);
                let canvas = bitmap.GetCanvas();
                canvas.FBrush.SetColor(0);
                let mut rect = [0; 4];
                canvas.GetClipRect(&mut rect);
                canvas.FillRect(&rect);
                canvas.FFont.SetColor(0xffffff);
                canvas.FBrush.SetStyle(1);
            }
            let canvas = bitmap.GetCanvas();
            self.s_bytes.alloc((self.s_bw * self.s_bh) as usize);
            self.s_bytes.fill(0); // is this really necessary?
            let mut glyph_buf = vec![0; max_buffer_size as usize];
            // draw the characters
            let mut x = 0;
            let mut y = 0;
            let mut row_height = 0;
            let hspacing = 2;
            let vspacing = 2;
            for c in chars {
                let c = c as usize;
                if x + self.s_w[c] >= self.s_bw {
                    x = 0;
                    y += row_height + vspacing;
                    row_height = 0;
                }
                row_height = row_height.max(self.s_h[c]);
                self.s_x[c] = x;
                self.s_y[c] = y;
                if c != 0x20 {
                    if vector_font {
                        // vector font, use GetGlyphOutlineW
                        glyph_buf.fill(0);
                        GetGlyphOutlineW(
                            hdc,
                            self.s_chr[c],
                            format,
                            &mut glyph_metrics,
                            max_buffer_size,
                            glyph_buf.as_mut_ptr(),
                            &matrix,
                        );
                        self.copy_glyph_onto_atlas(
                            format,
                            y,
                            glyph_buf.as_mut_ptr(),
                            &text_metrics,
                            &glyph_metrics,
                            c as u32,
                            x,
                        );
                    } else {
                        // bitmap font, draw onto bitmap
                        let s = UStr::from_char(c as u16);
                        let mut abc = Default::default();
                        if GetCharABCWidthsW(hdc, self.s_chr[c], self.s_chr[c], &mut abc) != 0 {
                            canvas.TextOut(self.s_x[c] as i32 - abc.abcA, self.s_y[c], &s);
                        } else {
                            canvas.TextOut(self.s_x[c] as i32, self.s_y[c], &s);
                        }
                    }
                }
                x += self.s_w[c as usize] + hspacing;
            }
            if !vector_font {
                // download bitmap
                for (y, dst_row) in self.s_bytes.chunks_mut(self.s_bw as usize).enumerate() {
                    let scanline = bitmap.GetScanline(y as u32);
                    for (dst, src) in
                        dst_row.iter_mut().zip(slice::from_raw_parts(scanline, self.s_bw as usize * 3).chunks(3))
                    {
                        *dst = (src.iter().copied().map(u16::from).sum::<u16>() / 3u16) as u8;
                    }
                }
            }
        }
    }
}
