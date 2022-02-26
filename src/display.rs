use crate::memory::{PeekPoke, PeekPokeExt};
use crate::Word;
use std::convert::TryFrom;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
struct DisplayRegisters {
    mode: u8,
    screen: Word,
    palette: Word,
    font: Word,
    height: Word,
    width: Word,
    row_offset: Word,
    col_offset: Word,
}

impl Default for DisplayRegisters {
    fn default() -> Self {
        Self {
            mode: 0,
            screen: Word::from(0x10000),
            palette: Word::from(0x20000 - 0x100),
            font: Word::from(0x20000 - 0x100 - 0x2000),
            height: Word::from(60),
            width: Word::from(80),
            row_offset: Word::from(0),
            col_offset: Word::from(0),
        }
    }
}

fn read_display_registers<P: PeekPoke>(machine: &P, start: Word) -> DisplayRegisters {
    DisplayRegisters {
        mode: machine.peek8(start),
        screen: machine.peek24(start + 1),
        palette: machine.peek24(start + 4),
        font: machine.peek24(start + 7),
        height: machine.peek24(start + 10),
        width: machine.peek24(start + 13),
        row_offset: machine.peek24(start + 16),
        col_offset: machine.peek24(start + 19),
    }
}

fn init_display_registers<P: PeekPoke>(machine: &mut P, start: Word) {
    let dr = DisplayRegisters::default();
    machine.poke(start, dr.mode);
    machine.poke24(start + 1, dr.screen);
    machine.poke24(start + 4, dr.palette);
    machine.poke24(start + 7, dr.font);
    machine.poke24(start + 10, dr.height);
    machine.poke24(start + 13, dr.width);
    machine.poke24(start + 16, dr.row_offset);
    machine.poke24(start + 19, dr.col_offset);
}

fn init_font<P: PeekPoke>(machine: &mut P) {
    machine.poke_slice(DisplayRegisters::default().font, include_bytes!("font.rom"));
}

pub fn draw<P: PeekPoke>(machine: &P, frame: &mut [u8]) {
    let reg = read_display_registers(machine, 16.into());
    let (gfx, highres, paletted) = (reg.mode & 1 > 0, reg.mode & 2 > 0, reg.mode & 4 > 0);

    match (paletted, highres, gfx) {
        (true, true, true) => draw_paletted_high_gfx(machine, reg, frame),
        (false, true, true) => draw_direct_high_gfx(machine, reg, frame),
        (true, true, false) => draw_paletted_high_text(machine, reg, frame),
        (false, true, false) => draw_direct_high_text(machine, reg, frame),
        (false, false, false) => draw_direct_low_text(machine, reg, frame),
        (true, false, false) => draw_paletted_low_text(machine, reg, frame),
        (false, false, true) => draw_direct_low_gfx(machine, reg, frame),
        (true, false, true) => draw_paletted_low_gfx(machine, reg, frame),
    }
}

pub fn reset<P: PeekPoke>(machine: &mut P) {
    init_display_registers(machine, 16.into());
    init_font(machine);
    init_palette(machine);
}

fn init_palette<P: PeekPoke>(machine: &mut P) {
    let palette_addr = read_display_registers(machine, Word::from(16)).palette;
    machine.poke_slice(
        palette_addr,
        &[
            0x00, 0x05, 0x65, 0x11, 0xa8, 0x49, 0xeb, 0xff, 0xe1, 0xf4, 0xfc, 0x1c, 0x37, 0x8e,
            0xee, 0xfa,
        ],
    );
}

fn to_byte_address((x, y): (Word, Word), reg: DisplayRegisters) -> Word {
    let row_start = (y + reg.row_offset % reg.height) * reg.width + reg.screen;
    ((x + reg.col_offset) % reg.width) + row_start
}

fn draw_direct_high_gfx<P: PeekPoke>(machine: &P, reg: DisplayRegisters, frame: &mut [u8]) {
    for (i, pixel) in frame.chunks_exact_mut(4).enumerate() {
        let (display_row, display_col) = (i / 640, i % 640);
        let (vulcan_row, vulcan_col) = (Word::from(display_row >> 2), Word::from(display_col >> 2));

        let vb = machine.peek(to_byte_address((vulcan_col, vulcan_row), reg));
        paint_pixel(<&mut [u8; 4]>::try_from(pixel).unwrap(), vb)
    }
}

fn draw_paletted_high_gfx<P: PeekPoke>(machine: &P, reg: DisplayRegisters, frame: &mut [u8]) {
    for (i, pixel) in frame.chunks_exact_mut(4).enumerate() {
        let (display_row, display_col) = (i / 640, i % 640);
        let (vulcan_row, vulcan_col) = (Word::from(display_row >> 2), Word::from(display_col >> 2));

        let addr = to_byte_address((vulcan_col, vulcan_row), reg);
        let color_idx = machine.peek(addr);
        let color = machine.peek(reg.palette + color_idx);

        paint_pixel(<&mut [u8; 4]>::try_from(pixel).unwrap(), color)
    }
}

fn draw_paletted_high_text<P: PeekPoke>(machine: &P, reg: DisplayRegisters, frame: &mut [u8]) {
    for (i, pixel) in frame.chunks_exact_mut(4).enumerate() {
        let (display_row, display_col) = (i / 640, i % 640);
        let (vulcan_row, vulcan_col) = (Word::from(display_row >> 3), Word::from(display_col >> 3));

        let addr = to_byte_address((vulcan_col, vulcan_row), reg);
        let char_idx = machine.peek(addr) as u32;
        let (char_row, char_col) = (display_row % 8, display_col % 8);
        let char_byte = machine.peek(reg.font + (char_idx << 3) + char_row);

        let color_addr = addr + (reg.width * reg.height);
        let color_byte = machine.peek(color_addr);
        let (fg_color_idx, bg_color_idx) = (color_byte & 0xf, color_byte >> 4);

        let fg_color = machine.peek(reg.palette + fg_color_idx);
        let bg_color = machine.peek(reg.palette + bg_color_idx);

        draw_text_pixel(
            <&mut [u8; 4]>::try_from(pixel).unwrap(),
            char_col as u8,
            char_byte,
            fg_color,
            bg_color,
        )
    }
}

fn draw_text_pixel(pixel: &mut [u8; 4], char_col: u8, char_byte: u8, fg_color: u8, bg_color: u8) {
    if char_byte & (1 << (7 - char_col)) != 0 {
        paint_pixel(pixel, fg_color)
    } else {
        paint_pixel(pixel, bg_color)
    }
}

fn draw_direct_high_text<P: PeekPoke>(machine: &P, reg: DisplayRegisters, frame: &mut [u8]) {
    for (i, pixel) in frame.chunks_exact_mut(4).enumerate() {
        let (display_row, display_col) = (i / 640, i % 640);
        let (vulcan_row, vulcan_col) = (Word::from(display_row >> 3), Word::from(display_col >> 3));

        let addr = to_byte_address((vulcan_col, vulcan_row), reg);
        let char_idx = machine.peek(addr) as u32;
        let (char_row, char_col) = (display_row % 8, display_col % 8);
        let char_byte = machine.peek(reg.font + (char_idx << 3) + char_row);

        let color_addr = addr + (reg.width * reg.height);
        let color = machine.peek(color_addr);

        draw_text_pixel(
            <&mut [u8; 4]>::try_from(pixel).unwrap(),
            char_col as u8,
            char_byte,
            color,
            0,
        )
    }
}

fn draw_direct_low_text<P: PeekPoke>(machine: &P, reg: DisplayRegisters, frame: &mut [u8]) {
    for (i, pixel) in frame.chunks_exact_mut(4).enumerate() {
        let (display_row, display_col) = (i / 640, i % 640);
        let (vulcan_row, vulcan_col) = (Word::from(display_row >> 4), Word::from(display_col >> 4));

        let addr = to_byte_address((vulcan_col, vulcan_row), reg);
        let char_idx = machine.peek(addr) as u32;
        let (char_row, char_col) = ((display_row / 2) % 8, (display_col / 2) % 8);
        let char_byte = machine.peek(reg.font + (char_idx << 3) + char_row);

        let color_addr = addr + (reg.width * reg.height);
        let color = machine.peek(color_addr);

        draw_text_pixel(
            <&mut [u8; 4]>::try_from(pixel).unwrap(),
            char_col as u8,
            char_byte,
            color,
            0,
        )
    }
}

fn draw_paletted_low_text<P: PeekPoke>(machine: &P, reg: DisplayRegisters, frame: &mut [u8]) {
    for (i, pixel) in frame.chunks_exact_mut(4).enumerate() {
        let (display_row, display_col) = (i / 640, i % 640);
        let (vulcan_row, vulcan_col) = (Word::from(display_row >> 4), Word::from(display_col >> 4));

        let addr = to_byte_address((vulcan_col, vulcan_row), reg);
        let char_idx = machine.peek(addr) as u32;
        let (char_row, char_col) = ((display_row >> 1) % 8, (display_col >> 1) % 8);
        let char_byte = machine.peek(reg.font + (char_idx << 3) + char_row);

        let color_addr = addr + (reg.width * reg.height);
        let color_byte = machine.peek(Word::from(color_addr));
        let (fg_color_idx, bg_color_idx) = (color_byte & 0xf, color_byte >> 4);

        let fg_color = machine.peek(reg.palette + fg_color_idx);
        let bg_color = machine.peek(reg.palette + bg_color_idx);

        draw_text_pixel(
            <&mut [u8; 4]>::try_from(pixel).unwrap(),
            char_col as u8,
            char_byte,
            fg_color,
            bg_color,
        )
    }
}

fn draw_direct_low_gfx<P: PeekPoke>(machine: &P, reg: DisplayRegisters, frame: &mut [u8]) {
    for (i, pixel) in frame.chunks_exact_mut(4).enumerate() {
        let (display_row, display_col) = (i / 640, i % 640);

        if display_row >= (240 - 64 * 3)
            && display_row < (240 + 64 * 3)
            && display_col >= (320 - 64 * 3)
            && display_col < (320 + 64 * 3)
        {
            let (vulcan_row, vulcan_col) = (
                Word::from((display_row - (240 - 64 * 3)) / 3),
                Word::from((display_col - (320 - 64 * 3)) / 3),
            );

            let vb = machine.peek(to_byte_address((vulcan_col, vulcan_row), reg));
            paint_pixel(<&mut [u8; 4]>::try_from(pixel).unwrap(), vb)
        } else {
            paint_pixel(<&mut [u8; 4]>::try_from(pixel).unwrap(), 0)
        }
    }
}

fn draw_paletted_low_gfx<P: PeekPoke>(machine: &P, reg: DisplayRegisters, frame: &mut [u8]) {
    for (i, pixel) in frame.chunks_exact_mut(4).enumerate() {
        let (display_row, display_col) = (i / 640, i % 640);

        if display_row >= (240 - 64 * 3)
            && display_row < (240 + 64 * 3)
            && display_col >= (320 - 64 * 3)
            && display_col < (320 + 64 * 3)
        {
            let (vulcan_row, vulcan_col) = (
                Word::from((display_row - (240 - 64 * 3)) / 3),
                Word::from((display_col - (320 - 64 * 3)) / 3),
            );

            let color_idx =
                machine.peek(to_byte_address((vulcan_col, vulcan_row), reg));
            let vb = machine.peek(reg.palette + color_idx);
            paint_pixel(<&mut [u8; 4]>::try_from(pixel).unwrap(), vb);
        } else {
            paint_pixel(<&mut [u8; 4]>::try_from(pixel).unwrap(), 0);
        }
    }
}

fn paint_pixel(pixel: &mut [u8; 4], vb: u8) {
    let (red, green, blue) = (vb >> 5, (vb >> 2) & 7, vb & 3);

    fn scale(b: u8) -> u8 {
        let tmp = (b << 3) | b;
        (tmp << 2) | (b & 3)
    }

    let blue_shifted2 = (blue << 2) | blue;
    let blue_shifted = (blue_shifted2 << 4) | blue_shifted2;

    pixel[0] = scale(red);
    pixel[1] = scale(green);
    pixel[2] = blue_shifted;
    pixel[3] = 0xff;
}

#[test]
fn test_paint_pixel() {
    let mut p: [u8; 4] = [0, 0, 0, 0];

    paint_pixel(&mut p, 0b11100000);
    assert_eq!(p, [0xff, 0, 0, 255]);

    paint_pixel(&mut p, 0b00011100);
    assert_eq!(p, [0, 0xff, 0, 255]);

    paint_pixel(&mut p, 0b00000011);
    assert_eq!(p, [0, 0, 0xff, 255]);

    paint_pixel(&mut p, 0xff);
    assert_eq!(p, [0xff, 0xff, 0xff, 255]);

    paint_pixel(&mut p, 0);
    assert_eq!(p, [0, 0, 0, 255]);
}
