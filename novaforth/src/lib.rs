/// The actual bytes of the NovaForth ROM, to be placed in memory starting at 0x400
pub const ROM: &'static [u8] = include_bytes!(concat!(env!("OUT_DIR"), "/4th.rom"));

/// The symbols (other than gensym'd ones) and their byte offsets from 0x400, as a JSON string
pub const SYMBOLS: &'static str = include_str!(concat!(env!("OUT_DIR"), "/4th.rom.sym"));

/// The "Prelude," words written in Novaforth itself that define part of the language.
pub const PRELUDE: &'static str = include_str!("prelude.f");
