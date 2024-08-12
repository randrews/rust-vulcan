use novaforth::{PRELUDE, ROM, SYMBOLS};
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub struct NovaForth;

#[wasm_bindgen]
impl NovaForth {
    pub fn rom() -> Vec<u8> {
        Vec::from(ROM)
    }
    pub fn symbols() -> String {
        String::from(SYMBOLS)
    }
    pub fn prelude() -> String { String::from(PRELUDE) }
}
