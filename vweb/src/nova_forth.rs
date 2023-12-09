use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
pub struct NovaForth;

#[wasm_bindgen]
impl NovaForth {
    pub fn rom() -> Vec<u8> {
        Vec::from(*include_bytes!("../4th/4th.rom"))
    }

    pub fn symbols() -> String {
        include_str!("../4th/4th.rom.sym").into()
    }
    pub fn prelude() -> String { include_str!("../4th/prelude.f").into() }
}
