use wasm_bindgen::{Clamped, JsValue};
use wasm_bindgen::prelude::wasm_bindgen;
use web_sys::{CanvasRenderingContext2d, ImageData};
use vgfx::display;
use crate::wasm_cpu::WasmCPU;

// This has to contain a Box or else it'll use up all the wasm_bindgen stack space
#[wasm_bindgen]
pub struct Display(Box<[u8; 640 * 480 * 4]>);

#[wasm_bindgen]
impl Display {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Display {
        Display([0u8; 640 * 480 * 4].into())
    }

    pub fn draw(&mut self, cpu: &WasmCPU, ctx: &CanvasRenderingContext2d) -> Result<(), JsValue> {
        display::draw(cpu.cpu(), self.0.as_mut());
        let image_data = ImageData::new_with_u8_clamped_array(Clamped(self.0.as_mut()), 640)?;
        ctx.put_image_data(&image_data, 0.0, 0.0)
    }
}
