mod display;
mod wasm_cpu;
mod nova_forth;
mod error;

use wasm_bindgen::prelude::*;
use forge_core::compiler::build_boot;
use crate::error::JsVasmError;

#[wasm_bindgen]
pub fn assemble_snippet(snippet: String) -> Result<Vec<u8>, JsVasmError> {
    vasm_core::assemble_snippet(snippet.lines().map(String::from)).map_err(|err| JsVasmError::from(err))
}

#[wasm_bindgen]
pub fn source_map(snippet: String) -> JsValue {
    let source_map = vasm_core::snippet_source_map(snippet.lines().map(String::from)).map_err(|err| JsVasmError::from(err));
    serde_wasm_bindgen::to_value(&source_map).unwrap()
}

#[wasm_bindgen]
pub fn compile_forge(src: String) -> Result<String, String> {
    build_boot(src.as_str()).map(|s| s.join("\n")).map_err(|e| format!("{}", e))
}