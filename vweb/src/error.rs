use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::wasm_bindgen;
use vasm_core::parse_error::AssembleError;

#[wasm_bindgen(inspectable, getter_with_clone)]
#[derive(Serialize, Deserialize)]
pub struct JsVasmError {
    pub line_number: usize,
    pub message: String
}

impl From<AssembleError> for JsVasmError {
    fn from(value: AssembleError) -> Self {
        let line = match value.clone() {
            AssembleError::ParseError(loc, _) => loc.line_num,
            AssembleError::EquResolveError(loc, _, _) => loc.line_num,
            AssembleError::EquDuplicateError(loc, _) => loc.line_num,
            AssembleError::OrgResolveError(loc, _) => loc.line_num,
            AssembleError::ArgError(loc, _) => loc.line_num,
            AssembleError::NoCode => 0,
            AssembleError::IncludeError(loc, _) => loc.line_num,
            AssembleError::FileError(_) => 0,
            AssembleError::MacroError(loc) => loc.line_num,
        };

        return JsVasmError {
            line_number: line,
            message: value.message()
        }
    }
}
