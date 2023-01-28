use wasm_bindgen::prelude::*;
use vcore::{CPU, Word};
use vcore::memory::{PeekPoke, PeekPokeExt};
use vasm_core::parse_error::AssembleError;
use serde::{ Deserialize, Serialize };

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

#[wasm_bindgen]
pub struct WasmCPU(CPU);

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
impl WasmCPU {
    #[wasm_bindgen(constructor)]
    pub fn new() -> WasmCPU {
        WasmCPU(CPU::new_random())
    }

    pub fn push_data(&mut self, data: i32) {
        self.0.push_data(data)
    }

    pub fn pop_data(&mut self) -> i32 {
        self.0.pop_data().into()
    }

    pub fn push_call(&mut self, data: i32) {
        self.0.push_call(data)
    }

    pub fn pop_call(&mut self) -> i32 {
        self.0.pop_call().into()
    }

    pub fn poke(&mut self, addr: u32, val: u8) {
        self.0.poke(addr.into(), val.into())
    }

    pub fn peek(&self, addr: u32) -> u8 {
        self.0.peek(addr.into()).into()
    }

    pub fn poke24(&mut self, addr: u32, val: i32) {
        self.0.poke24(Word::from(addr), Word::from(val))
    }

    pub fn peek24(&self, addr: u32) -> i32 {
        self.0.peek24(Word::from(addr)).into()
    }

    pub fn pc(&self) -> u32 {
        self.0.pc().into()
    }

    pub fn sp(&self) -> u32 {
        self.0.sp().into()
    }

    pub fn dp(&self) -> u32 {
        self.0.dp().into()
    }

    pub fn halted(&self) -> bool { self.0.halted() }

    pub fn get_stack(&self) -> Vec<i32> {
        self.0.get_stack().into_iter().map(i32::from).collect()
    }

    pub fn get_call(&self) -> Vec<i32> {
        self.0.get_call().into_iter().map(i32::from).collect()
    }

    pub fn set_pc(&mut self, val: u32) {
        self.0.set_pc(Word::from(val))
    }

    pub fn run(&mut self) {
        self.0.run_to_halt()
    }

    pub fn safe_run(&mut self, max_ticks: usize) {
        let mut current = 0;
        while !self.0.halted() && current < max_ticks {
            current += 1;
            self.tick()
        }
    }

    pub fn load(&mut self, rom: Vec<u8>) {
        for (i, b) in rom.iter().enumerate() {
            self.poke(0x400 + i as u32, *b)
        }

        self.set_pc(0x400)
    }

    pub fn reset(&mut self) {
        self.0.reset()
    }

    pub fn start(&mut self) {
        self.0.start()
    }

    pub fn tick(&mut self) {
        self.0.tick()
    }
}
