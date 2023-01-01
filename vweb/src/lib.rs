use wasm_bindgen::prelude::*;
use vcore::{CPU, Word};
use vcore::memory::{PeekPoke, PeekPokeExt};

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

    pub fn set_pc(&mut self, val: u32) {
        self.0.set_pc(Word::from(val))
    }

    pub fn run(&mut self) {
        self.0.run_to_halt()
    }

    pub fn load(&mut self, rom: Vec<u8>) {
        for (i, b) in rom.iter().enumerate() {
            self.poke(0x400 + i as u32, *b)
        }

        self.set_pc(0x400)
    }
}
