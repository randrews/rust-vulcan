use wasm_bindgen::prelude::wasm_bindgen;
use vcore::{CPU, Word};
use vcore::memory::{PeekPoke, PeekPokeExt};
use vgfx::display;

#[wasm_bindgen]
pub struct WasmCPU(CPU);

#[wasm_bindgen]
impl WasmCPU {
    #[wasm_bindgen(constructor)]
    pub fn new() -> WasmCPU {
        let mut cpu = CPU::new_random();
        display::init_gfx(&mut cpu);
        WasmCPU(cpu)
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

impl WasmCPU {
    pub fn cpu(&self) -> &CPU {
        &self.0
    }
}