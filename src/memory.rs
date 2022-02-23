use crate::word::Word;
use crate::word::MEM_SIZE;
use rand::Rng;

pub struct Memory([u8; MEM_SIZE as usize]);

impl Default for Memory {
    fn default() -> Self {
        Self([0u8; MEM_SIZE as usize])
    }
}

impl From<Word> for usize {
    fn from(w: Word) -> Self {
        let w: u32 = w.into();
        (w & (MEM_SIZE - 1)) as usize
    }
}

impl<R: Rng> From<R> for Memory {
    fn from(mut rng: R) -> Self {
        let mut mem = Memory::default();
        for i in 0..(MEM_SIZE - 1) {
            mem.0[i as usize] = rng.gen()
        }
        mem
    }
}

impl std::ops::Index<Word> for Memory {
    type Output = u8;
    fn index(&self, index: Word) -> &Self::Output {
        &self.0[usize::from(index)]
    }
}

impl std::ops::IndexMut<Word> for Memory {
    fn index_mut(&mut self, index: Word) -> &mut Self::Output {
        &mut self.0[usize::from(index)]
    }
}

pub trait PeekPoke {
    fn peek(&self, addr: Word) -> u8;
    fn poke(&mut self, addr: Word, val: u8);

    fn peek24(&self, addr: Word) -> u32 {
        (self.peek(addr) as u32)
            | ((self.peek(addr + 1) as u32) << 8)
            | ((self.peek(addr + 2) as u32) << 16)
    }

    fn poke24(&mut self, addr: Word, val: u32) {
        self.poke(addr, val as u8);
        self.poke(addr + 1, (val >> 8) as u8);
        self.poke(addr + 2, (val >> 16) as u8);
    }

    fn peek_u32(&self, addr: u32) -> u8 {
        self.peek(addr.into())
    }
    fn poke_u32(&mut self, addr: u32, val: u8) {
        self.poke(addr.into(), val)
    }
    fn peek24_u32(&mut self, addr: u32) -> u32 {
        self.peek24(addr.into())
    }
    fn poke24_u32(&mut self, addr: u32, val: u32) {
        self.poke24(addr.into(), val)
    }
}

impl PeekPoke for Memory {
    fn peek(&self, addr: Word) -> u8 {
        self[addr]
    }
    fn poke(&mut self, addr: Word, val: u8) {
        self[addr] = val;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mem_peek_poke() {
        let mut mem = Memory::default();
        assert_eq!(mem.peek_u32(35), 0);
        mem.poke_u32(35, 45);
        assert_eq!(mem.peek_u32(35), 45);
        assert_eq!(mem.peek_u32(36), 0);
    }

    #[test]
    fn test_mem_word_fns() {
        let mut mem = Memory::default();
        mem.poke24(10.into(), 0x123456);
        assert_eq!(mem.peek_u32(10), 0x56);
        assert_eq!(mem.peek_u32(11), 0x34);
        assert_eq!(mem.peek_u32(12), 0x12);
        assert_eq!(mem.peek24(10.into()), 0x123456);
        assert_eq!(mem.peek24(11.into()), 0x001234);
    }

    #[test]
    fn test_addressing_arrays() {
        let a: usize = Word::from(0xffffff).into();
        assert_eq!(a, 0x01ffff as usize);
    }
}
