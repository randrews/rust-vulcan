use crate::word::Word;
use crate::word::MEM_SIZE;
use rand::Rng;

pub struct Memory([u8; MEM_SIZE as usize]);

impl Default for Memory {
    fn default() -> Self {
        Self([0u8; MEM_SIZE as usize])
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

impl Memory {
    /// Create a memory containing the given contents (probably code) starting at `0x400` (the
    /// starting address for code). The rest of the memory is zeroes.
    /// ```
    /// # use crate::vcore::memory::PeekPokeExt;
    /// # use crate::vcore::Word;
    /// # use crate::vcore::Memory;
    /// let mem = Memory::with_program(vec![0x02, 0x12, 0x34]);
    /// assert_eq!(mem.peek8(Word::from(0x400)), 0x02);
    /// assert_eq!(mem.peek8(Word::from(0x401)), 0x12); // etc
    /// ```
    pub fn with_program<T: IntoIterator<Item = u8>>(code: T) -> Memory {
        let mut m = Memory::default();
        for (addr, byte) in code.into_iter().enumerate() {
            m.poke8(addr + 0x400, byte)
        }
        m
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
    /// Read a byte from a given address.
    fn peek(&self, addr: Word) -> u8;

    /// Write a byte to a given address.
    fn poke(&mut self, addr: Word, val: u8);

    /// Read a slice, starting from a given address.
    fn peek_slice(&self, addr: Word, buffer: &mut [u8]) {
        for (i, byte) in buffer.iter_mut().enumerate() {
            *byte = self.peek(addr + i);
        }
    }

    /// Write a slice, starting at a given address.
    fn poke_slice(&mut self, addr: Word, buffer: &[u8]) {
        for (i, byte) in buffer.iter().enumerate() {
            self.poke(addr + i, *byte);
        }
    }
}

pub trait PeekPokeExt {
    fn peek8<A: Into<Word>>(&self, addr: A) -> u8;
    fn poke8<A: Into<Word>, V: Into<u8>>(&mut self, addr: A, val: V);

    fn peek24<A: Into<Word>>(&self, addr: A) -> Word;
    fn poke24<A: Into<Word>, V: Into<Word>>(&mut self, addr: A, val: V);
}

impl<T: PeekPoke> PeekPokeExt for T {
    fn peek8<A: Into<Word>>(&self, addr: A) -> u8 {
        self.peek(addr.into())
    }

    fn poke8<A: Into<Word>, V: Into<u8>>(&mut self, addr: A, val: V) {
        self.poke(addr.into(), val.into());
    }

    fn peek24<A: Into<Word>>(&self, addr: A) -> Word {
        let mut bytes = [0u8; 3];
        self.peek_slice(addr.into(), &mut bytes);
        bytes.into()
    }

    fn poke24<A: Into<Word>, V: Into<Word>>(&mut self, addr: A, val: V) {
        let addr = addr.into();
        let val = val.into();
        self.poke_slice(addr, &val.to_bytes());
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
        assert_eq!(mem.peek24(35), 0);
        mem.poke24(35, 45);
        assert_eq!(mem.peek24(35), 45);
        assert_eq!(mem.peek24(36), 0);
    }

    #[test]
    fn test_mem_word_fns() {
        let mut mem = Memory::default();
        mem.poke24(10, 0x123456);
        assert_eq!(mem.peek8(10), 0x56);
        assert_eq!(mem.peek8(11), 0x34);
        assert_eq!(mem.peek8(12), 0x12);
        assert_eq!(mem.peek24(10), 0x123456);
        assert_eq!(mem.peek24(11), 0x001234);
    }
}
