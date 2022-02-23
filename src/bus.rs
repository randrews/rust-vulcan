use crate::memory::PeekPoke;
use crate::word::Word;

pub trait Device {
    fn tick(&mut self);
    fn reset(&mut self);
}

pub struct Bus<A, B> {
    start: Word,
    end: Word,
    device: A,
    rest: B,
}

impl<A, B> Bus<A, B> {
    fn new(start: u32, end: u32, device: A, rest: B) -> Self {
        Self {
            start: start.into(),
            end: end.into(),
            device,
            rest,
        }
    }

    fn at(addr: u32, device: A, rest: B) -> Self {
        Self::new(addr, addr, device, rest)
    }
}

impl<A: PeekPoke, B: PeekPoke> PeekPoke for Bus<A, B> {
    fn peek(&self, addr: Word) -> u8 {
        if addr >= self.start && addr <= self.end {
            self.device.peek(addr - self.start)
        } else {
            self.rest.peek(addr)
        }
    }

    fn poke(&mut self, addr: Word, val: u8) {
        if addr >= self.start && addr <= self.end {
            self.device.poke(addr - self.start, val)
        } else {
            self.rest.poke(addr, val)
        }
    }
}

impl<A: Device, B: Device> Device for Bus<A, B> {
    fn tick(&mut self) {
        self.device.tick();
        self.rest.tick();
    }

    fn reset(&mut self) {
        self.device.reset();
        self.rest.reset();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::memory::PeekPokeExt;

    struct TestDevice(i32);
    impl Device for TestDevice {
        fn tick(&mut self) {
            self.0 += 1
        }
        fn reset(&mut self) {
            self.0 = 10
        }
    }

    struct ArrayDevice([u8; 10]);
    impl PeekPoke for ArrayDevice {
        fn peek(&self, addr: Word) -> u8 {
            self.0[usize::from(addr)]
        }
        fn poke(&mut self, addr: Word, val: u8) {
            self.0[usize::from(addr)] = val
        }
    }

    #[test]
    fn test_tick() {
        let device1 = TestDevice(5);
        let device2 = TestDevice(6);
        let device3 = TestDevice(7);
        let mut bus = Bus::at(5, device1, Bus::at(6, device2, device3));

        for _ in 0..5 {
            bus.tick()
        }
        assert_eq!(bus.device.0, 10);
        assert_eq!(bus.rest.device.0, 11);
        assert_eq!(bus.rest.rest.0, 12);
    }

    #[test]
    fn test_reset() {
        let device1 = TestDevice(5);
        let device2 = TestDevice(6);
        let mut bus = Bus::at(5, device1, device2);

        bus.reset();
        assert_eq!(bus.device.0, 10);
        assert_eq!(bus.rest.0, 10);
    }

    #[test]
    fn test_poke_peek() {
        let mut bus = Bus::new(5, 10, ArrayDevice([0u8; 10]), ArrayDevice([0u8; 10]));
        bus.poke8(2, 2); // Goes into the 2nd device
        bus.poke8(6, 6); // Goes into the first device...
        assert_eq!(bus.device.0[1], 6); // At index 1
        assert_eq!(bus.rest.0[2], 2); // Second device gets the other write

        // Neither device sees the other's write:
        assert_eq!(bus.device.0[2], 0);
        assert_eq!(bus.rest.0[1], 0);

        assert_eq!(bus.peek8(2), 2); // Reading from the first device
        assert_eq!(bus.peek8(6), 6); // And the second
    }
}
