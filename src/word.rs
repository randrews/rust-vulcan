// 128k, the amount of memory in a standard Vulcan machine
pub const MEM_SIZE: u32 = 128 * 1024;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Word(u32);

impl From<u32> for Word {
    fn from(a: u32) -> Self {
        Self(a & 0xffffff)
    }
}

impl From<Word> for u32 {
    fn from(word: Word) -> Self {
        word.0
    }
}

impl std::ops::Add<i32> for Word {
    type Output = Word;
    fn add(self, rhs: i32) -> Self::Output {
        Word::from((self.0 as i32).overflowing_add(rhs).0 as u32)
    }
}

impl std::ops::Sub<i32> for Word {
    type Output = Word;
    fn sub(self, rhs: i32) -> Self::Output {
        self + -rhs
    }
}

impl std::ops::Sub<Word> for Word {
    type Output = Word;
    fn sub(self, rhs: Word) -> Self::Output {
        Word(self.0 - rhs.0)
    }
}

impl std::ops::SubAssign<i32> for Word {
    fn sub_assign(&mut self, rhs: i32) {
        *self = *self - rhs;
    }
}

impl std::ops::AddAssign<i32> for Word {
    fn add_assign(&mut self, rhs: i32) {
        *self = *self + rhs;
    }
}

#[test]
fn test_address_truncation() {
    let a: Word = 0x11223344.into();
    assert_eq!(a, 0x00223344.into());
}

#[test]
fn test_address_overflows() {
    let a = Word::from(0xfffffa);
    assert_eq!(a + 10, Word(4));

    let b = Word::from(3);
    assert_eq!(b - 10, Word(0xfffff9));

    let mut c = Word::from(5);
    c += 3;
    assert_eq!(c, Word(8));

    let mut d = Word::from(5);
    d -= 3;
    assert_eq!(d, Word(2));
}
