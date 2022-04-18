use std::fmt::{Display, Formatter};

// 128k, the amount of memory in a standard Vulcan machine
pub const MEM_SIZE: u32 = 128 * 1024;

#[derive(Debug, Copy, Clone, Eq)]
pub struct Word(u32);

impl Word {
    /// Create a `Word` from three bytes.
    ///
    /// # Example
    ///
    /// ```
    /// use vcore::word::Word;
    ///
    /// assert_eq!(Word::from_bytes([0x01, 0x02, 0x03]), 0x030201);
    /// ```
    pub fn from_bytes(bytes: [u8; 3]) -> Self {
        let [a, b, c] = bytes;
        Self(u32::from_le_bytes([a, b, c, 0]))
    }

    /// Convert a `Word` into three bytes.
    ///
    /// # Example
    ///
    /// ```
    /// use vcore::word::Word;
    ///
    /// assert_eq!(Word::from(0x010203).to_bytes(), [0x03, 0x02, 0x01]);
    /// ```
    pub fn to_bytes(self) -> [u8; 3] {
        let [a, b, c, _] = self.0.to_le_bytes();
        [a, b, c]
    }
}

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

impl Display for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", i32::from(*self))
    }
}
#[test]
fn to_from_u32() {
    assert_eq!(u32::from(Word::from(0x123456u32)), 0x123456u32);
    assert_eq!(u32::from(Word::from(0x12345678u32)), 0x345678u32);
}

impl From<Word> for i32 {
    fn from(word: Word) -> Self {
        if word.0 & 0x800000 != 0 {
            -(((word.0 ^ 0xffffff) + 1) as i32)
        } else {
            word.0 as i32
        }
    }
}
impl From<i32> for Word {
    fn from(value: i32) -> Self {
        (value as u32).into()
    }
}

#[test]
fn to_from_i32() {
    assert_eq!(i32::from(Word::from(0x123456i32)), 0x123456i32);
    assert_eq!(i32::from(Word::from(-555i32)), -555i32);
}

// Perform various conversions using the conversions above
macro_rules! convert_via {
    ($big:ty => $little:ty) => {
        impl From<$big> for Word {
            fn from(value: $big) -> Self {
                (value as $little).into()
            }
        }
        impl From<Word> for $big {
            fn from(value: Word) -> Self {
                <$little>::from(value) as $big
            }
        }
    };
}
convert_via!(u8 => u32);
convert_via!(i8 => i32);

#[test]
fn to_from_u8() {
    assert_eq!(u8::from(Word::from(0x7fu8)), 0x7fu8);
    assert_eq!(u8::from(Word::from_bytes([1, 2, 3])), 1u8);
}

#[test]
fn to_from_i8() {
    assert_eq!(i8::from(Word::from(-1i8)), -1i8);
    assert_eq!(i8::from(Word::from_bytes([1, 2, 3])), 1i8);
    assert_eq!(i8::from(Word::from_bytes([0xff, 0, 0])), -1i8);
}

convert_via!(u16 => u32);
convert_via!(i16 => i32);

convert_via!(u64 => u32);
convert_via!(i64 => i32);

convert_via!(usize => u32);
convert_via!(isize => i32);

impl From<bool> for Word {
    fn from(value: bool) -> Self {
        if value {
            Word::from(1)
        } else {
            Word::from(0)
        }
    }
}

impl From<Word> for bool {
    fn from(word: Word) -> Self {
        word.0 != 0
    }
}

#[test]
fn to_from_bool() {
    assert!(!bool::from(Word::from(0)));
    assert!(bool::from(Word::from(1)));
    assert!(bool::from(Word::from(0x123456)));

    assert_eq!(Word::from(false), Word::from(0));
    assert_eq!(Word::from(true), Word::from(1));
}

impl From<[u8; 3]> for Word {
    fn from(value: [u8; 3]) -> Self {
        Word::from_bytes(value)
    }
}
impl From<Word> for [u8; 3] {
    fn from(value: Word) -> Self {
        value.to_bytes()
    }
}

#[test]
fn to_from_u8_3() {
    assert_eq!(Word::from([0, 0, 0]), Word::from(0));
    assert_eq!(Word::from([1, 0, 0]), Word::from(1));
    assert_eq!(Word::from([0xff, 0xff, 0xff]), Word::from(0xffffff));

    assert_eq!(<[u8; 3]>::from(Word::from(0)), [0, 0, 0]);
    assert_eq!(<[u8; 3]>::from(Word::from(1)), [1, 0, 0]);
    assert_eq!(<[u8; 3]>::from(Word::from(0xffffff)), [0xff, 0xff, 0xff]);
}

// Implement negation via i32
impl std::ops::Neg for Word {
    type Output = Word;

    fn neg(self) -> Self::Output {
        Self::from(-i32::from(self))
    }
}

macro_rules! ops {
    // Implement operations for $target by converting both Word and $target to $target
    ($target:ty) => {
        ops!($target, $target);
    };

    // Implement operations for $target by converting both Word and $target to $intermediate
    ($target:ty, $intermediate:ty) => {
        impl std::ops::Add<$target> for Word {
            type Output = Word;
            fn add(self, rhs: $target) -> Self::Output {
                <$intermediate>::from(self)
                    .overflowing_add(<$intermediate>::from(rhs))
                    .0
                    .into()
            }
        }

        impl std::ops::Sub<$target> for Word {
            type Output = Word;
            fn sub(self, rhs: $target) -> Self::Output {
                <$intermediate>::from(self)
                    .overflowing_sub(<$intermediate>::from(rhs))
                    .0
                    .into()
            }
        }

        impl std::ops::Mul<$target> for Word {
            type Output = Word;
            fn mul(self, rhs: $target) -> Self::Output {
                <$intermediate>::from(self)
                    .overflowing_mul(<$intermediate>::from(rhs))
                    .0
                    .into()
            }
        }

        impl std::ops::Div<$target> for Word {
            type Output = Word;
            fn div(self, rhs: $target) -> Self::Output {
                <$intermediate>::from(self)
                    .overflowing_div(<$intermediate>::from(rhs))
                    .0
                    .into()
            }
        }

        impl std::ops::Rem<$target> for Word {
            type Output = Word;
            fn rem(self, rhs: $target) -> Self::Output {
                <$intermediate>::from(self)
                    .rem(<$intermediate>::from(rhs))
                    .into()
            }
        }

        impl std::ops::BitAnd<$target> for Word {
            type Output = Word;
            fn bitand(self, rhs: $target) -> Self::Output {
                <$intermediate>::from(self)
                    .bitand(<$intermediate>::from(rhs))
                    .into()
            }
        }

        impl std::ops::BitOr<$target> for Word {
            type Output = Word;
            fn bitor(self, rhs: $target) -> Self::Output {
                <$intermediate>::from(self)
                    .bitor(<$intermediate>::from(rhs))
                    .into()
            }
        }

        impl std::ops::BitXor<$target> for Word {
            type Output = Word;
            fn bitxor(self, rhs: $target) -> Self::Output {
                <$intermediate>::from(self)
                    .bitxor(<$intermediate>::from(rhs))
                    .into()
            }
        }

        impl std::ops::Shl<$target> for Word {
            type Output = Word;
            fn shl(self, rhs: $target) -> Self::Output {
                <$intermediate>::from(self)
                    .shl(<$intermediate>::from(rhs))
                    .into()
            }
        }

        impl std::ops::Shr<$target> for Word {
            type Output = Word;
            fn shr(self, rhs: $target) -> Self::Output {
                <$intermediate>::from(self)
                    .shr(<$intermediate>::from(rhs))
                    .into()
            }
        }

        impl std::ops::SubAssign<$target> for Word {
            fn sub_assign(&mut self, rhs: $target) {
                *self = *self - rhs;
            }
        }

        impl std::ops::AddAssign<$target> for Word {
            fn add_assign(&mut self, rhs: $target) {
                *self = *self + rhs;
            }
        }

        impl std::cmp::PartialOrd<$target> for Word {
            fn partial_cmp(&self, rhs: &$target) -> Option<std::cmp::Ordering> {
                <$intermediate>::from(*self).partial_cmp(&<$intermediate>::from(*rhs))
            }
        }
        impl std::cmp::PartialEq<$target> for Word {
            fn eq(&self, rhs: &$target) -> bool {
                <$intermediate>::from(*self).eq(&<$intermediate>::from(*rhs))
            }
        }
    };
}

ops!(Word, u32);

ops!(u8);
ops!(u16);
ops!(u32);
ops!(u64);
ops!(usize);

ops!(i8);
ops!(i16);
ops!(i32);
ops!(i64);
ops!(isize);

#[test]
fn test_address_truncation() {
    let a: Word = 0x11223344.into();
    assert_eq!(a, 0x00223344);
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
