use packed_simd::m8x64;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, Not, Shl, Shr};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Bitmask(u64);

pub type Chunk = [u8; 64];

impl Bitmask {
    pub const ZERO: Self = Bitmask(0);

    pub const fn new(num: u64) -> Self {
        Self(num)
    }

    pub const fn is_zero(self) -> bool {
        self.0 == 0
    }

    #[inline(always)]
    pub fn find(needle: u8, chunk: Chunk) -> Self {
        // TODO use ~1 simd instruction for this. Just trying to prove out the architecture right now!
        let mut bitmask = 0;

        for (index, byte) in chunk.iter().copied().enumerate() {
            if byte == needle {
                // record a 1 in the bitmask at this index
                if byte == needle {
                    bitmask |= 1 << index;
                }
            }
        }

        Self(bitmask)
    }

    #[inline(always)]
    pub fn wrapping_add(self, rhs: impl Into<Bitmask>) -> Self {
        Bitmask(self.0.wrapping_add(rhs.into().0))
    }

    #[inline(always)]
    pub fn wrapping_sub(self, rhs: impl Into<Bitmask>) -> Self {
        Bitmask(self.0.wrapping_sub(rhs.into().0))
    }

    #[inline(always)]
    pub fn trailing_zeros(self) -> u32 {
        self.0.trailing_zeros()
    }

    #[inline(always)]
    pub fn leading_zeros(self) -> u32 {
        self.0.trailing_zeros()
    }

    #[inline(always)]
    pub fn trailing_ones(self) -> u32 {
        self.0.trailing_ones()
    }

    #[inline(always)]
    pub fn leading_ones(self) -> u32 {
        self.0.trailing_ones()
    }

    #[inline(always)]
    pub fn count_zeros(self) -> u32 {
        self.0.count_zeros()
    }

    #[inline(always)]
    pub fn count_ones(self) -> u32 {
        self.0.count_ones()
    }

    #[inline(always)]
    pub fn ones(self) -> BitmaskOnes {
        BitmaskOnes { bitmask: self.0 }
    }

    #[inline(always)]
    pub fn into_inner(self) -> u64 {
        self.0
    }
}

pub struct BitmaskOnes {
    bitmask: u64,
}

impl Iterator for BitmaskOnes {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        let bitmask = self.bitmask;

        if bitmask != 0 {
            let index = bitmask.trailing_zeros();

            self.bitmask &= bitmask - 1; // clear the bit we just encountered

            Some(index)
        } else {
            None
        }
    }
}

impl From<u64> for Bitmask {
    fn from(num: u64) -> Self {
        Self(num)
    }
}

impl From<m8x64> for Bitmask {
    fn from(simd: m8x64) -> Self {
        Self(simd.bitmask())
    }
}

impl BitAnd for Bitmask {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Bitmask(self.0 & rhs.0)
    }
}

impl BitOr for Bitmask {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Bitmask(self.0 | rhs.0)
    }
}

impl BitXor for Bitmask {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Bitmask(self.0 ^ rhs.0)
    }
}

impl Not for Bitmask {
    type Output = Self;

    fn not(self) -> Self::Output {
        Bitmask(!self.0)
    }
}

impl Shl<u8> for Bitmask {
    type Output = Self;

    fn shl(self, rhs: u8) -> Self::Output {
        Bitmask(self.0 << rhs)
    }
}

impl Shr<u8> for Bitmask {
    type Output = Self;

    fn shr(self, rhs: u8) -> Self::Output {
        Bitmask(self.0 >> rhs)
    }
}

impl BitAndAssign for Bitmask {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl BitOrAssign for Bitmask {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}
