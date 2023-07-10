use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, Not, Shl, Shr};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Bitmask(u64);

pub type Chunk = [u8; 64];

impl Bitmask {
    pub const ZERO: Self = Bitmask(0);

    pub const fn new(num: u64) -> Self {
        Self(num)
    }

    /// Branchlessly convert the given 64 bytes into a bitmap
    pub const fn for_byte(bytes: [u8; 64], needle: u8) -> Self {
        // The packed_simd version of this, which we could use if we were on Nightly
        // (assuming it supports sufficiently graceful fallbacks, and supports non-Intel SIMD)
        //
        //     return Self(u8x64::from_slice_unaligned(&bytes).eq(u8x64::splat(needle)).bitmask());
        //
        // However, does an unnecessary number of SIMD loads. Instead, we should do the u8x64::from_slice_unaligned
        // once, and then do multiple splat() and eq() calls, like so:
        //
        //     let simd_chunk = u8x64::from_slice_unaligned(&chunk);
        //     let backslashes = Bitmask::from(simd_chunk.eq(u8x64::splat(b'\\')));
        //     let single_quotes = Bitmask::from(simd_chunk.eq(u8x64::splat(b'\''))) & preceded_by_escape;

        let mut bitmask: u64 = 0;

        // Without branching (e.g. using a loop), fill each of the 64 bits with a 1 if the corresponding byte
        // at that index matches the needle, and a 0 otherwise.
        macro_rules! fill_bitmask {
            ($($index:literal),*) => {
                $(
                    bitmask |= ((bytes[$index] == needle) as u64) << $index;
                )*
            };
        }

        fill_bitmask!(
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
            24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
            46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63
        );

        Self(bitmask)
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

impl From<[u8; 64]> for Bitmask {
    fn from(bytes: [u8; 64]) -> Self {
        // The packed_simd version of this, which we could use if we were on Nightly
        // (assuming it supports sufficiently graceful fallbacks, and supports non-Intel SIMD)
        //
        // return Self(u8x64::from_slice_unaligned(&bytes).bitmask());

        let mut bitmask: u64 = 0;

        // Without branching (e.g. using a loop), fill each of the 64 bits with a 1 if the corresponding byte
        // at that index is nonzero, and a 0 otherwise.
        macro_rules! fill_bitmask {
            ($($index:literal),*) => {
                $(
                    bitmask |= ((bytes[$index] != 0) as u64) << $index;
                )*
            };
        }

        fill_bitmask!(
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
            24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
            46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63
        );

        Self(bitmask)
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
