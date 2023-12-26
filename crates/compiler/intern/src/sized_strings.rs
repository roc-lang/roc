use core::{mem, num::NonZeroU32, ptr, str};

pub trait UsizeModulo {
    fn usize_modulo(self, other: usize) -> usize;
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Str4(NonZeroU32);

impl UsizeModulo for Str4 {
    fn usize_modulo(self, other: usize) -> usize {
        other % self.as_u32() as usize
    }
}

impl Str4 {
    pub fn new(str: &str) -> Self {
        let len = mem::size_of::<Self>().min(str.len());

        unsafe { Self::from_utf8_unchecked(&str.as_bytes()[..len]) }
    }

    /// Safety: `ptr` must point to valid UTF-8 bytes, and
    /// `len` may not exceed the size of `Self`
    pub unsafe fn from_utf8_unchecked(bytes: &[u8]) -> Self {
        // Start with all zeroes, then copy in the contents of the pointer.
        let mut answer = [0u8; mem::size_of::<Self>()];

        ptr::copy_nonoverlapping(bytes.as_ptr(), answer.as_mut_ptr(), bytes.len());

        Self(mem::transmute(answer))
    }

    pub fn to_str(&self) -> &str {
        unsafe {
            // Safety: this is just self as a byte array
            let slice = mem::transmute::<&Self, &[u8; mem::size_of::<Self>()]>(self);

            // Safety: We only ever store valid UTF-8 bytes in here.
            str::from_utf8_unchecked(slice)
        }
    }

    pub fn as_u32(&self) -> u32 {
        self.0.into()
    }

    pub fn as_nonzero_u32(&self) -> NonZeroU32 {
        self.0
    }

    pub fn len(&self) -> usize {
        let trailing_zero_bits = self.as_u32().trailing_zeros();
        let trailing_zero_bytes = trailing_zero_bits / 8;

        mem::size_of::<Self>() - trailing_zero_bytes as usize
    }
}
