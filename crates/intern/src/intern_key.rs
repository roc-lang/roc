use core::num::NonZeroI16;

#[repr(align(2))]
#[derive(Copy, Clone)]
pub struct InternKey {
    inner: InternKeyInner,
}

impl Clone for InternKeyInner {
    fn clone(&self) -> Self {
        Self {
            tagged: unsafe { self.tagged },
        }
    }
}

#[derive(Copy)]
union InternKeyInner {
    inline: [u8; 2],
    tagged: NonZeroI16,
}

impl InternKey {
    /// Returns whether the string this key references is small enough to be stored inline in the key
    /// (so, the string is either 1 byte or 2 bytes long).
    pub(crate) fn is_inline(&self) -> bool {
        !unsafe { self.inner.tagged }.is_negative()
    }

    /// This is only useful once we've already verified this key is inline!
    pub(crate) fn inline_len(&self) -> usize {
        // If the second byte is nonzero, the length is definitely 2.
        let len_is_2 = unsafe { self.inner.inline[1] != 0 };

        1 + (len_is_2 as usize)
    }

    pub(crate) fn bucket(self) -> Bucket {
        unsafe {
            // + 1 to also shift out the sign bit (which is used to mark inline vs not)
            core::mem::transmute(self.as_i16() >> (Bucket::TAG_BITS + 1))
        }
    }

    pub(crate) unsafe fn as_i16(self) -> i16 {
        Into::<i16>::into(self.inner.tagged)
    }

    /// This is only useful once we've already verified this key is not stored inline!
    /// Assuming that's the case, this returns the index *within* that bucket where
    /// the interned string can be found.
    pub(crate) fn index_within_bucket(&self) -> usize {
        (unsafe { self.as_i16() & 0b0001_1111_1111_1111 }) as usize
    }
}

// If the string is 1 or 2 characters long, since Roc identifiers have to start with an ASCII character,
// we can use a 0 bit on the first character as a flag for a "small ident optimization": we store the
// ident inside the u16 itself. There's a separate question of scope, of course. That's out of (ha) scope here.
// (If in the future we relax that, then we can just decline to do this optimization if the first
// character is not in the ASCII range.) Actually let's just do that now. For length=1B strings we don't need
// to check it (only possible valid UTF-8 is if it's ASCII), and for length=2 strings we just need to check
// if either i8 is negative. (If so, then it's a 2-byte character.) Also, since we know the original was 2B
// in length, we can definitely just stick it in Str4 and call it a day.

// 3 of the 16 bits in the Id are reserved for the tag telling us which bucket this is.
// The other 13 bits are for the index into that bucket (so, 2^13 = 8,192 strings per bucket).
// In order to exhaust this on variables (for example), you would need something like 8k variables
// and they're all 3-4 chars, or 5-8, or 9-16. If that ever does happen, we can always introduce
// a layer of indirection: have each u16 identifier be an index into a Vec of (BucketId, u16 offset into that bucket)
// and then do an extra lookup on resolution. But this is faster, so let's default to it and see if
// it ever comes up as a problem in practice. Seems very unlikely (and of course you can always say,
// "hey, don't do that."
#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Bucket {
    // These bit patterns are used as inline tags, so do not change these numbers!
    Str4 = 0,
    Str8 = 1,
    Str16 = 2,
    StrBig = 3,
}

impl Bucket {
    /// We use 2 bits for the tag
    const TAG_BITS: u32 = 2;

    // Bucket::Str4   => 0
    // Bucket::Str8   => 1
    // Bucket::Str16  => 2
    // Bucket::StrBig => 3
    pub(crate) fn as_bucket_index(self) -> usize {
        self as usize
    }

    /// Returns the size, in bytes, of the strings stored in this bucket,
    /// assuming they are 4, 8, or 16.
    pub(crate) fn str_size(self) -> u32 {
        // 2^(bucket_index + 2) gives the size:
        // bucket_index 0 => 2^2 => Str4
        // bucket_index 1 => 2^3 => Str8
        // bucket_index 2 => 2^4 => Str16
        1 << (self.as_bucket_index() + 2)
    }
}
