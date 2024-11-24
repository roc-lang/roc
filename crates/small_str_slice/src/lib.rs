#![cfg_attr(not(any(debug_assertions, test)), no_std)]

use core::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem,
    num::NonZeroUsize,
    ptr, slice, str,
};
use nonempty_str::NonEmptyStr;

/// Like a &str except it inlines strings that are small enough to fit in size_of::<&str>() bytes,
/// and only supports lengths of up to (size_of::<usize>() - 1) bytes.
pub union SmallStrSlice<'a> {
    small: [u8; mem::size_of::<&'static str>()],
    big: (*const u8, NonZeroUsize),
    _phantom: PhantomData<&'a ()>,
}

impl<'a> SmallStrSlice<'a> {
    const INLINE_MASK: u8 = 0b11000000;
    const BIG_MASK: u8 = 0b11111111;
    const BIG_LEN_MASK: usize = (Self::BIG_MASK as usize) << (8 * (mem::size_of::<usize>() - 1));

    pub fn as_str(&'a self) -> &'a str {
        self.as_nonempty_str().get()
    }

    pub fn as_nonempty_str(&'a self) -> &'a NonEmptyStr {
        let len = self.len().get();
        let small = unsafe {
            let filled_small = self.small_filled();
            let unfilled_small = NonEmptyStr::new_unchecked(str::from_utf8_unchecked(
                &slice::from_raw_parts(self.small.as_ptr(), len),
            ));
            if self.is_filled_small() {
                filled_small
            } else {
                unfilled_small
            }
        };

        let big = unsafe { self.big_str() };

        if self.is_big() {
            big
        } else {
            small
        }
    }

    pub fn is_small_and_unfilled(&self) -> bool {
        (self.discriminant() & Self::INLINE_MASK) == Self::INLINE_MASK
    }

    pub fn len(&self) -> NonZeroUsize {
        let big_len = unsafe { self.big_len() };

        let small_len = unsafe {
            let small_len = NonZeroUsize::new_unchecked(self.small_len() as usize);
            let filled_len = NonZeroUsize::new_unchecked(mem::size_of::<Self>());
            let is_filled = self.is_filled_small();

            if is_filled {
                filled_len
            } else {
                small_len
            }
        };

        if self.is_big() {
            big_len
        } else {
            small_len
        }
    }

    /// Returns whether it's using every byte of the small string.
    fn is_filled_small(&self) -> bool {
        self.discriminant() <= 192
    }

    fn is_big(&self) -> bool {
        self.discriminant() == Self::BIG_MASK
    }

    fn discriminant(&self) -> u8 {
        unsafe { *self.small.get_unchecked(mem::size_of::<Self>() - 1) }
    }

    unsafe fn small_filled(&'a self) -> &'a NonEmptyStr {
        NonEmptyStr::new_unchecked(str::from_utf8_unchecked(&slice::from_raw_parts(
            self as *const Self as *const u8,
            mem::size_of::<Self>(),
        )))
    }

    unsafe fn small_len(&self) -> u8 {
        self.discriminant() & !Self::INLINE_MASK
    }

    #[cfg(target_endian = "little")]
    unsafe fn big_len(&self) -> NonZeroUsize {
        NonZeroUsize::new_unchecked(self.big.1.get() & !Self::BIG_LEN_MASK)
    }

    unsafe fn big_str(&'a self) -> &'a NonEmptyStr {
        let slice = slice::from_raw_parts(self.big.0, self.big_len().get());
        let str = str::from_utf8_unchecked(slice);

        NonEmptyStr::new_unchecked(str)
    }

    #[cfg(target_endian = "little")]
    fn to_big_len(len: NonZeroUsize) -> NonZeroUsize {
        // Safety: the mask has 1s in it.
        unsafe { NonZeroUsize::new_unchecked(len.get() | Self::BIG_LEN_MASK) }
    }

    pub fn from_str(src: &'a NonEmptyStr) -> Self {
        // First, branchlessly make a small string that truncates extras.
        let small = Self::new_small(src);
        let big = Self {
            big: (src.as_ptr(), Self::to_big_len(src.len())),
        };

        // Branchlessly return either the small one or the existing str slice.
        if src.len().get() <= mem::size_of::<Self>() {
            small
        } else {
            big
        }
    }

    /// Silently discards any bytes that don't fit.
    pub fn new_small(src: &NonEmptyStr) -> Self {
        // Safety: we know these bytes are UTF-8, and we're slicing it down to the expected size.
        unsafe {
            Self::from_small_bytes(slice::from_raw_parts(
                src.as_ptr(),
                src.len().get().min(mem::size_of::<Self>()),
            ))
        }
    }

    /// Safety: the given slice must have (len <= size_of::<Self>())
    pub unsafe fn new_small_unchecked(src: &NonEmptyStr) -> Self {
        Self::from_small_bytes(src.get().as_bytes())
    }

    /// Safety: the given bytes must be valid UTF-8, and must have (len <= size_of::<Self>())
    unsafe fn from_small_bytes(bytes: &[u8]) -> Self {
        // Start with zeroes and copy over as many bytes as we got.
        let mut small = [0u8; mem::size_of::<Self>()];

        ptr::copy_nonoverlapping(bytes.as_ptr(), small.as_mut_ptr(), bytes.len());

        // A good explanation of how storing the length works:
        // https://github.com/ParkMyCar/compact_str/blob/main/compact_str/README.md#how-it-works
        // Do a saturating sub because length might be 0.
        let last_byte_ptr = small.get_unchecked_mut(mem::size_of::<Self>() - 1);

        // We don't need a mask if we're using every single one of our bytes for storage. Otherwise, we do!
        let needs_mask = bytes.len() < mem::size_of::<Self>();
        let masked = bytes.len() as u8 | Self::INLINE_MASK;
        let cur_last_byte = *last_byte_ptr;

        // Update the last byte branchlessly.
        *last_byte_ptr = if needs_mask { masked } else { cur_last_byte };

        Self { small }
    }
}

impl<'a> PartialEq for SmallStrSlice<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl<'a> Eq for SmallStrSlice<'a> {}

impl<'a> PartialEq<str> for SmallStrSlice<'a> {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}
impl<'a> PartialEq<&'a str> for SmallStrSlice<'a> {
    fn eq(&self, other: &&'a str) -> bool {
        self.as_str() == *other
    }
}

impl<'a> PartialEq<SmallStrSlice<'a>> for &'a str {
    fn eq(&self, other: &SmallStrSlice<'a>) -> bool {
        *self == other.as_str()
    }
}

impl<'a> Hash for SmallStrSlice<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl<'a> PartialOrd for SmallStrSlice<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for SmallStrSlice<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl<'a> fmt::Display for SmallStrSlice<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

impl<'a> fmt::Debug for SmallStrSlice<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_str_small() {
        let s = SmallStrSlice::from_str("hello");
        assert_eq!(s.as_str(), "hello");
    }

    #[test]
    fn from_str_big() {
        let big_str = "this is a very long string that won't fit in the small buffer";
        let s = SmallStrSlice::from_str(big_str);
        assert_eq!(s.as_str(), big_str);
    }

    #[test]
    fn new_small_not_exact_fit() {
        let s = SmallStrSlice::new_small("hello world");
        assert_eq!(s.as_str(), "hello world");
    }

    #[test]
    fn new_small_exact_fit() {
        let str;

        #[cfg(target_pointer_width = "32")]
        {
            str = "12345678";
        }

        #[cfg(target_pointer_width = "64")]
        {
            str = "1234567890123456";
        }

        let slice = SmallStrSlice::new_small(str);
        assert_eq!(slice.as_str(), str);
    }

    #[test]
    fn new_small_unicode() {
        let s = SmallStrSlice::new_small("こんに");
        assert_eq!(s.as_str(), "こんに");
    }

    #[test]
    fn new_small_exactly_fits() {
        let s = SmallStrSlice::new_small("12345678"); // Assuming 8 bytes for &str
        assert_eq!(s.as_str(), "12345678");
    }

    #[test]
    fn from_str_big_by_1() {
        let s = SmallStrSlice::from_str("123456789"); // One byte too long for msall
        assert_eq!(s.as_str(), "123456789");
    }

    #[test]
    fn new_small_unchecked() {
        let s = unsafe { SmallStrSlice::new_small_unchecked("test") };
        assert_eq!(s.as_str(), "test");
    }

    #[test]
    fn size() {
        assert_eq!(
            core::mem::size_of::<SmallStrSlice>(),
            core::mem::size_of::<&str>()
        );
    }

    #[test]
    fn eq() {
        let s1 = SmallStrSlice::from_str("hello");
        let s2 = SmallStrSlice::from_str("hello");
        let s3 = SmallStrSlice::from_str("world");
        assert_eq!(s1, s2);
        assert_ne!(s1, s3);
        assert_eq!(s1, "hello");
        assert_eq!("hello", s1);
    }

    #[test]
    fn hash() {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let s1 = SmallStrSlice::from_str("hello");
        let s2 = SmallStrSlice::from_str("hello");
        let s3 = "hello";

        let mut hasher1 = DefaultHasher::new();
        let mut hasher2 = DefaultHasher::new();
        let mut hasher3 = DefaultHasher::new();

        s1.hash(&mut hasher1);
        s2.hash(&mut hasher2);
        s3.hash(&mut hasher3);

        assert_eq!(hasher1.finish(), hasher2.finish());
        assert_eq!(hasher1.finish(), hasher3.finish());
    }

    #[test]
    fn ord() {
        let s1 = SmallStrSlice::from_str("abc");
        let s2 = SmallStrSlice::from_str("def");
        let s3 = SmallStrSlice::from_str("abc");

        assert!(s1 < s2);
        assert!(s2 > s1);
        assert!(s1 <= s3);
        assert!(s1 >= s3);
    }

    #[test]
    fn display() {
        let s = SmallStrSlice::from_str("hello");
        assert_eq!(format!("{}", s), "hello");
    }

    #[test]
    fn debug() {
        let s = SmallStrSlice::from_str("hello");
        assert_eq!(format!("{:?}", s), "\"hello\"");
    }
    #[test]
    fn eq_with_str_ref() {
        let small_str = SmallStrSlice::from_str("hello");
        let str_ref: &str = "hello";
        assert_eq!(small_str, str_ref);
        assert_eq!(str_ref, small_str);
    }

    #[test]
    fn ne_with_str_ref() {
        let small_str = SmallStrSlice::from_str("hello");
        let str_ref: &str = "world";
        assert_ne!(small_str, str_ref);
        assert_ne!(str_ref, small_str);
    }

    #[test]
    fn big_len() {
        let big_str = "this is a very long string that won't fit in the small buffer";
        let s = SmallStrSlice::from_str(big_str);
        assert_eq!(s.len(), big_str.len());
    }
}
