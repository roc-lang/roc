use std::{fmt::Debug, mem::ManuallyDrop};
use std::borrow::Cow;

/// Collection of small (length < u16::MAX) strings, stored compactly.
#[derive(Clone, Default, PartialEq, Eq)]
pub struct SmallStringInterner {
    buffer: Vec<u8>,

    // lengths could be Vec<u8>, but the mono refcount generation
    // stringifies Layout's and that creates > 256 character strings
    lengths: Vec<Length>,
    offsets: Vec<u32>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
struct Length(i16);

impl std::fmt::Debug for Length {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind().fmt(f)
    }
}

impl Length {
    #[inline(always)]
    const fn kind(self) -> Kind {
        if self.0 == 0 {
            Kind::Empty
        } else if self.0 > 0 {
            Kind::Interned(self.0 as usize)
        } else {
            Kind::Generated(self.0.unsigned_abs() as usize)
        }
    }

    #[inline(always)]
    const fn from_usize(len: usize) -> Self {
        Self(len as i16)
    }
}

enum Kind {
    Generated(usize),
    Empty,
    Interned(usize),
}

impl Debug for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Generated(arg0) => write!(f, "Generated({arg0})"),
            Self::Empty => write!(f, "Empty"),
            Self::Interned(arg0) => write!(f, "Interned({arg0})"),
        }
    }
}

impl std::fmt::Debug for SmallStringInterner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let strings: Vec<_> = self.iter().collect();

        f.debug_struct("SmallStringInterner")
            .field("buffer", &self.buffer)
            .field("lengths", &self.lengths)
            .field("offsets", &self.offsets)
            .field("strings", &strings)
            .finish()
    }
}

impl SmallStringInterner {
    pub const fn new() -> Self {
        Self {
            buffer: Vec::new(),
            lengths: Vec::new(),
            offsets: Vec::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            // guess: the average symbol length is 5
            buffer: Vec::with_capacity(5 * capacity),

            lengths: Vec::with_capacity(capacity),
            offsets: Vec::with_capacity(capacity),
        }
    }

    /// # Safety
    ///
    /// lengths must be non-negative integers less than 2^15
    pub unsafe fn from_parts(buffer: Vec<u8>, lengths: Vec<u16>, offsets: Vec<u32>) -> Self {
        // the recommended way of transmuting a vector
        let mut lengths = ManuallyDrop::new(lengths);

        let lengths = Vec::from_raw_parts(
            lengths.as_mut_ptr().cast(),
            lengths.len(),
            lengths.capacity(),
        );

        Self {
            buffer,
            lengths,
            offsets,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &str> {
        (0..self.offsets.len()).map(move |index| self.get(index))
    }

    /// Insert without deduplicating
    pub fn insert(&mut self, string: &str) -> usize {
        let string = Self::snakify_ident(string);
        let bytes = string.as_bytes();

        assert!(bytes.len() < (1 << 15));

        let offset = self.buffer.len() as u32;
        let length = Length::from_usize(bytes.len());

        let index = self.lengths.len();

        self.lengths.push(length);
        self.offsets.push(offset);

        self.buffer.extend(bytes);

        index
    }

    /// Create a new entry that uses the same string bytes as an existing entry
    pub fn duplicate(&mut self, existing: usize) -> usize {
        let offset = self.offsets[existing];
        let length = self.lengths[existing];

        let index = self.lengths.len();

        self.lengths.push(length);
        self.offsets.push(offset);

        index
    }

    /// Insert a string equal to the current length into the interner.
    ///
    /// Assuming that normally you don't insert strings consisting of just digits,
    /// this is an easy way to create a unique string name. We use this to create
    /// unique variable names: variable names cannot start with a digit in the source,
    /// so if we insert the current length of `length` as its digits, that is always unique
    pub fn insert_index_str(&mut self) -> usize {
        use std::io::Write;

        let index = self.lengths.len();

        let offset = self.buffer.len();
        write!(self.buffer, "{index}").unwrap();

        // this is a generated name, so store it as a negative length
        let length = Length(-((self.buffer.len() - offset) as i16));

        self.lengths.push(length);
        self.offsets.push(offset as u32);

        index
    }

    #[inline(always)]
    pub fn find_index(&self, string: &str) -> Option<usize> {
        self.find_indices(string).next()
    }

    #[inline(always)]
    pub fn find_indices<'a>(&'a self, string: &'a str) -> impl Iterator<Item = usize> + 'a {
        use std::slice::from_raw_parts;

        let string = Self::snakify_ident(string);
        let target_length = string.len();

        let lengths: &[i16] =
            unsafe { from_raw_parts(self.lengths.as_ptr().cast(), self.lengths.len()) };

        let mut index = 0;

        std::iter::from_fn(move || {
            while let Some(length_match) = find_i16_slice(
                unsafe { from_raw_parts(lengths.as_ptr().add(index), lengths.len() - index) },
                target_length as i16,
            ) {
                index += length_match;

                let offset = self.offsets[index];
                let slice = &self.buffer[offset as usize..][..target_length];

                let result = index;
                index += 1;

                if string.as_bytes() == slice {
                    return Some(result);
                }
            }

            None
        })
    }

    fn get(&self, index: usize) -> &str {
        match self.lengths[index].kind() {
            Kind::Empty => "",
            Kind::Generated(length) | Kind::Interned(length) => {
                let offset = self.offsets[index] as usize;

                let bytes = &self.buffer[offset..][..length];

                unsafe { std::str::from_utf8_unchecked(bytes) }
            }
        }
    }

    pub fn try_get(&self, index: usize) -> Option<&str> {
        if index < self.lengths.len() {
            Some(self.get(index))
        } else {
            None
        }
    }

    pub fn update(&mut self, index: usize, new_string: &str) {
        let new_string = Self::snakify_ident(new_string);
        let length = new_string.len();
        let offset = self.buffer.len();

        // future optimization idea: if the current name bytes are at the end of
        // `buffer`, we can update them in-place
        self.buffer.extend(new_string.bytes());

        self.lengths[index] = Length::from_usize(length);
        self.offsets[index] = offset as u32;
    }

    pub fn find_and_update(&mut self, old_string: &str, new_string: &str) -> Option<usize> {
        match self.find_index(old_string) {
            Some(index) => {
                let new_string = Self::snakify_ident(new_string);
                self.update(index, &new_string);

                Some(index)
            }
            None => None,
        }
    }

    pub fn len(&self) -> usize {
        self.lengths.len()
    }

    pub fn is_empty(&self) -> bool {
        self.lengths.is_empty()
    }

    fn snakify_ident(s: &str) -> Cow<'_, str> {
        if s.chars()
            .next()
            .is_some_and(|first_char| first_char.is_ascii_uppercase())
            || (s.contains('_') && !s.ends_with('_'))
        {
            return s.into();
        }

        let chars: Vec<char> = s.chars().collect();
        let mut index = 0;
        let len = chars.len();
        let mut out = String::new();

        while index < len {
            let prev = if index == 0 {
                None
            } else {
                Some(chars[index - 1])
            };
            let c = chars[index];
            let next = chars.get(index + 1);
            let boundary = match (prev, c, next) {
                // LUU, LUN, and LUL (simplified to LU_)
                (Some(p), curr, _) if !p.is_ascii_uppercase() && curr.is_ascii_uppercase() => true,
                // UUL
                (Some(p), curr, Some(n))
                    if p.is_ascii_uppercase()
                        && curr.is_ascii_uppercase()
                        && n.is_ascii_lowercase() =>
                {
                    true
                }
                _ => false,
            };
            // those are boundary transitions - should push _ and curr
            if boundary {
                out.push('_');
            }
            out.push(c.to_ascii_lowercase());
            index += 1;
        }

        out.into()
    }
}

#[allow(dead_code)]
fn find_i16_slice(slice: &[i16], key: i16) -> Option<usize> {
    // run with RUSTFLAGS="-C target-cpu=native" to enable
    #[cfg(all(
        target_arch = "x86_64",
        target_feature = "avx",
        target_feature = "avx2"
    ))]
    return find_i16_slice_x86_64(slice, key);

    #[cfg(not(all(
        target_arch = "x86_64",
        target_feature = "avx",
        target_feature = "avx2"
    )))]
    return find_i16_slice_fallback(slice, key);
}

#[cfg(all(
    target_arch = "x86_64",
    target_feature = "avx",
    target_feature = "avx2"
))]
fn find_i16_slice_x86_64(slice: &[i16], key: i16) -> Option<usize> {
    use std::arch::x86_64::*;

    let mut index = 0;
    let length = slice.len();

    if length >= 16 {
        let keys = unsafe { _mm256_set1_epi16(key) };

        let ptr = slice.as_ptr();
        while index + 15 < length {
            unsafe {
                let haystack = _mm256_loadu_si256(ptr.add(index).cast());
                let compared = _mm256_cmpeq_epi16(haystack, keys);

                // printer_u16(compared);
                // println!("{:032b}", _mm256_movemask_epi8(compared));

                let mask = _mm256_movemask_epi8(compared);
                let first_one = mask.trailing_zeros() >> 1;

                if first_one != 16 {
                    index += first_one as usize;
                    return Some(index);
                }

                index += 16;
            }
        }
    }

    while index < length {
        if slice[index] == key {
            return Some(index);
        }

        index += 1;
    }

    None
}

#[allow(unused)]
fn find_i16_slice_fallback(slice: &[i16], key: i16) -> Option<usize> {
    let mut index = 0;
    let length = slice.len();

    while index < length {
        if slice[index] == key {
            return Some(index);
        }

        index += 1;
    }

    None
}

#[cfg(test)]
mod test {
    use super::SmallStringInterner;

    #[test]
    fn update_key() {
        let mut interner = SmallStringInterner::default();

        interner.insert("main");
        interner.insert("a");
        assert!(interner.find_and_update("a", "ab").is_some());
        interner.insert("c");
        assert!(interner.find_and_update("c", "cd").is_some());
    }

    #[cfg(all(
        target_arch = "x86_64",
        target_feature = "avx",
        target_feature = "avx2"
    ))]
    #[test]
    fn find_test_1() {
        use super::find_i16_slice;

        assert_eq!(
            find_i16_slice(
                &[50, 39, 28, 17, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                17
            ),
            Some(3)
        );
        assert_eq!(
            find_i16_slice(
                &[50, 39, 28, 17, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                50
            ),
            Some(0)
        );
    }
}
