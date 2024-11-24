// This should be no_std, but we want to be able to use dbg! in development and std conveniences in testing
// Having this be no_std isn't strictly necessary, but it reduces the risk of accidental heap allocations.
#![cfg_attr(not(any(debug_assertions, test)), no_std)]

use core::{fmt, num::NonZeroUsize};

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct NonEmptyStr {
    inner: str,
}

impl fmt::Display for NonEmptyStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

impl NonEmptyStr {
    /// Safety: this string must be nonempty *and* must contain no zero-bytes
    pub unsafe fn new_unchecked(str: &str) -> &Self {
        debug_assert_ne!(str.len(), 0);
        debug_assert!(str.as_bytes().iter().all(|&b| b != 0));

        unsafe { core::mem::transmute(str) }
    }

    pub fn len(&self) -> NonZeroUsize {
        // Safety: we verified on construction that this is nonempty
        unsafe { NonZeroUsize::new_unchecked(self.inner.len()) }
    }

    pub fn first_byte(&self) -> u8 {
        // Safety: we verified on construction that this is nonempty
        unsafe { *self.inner.as_bytes().get_unchecked(0) }
    }

    pub fn first_byte_mut(&mut self) -> &mut u8 {
        // Safety: we verified on construction that this is nonempty
        unsafe { self.inner.as_bytes_mut().get_unchecked_mut(0) }
    }

    pub fn last_byte(&self) -> u8 {
        // Safety: we verified on construction that this is nonempty
        unsafe { *self.inner.as_bytes().get_unchecked(self.len().get() - 1) }
    }

    pub fn last_byte_mut(&mut self) -> &mut u8 {
        let len = self.len().get();

        // Safety: we verified on construction that this is nonempty
        unsafe { self.inner.as_bytes_mut().get_unchecked_mut(len - 1) }
    }

    pub fn get(&self) -> &str {
        &self.inner
    }

    pub fn get_mut(&mut self) -> &mut str {
        &mut self.inner
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.inner.as_ptr()
    }
}
