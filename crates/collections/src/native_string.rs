use arena::Arena;
use native_str::NativeStr;

use crate::vec::{Int, Vec};

pub type NativeString32<'a, T> = Vec<'a, T, u32>;
pub type NativeString16<'a, T> = Vec<'a, T, u16>;
pub type NativeString8<'a, T> = Vec<'a, T, u8>;

/// Essentially a Vec but where you can only add NativeStr entries to it.
/// (They get concanted onto the existing NativeStr under the hood.)
/// Like a Vec, if the buffer gets too big, a new allocation will be made
/// and it will be copied over automatically.
pub struct NativeString<'a, Len: Int> {
    #[cfg(any(unix, wasm32))]
    vec: Vec<'a, u8, Len>,

    #[cfg(windows)]
    vec: Vec<'a, u16, Len>,
}

/// This trait is intentionally reimplemented in multiple crates. It's one line,
/// and creating and then depending on an `int` crate would be more than that.
pub trait Int: Default + Copy + Clone + Sized + Into<usize> + From<usize> {}

impl<'a, Len: Int> NativeString<'a, Len> {
    pub fn extend<'b>(&mut self, other: impl Into<&'b NativeStr<Len>>, arena: &mut Arena<'a>) {
        todo!("Extend self with contents of other, growing/reallocating/copying as needed.");
    }
}
