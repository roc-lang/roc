/// These traits are for testing. If we write functions which need to do I/O
/// in terms of these, then tests can make custom trait impls which simulate
/// various I/O outcomes (including errors that are difficult to simulate otherwise)
/// when calling those functions.
///
/// These are granular (e.g. not a big File trait with all of them) so that
/// the tests only need to specify the ones that are necessary. With a big
/// trait, every test would need to specify every trait member, even though
/// most of them would not come up in the test.
use crate::{path::Path, IoError};

pub trait OpenFile: Sized {
    /// Open the file for reading
    fn open_read(path: &Path) -> Result<Self, IoError>;
}

pub trait FileMetadata: Sized {
    /// The number of bytes the file's metadata says it takes up on disk
    fn size_on_disk(&mut self) -> Result<u64, IoError>;
}

pub trait ReadFile: Sized {
    /// Read the contents of the file into the buffer if the buffer is large enough,
    /// returning how many bytes were actually read. (It might be less than the length
    /// of the buffer.)
    fn read_into(&mut self, buf: &mut [u8]) -> Result<usize, IoError>;
}

pub trait WriteFile: Sized {
    /// Write the given bytes to the file
    fn write(&self, content: &[u8]) -> Result<(), IoError>;
}
