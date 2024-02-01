use crate::{IoError, Path};

pub trait File: Sized {
    fn open_read(path: &Path) -> Result<Self, IoError>;
    fn read_into(&mut self, buf: &mut [u8]) -> Result<usize, IoError>;
    fn size_on_disk(&mut self) -> Result<u64, IoError>;

    #[cfg(unix)]
    fn fd(&mut self) -> i32;

    #[cfg(windows)]
    fn handle(&mut self) -> *mut c_void;
}
