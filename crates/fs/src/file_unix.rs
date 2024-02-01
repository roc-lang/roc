use crate::error_unix::IoError;
use crate::file::File;
use crate::path::Path;

#[derive(Debug)]
#[repr(transparent)]
pub struct NeverClosedFile {
    fd: i32,
}

#[derive(Debug)]
#[repr(transparent)]
pub struct SelfClosingFile {
    file: NeverClosedFile,
}

impl Drop for SelfClosingFile {
    fn drop(&mut self) {
        extern "C" {
            // https://www.man7.org/linux/man-pages/man2/close.2.html
            fn close(fd: i32) -> i32;
        }

        unsafe {
            close(self.fd());
        }
    }
}

impl NeverClosedFile {
    // https://docs.rs/libc/latest/libc/constant.O_RDONLY.html
    const O_RDONLY: i32 = 0;

    fn open(path: &Path, oflag: i32) -> Result<Self, IoError> {
        extern "C" {
            // https://www.man7.org/linux/man-pages/man2/open.2.html
            fn open(path: *const i8, oflag: i32, ...) -> i32;
        }

        let fd = unsafe { open(path.as_ptr(), oflag) };

        if fd >= 0 {
            Ok(Self { fd })
        } else {
            Err(IoError::most_recent())
        }
    }

    fn metadata(&mut self) -> Result<Stat64, IoError> {
        use core::mem::MaybeUninit;

        extern "C" {
            // https://linux.die.net/man/2/fstat64
            fn fstat64(fd: i32, buf: *mut Stat64) -> i32;
        }

        let mut buf = MaybeUninit::uninit();

        unsafe {
            if fstat64(self.fd, buf.as_mut_ptr()) == 0 {
                Ok(buf.assume_init())
            } else {
                Err(IoError::most_recent())
            }
        }
    }
}

impl File for NeverClosedFile {
    fn fd(&mut self) -> i32 {
        self.fd
    }

    fn open_read(path: &Path) -> Result<Self, IoError> {
        Self::open(path, Self::O_RDONLY)
    }

    fn read_into(&mut self, buf: &mut [u8]) -> Result<usize, IoError> {
        extern "C" {
            fn read(fd: i32, buf: *mut u8, count: usize) -> isize;
        }

        let bytes_read = unsafe { read(self.fd, buf.as_mut_ptr(), buf.len()) };

        if bytes_read >= 0 {
            Ok(bytes_read as usize)
        } else {
            Err(IoError::most_recent())
        }
    }

    fn size_on_disk(&mut self) -> Result<u64, IoError> {
        Ok(self.metadata()?.st_size as u64)
    }
}

impl File for SelfClosingFile {
    fn fd(&mut self) -> i32 {
        self.file.fd()
    }

    fn open_read(path: &Path) -> Result<Self, IoError> {
        match NeverClosedFile::open_read(path) {
            Ok(file) => Ok(Self { file }),
            Err(err) => Err(err),
        }
    }

    fn read_into(&mut self, buf: &mut [u8]) -> Result<usize, IoError> {
        self.file.read_into(buf)
    }

    fn size_on_disk(&mut self) -> Result<u64, IoError> {
        self.file.size_on_disk()
    }
}

#[derive(Copy, Clone, Debug)]
#[repr(C)]
/// https://docs.rs/libc/latest/src/libc/unix/linux_like/linux/gnu/b64/x86_64/mod.rs.html#96
pub struct Stat64 {
    pub st_dev: u64,
    pub st_ino: u64,
    pub st_nlink: u64,
    pub st_mode: u32,
    pub st_uid: u32,
    pub st_gid: u32,
    __padding: i32,
    pub st_rdev: u64,
    pub st_size: i64,
    pub st_blksize: i64,
    pub st_blocks: i64,
    pub st_atime: i64,
    pub st_atime_nsec: i64,
    pub st_mtime: i64,
    pub st_mtime_nsec: i64,
    pub st_ctime: i64,
    pub st_ctime_nsec: i64,
    __reserved: [i64; 3],
}
