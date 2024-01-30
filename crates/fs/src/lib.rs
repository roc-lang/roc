// This should be no_std, but we want to be able to use dbg! in development and std conveniences in testing
// Having this be no_std isn't strictly necessary, but it reduces the risk of accidental heap allocations.
#![cfg_attr(not(any(debug_assertions, test)), no_std)]

mod error;

pub use error::IoError;

#[cfg(unix)]
use core::ffi::CStr;

#[cfg(windows)]
use widestring::ucstr::U16CStr;

#[cfg(unix)]
#[derive(Debug)]
pub struct File {
    fd: i32,
}

#[cfg(unix)]
impl Drop for File {
    fn drop(&mut self) {
        extern "C" {
            // https://www.man7.org/linux/man-pages/man2/close.2.html
            fn close(fd: i32) -> i32;
        }

        unsafe {
            close(self.fd);
        }
    }
}

#[cfg(windows)]
#[derive(Debug)]
pub struct File {
    handle: *mut u8,
}

#[cfg(windows)]
impl Drop for File {
    #[cfg(windows)]
    fn drop(&mut self) {
        extern "system" {
            // https://learn.microsoft.com/en-us/windows/win32/api/handleapi/nf-handleapi-closehandle
            fn CloseHandle(handle: *mut u8) -> i32;
        }

        unsafe {
            CloseHandle(self.handle);
        }
    }
}

#[cfg(unix)]
impl File {
    // https://docs.rs/libc/latest/libc/constant.O_RDONLY.html
    const O_RDONLY: i32 = 0;
    const O_WRONLY: i32 = 1;

    pub fn open_read(path: &CStr) -> Result<Self, IoError> {
        Self::open(path, Self::O_RDONLY)
    }

    pub fn open_write(path: &CStr) -> Result<Self, IoError> {
        Self::open(path, Self::O_WRONLY)
    }

    fn open(path: &CStr, oflag: i32) -> Result<Self, IoError> {
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
}

#[cfg(windows)]
impl File {
    // https://docs.rs/winapi/latest/winapi/um/handleapi/constant.INVALID_HANDLE_VALUE.html
    const INVALID_HANDLE_VALUE: *mut u8 = -1isize as _;

    fn open(
        path: &U16CStr,
        dwDesiredAccess: u32,
        dwCreationDisposition: u32,
        dwFlagsAndAttributes: u32,
    ) -> Result<Self, IoError> {
        extern "C" {
            fn CreateFileW(
                lpFileName: *const u16,
                dwDesiredAccess: u32,
                dwShareMode: u32,
                lpSecurityAttributes: *mut u8,
                dwCreationDisposition: u32,
                dwFlagsAndAttributes: u32,
                hTemplateFile: *mut u8,
            ) -> *mut u8;
        }

        let handle = unsafe {
            CreateFileW(
                file_path.as_ptr(),
                dwDesiredAccess,
                0, // Share mode: prevent other processes from accessing the file
                core::ptr::null_mut(), // Security attributes: none
                dwCreationDisposition,
                dwFlagsAndAttributes,
                core::ptr::null_mut(), // Template file: none
            )
        };

        if handle != INVALID_HANDLE_VALUE {
            Ok(Self { handle })
        } else {
            Err(IoError(error::last_error()))
        }
    }
}

#[cfg(any(unix, windows))]
impl File {
    pub fn read_from_file(file: &mut File, buf: &mut [u8]) -> Result<usize, IoError> {
        #[cfg(unix)]
        {
            extern "C" {
                fn read(fd: i32, buf: *mut u8, count: usize) -> isize;
            }

            let bytes_read = unsafe { read(file.fd, buf.as_mut_ptr(), buf.len()) };

            if bytes_read >= 0 {
                Ok(bytes_read as usize)
            } else {
                Err(IoError::most_recent())
            }
        }

        #[cfg(windows)]
        {
            extern "C" {
                // https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile
                fn ReadFile(
                    hFile: *mut u8,
                    lpBuffer: *mut u8,
                    nNumberOfBytesToRead: u32,
                    lpNumberOfBytesRead: *mut u32,
                    lpOverlapped: *mut u8,
                ) -> i32;
            }

            let mut bytes_read: MaybeUninit<u32> = MaybeUninit::uninit();

            unsafe {
                if ReadFile(
                    file.handle,
                    buf.as_mut_ptr(),
                    buf.len() as u32,
                    bytes_read.as_mut_ptr(),
                    core::ptr::null_mut(),
                ) == 0
                {
                    Ok(bytes_read.assume_init() as usize)
                } else {
                    Err(IoError::most_recent())
                }
            }
        }
    }
}
