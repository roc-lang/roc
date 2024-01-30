// This should be no_std, but we want to be able to use dbg! in development and std conveniences in testing
// Having this be no_std isn't strictly necessary, but it reduces the risk of accidental heap allocations.
#![cfg_attr(not(any(debug_assertions, test)), no_std)]

mod error;

#[cfg(unix)]
pub struct File {
    fd: i32,
}

#[cfg(windows)]
pub struct File {
    handle: *mut u8,
}

#[cfg(unix)]
pub struct OsError(i32);

#[cfg(windows)]
pub struct OsError(u32);

#[cfg(any(unix, windows))]
pub struct ReadError {
    pub error: OsError,
}

#[cfg(any(unix, windows))]
impl OsError {
    #[cfg(unix)]
    pub fn write(&self, buf: &mut [u8]) -> Result<(), OsError> {
        extern "C" {
            // https://pubs.opengroup.org/onlinepubs/009695399/functions/strerror.html
            fn strerror_r(errnum: i32, buf: *mut u8, buf_len: usize) -> i32;
        }

        let error = unsafe { strerror_r(self.0, buf.as_mut_ptr(), buf.len()) };

        if error == 0 {
            Ok(())
        } else {
            Err(OsError(error))
        }
    }

    #[cfg(windows)]
    pub fn write(&self, buf: &mut [u16]) -> usize {
        // https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-formatmessagew
        extern "system" {
            fn FormatMessageW(
                dwFlags: u32,
                lpSource: *const c_void,
                dwMessageId: u32,
                dwLanguageId: u32,
                lpBuffer: *mut u16,
                nSize: u32,
                Arguments: *mut c_void,
            ) -> u32;
        }

        const FORMAT_MESSAGE_FROM_SYSTEM: u32 = 0x00001000;
        const FORMAT_MESSAGE_IGNORE_INSERTS: u32 = 0x00000200;

        let chars_written = unsafe {
            FormatMessageW(
                FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                core::ptr::null(),
                error_code,
                0, // Use default language
                buf.as_mut_ptr(),
                buf.len() as u32,
                core::ptr::null_mut(),
            )
        };

        if chars_written > 0 {
            Ok(chars_written as usize)
        } else {
            Err(OsError(error::last_error()))
        }
    }
}

#[cfg(any(unix, windows))]
impl File {
    pub fn read_from_file(file: &mut File, buf: &mut [u8]) -> Result<usize, ReadError> {
        #[cfg(unix)]
        {
            extern "C" {
                fn read(fd: i32, buf: *mut u8, count: usize) -> isize;
            }

            let bytes_read = unsafe { read(file.fd, buf.as_mut_ptr(), buf.len()) };

            if bytes_read >= 0 {
                Ok(bytes_read as usize)
            } else {
                Err(ReadError {
                    error: OsError(error::last_error()),
                })
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
                    Err(ReadError {
                        error: OsError(error::last_error()),
                    })
                }
            }
        }
    }
}
