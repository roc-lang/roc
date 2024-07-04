use crate::native_path::NativePath;
use core::{fmt, mem::MaybeUninit};

#[cfg(windows)]
use widestring::U16CString;

#[cfg(unix)]
use core::ffi::{c_char, c_int};

#[cfg(unix)]
pub struct File {
    fd: i32,
}

#[cfg(windows)]
pub struct File {
    handle: isize,
}

#[cfg(unix)]
extern "C" {
    fn open(pathname: *const c_char, flags: c_int, ...) -> c_int;
    fn close(fd: c_int) -> c_int;
    fn read(fd: c_int, buf: *mut MaybeUninit<u8>, count: usize) -> isize;
    fn write(fd: c_int, buf: *const u8, count: usize) -> isize;
    fn mkstemp(template: *mut c_char) -> c_int;
    fn unlink(pathname: *const c_char) -> c_int;
}

#[cfg(unix)]
impl Drop for File {
    fn drop(&mut self) {
        unsafe {
            close(self.fd);
        }
    }
}

#[cfg(windows)]
extern "system" {
    fn CreateFileW(
        lpFileName: *const u16,
        dwDesiredAccess: u32,
        dwShareMode: u32,
        lpSecurityAttributes: *mut core::ffi::c_void,
        dwCreationDisposition: u32,
        dwFlagsAndAttributes: u32,
        hTemplateFile: isize,
    ) -> isize;
    fn CloseHandle(hObject: isize) -> i32;
    fn ReadFile(
        hFile: isize,
        lpBuffer: *mut MaybeUninit<u8>,
        nNumberOfBytesToRead: u32,
        lpNumberOfBytesRead: *mut u32,
        lpOverlapped: *mut core::ffi::c_void,
    ) -> i32;
    fn WriteFile(
        hFile: isize,
        lpBuffer: *const u8,
        nNumberOfBytesToWrite: u32,
        lpNumberOfBytesWritten: *mut u32,
        lpOverlapped: *mut core::ffi::c_void,
    ) -> i32;
    fn DeleteFileW(lpFileName: *const u16) -> i32;
    fn GetTempPathW(nBufferLength: u32, lpBuffer: *mut u16) -> u32;
    fn CreateDirectoryW(
        lpPathName: *const u16,
        lpSecurityAttributes: *mut core::ffi::c_void,
    ) -> i32;
}

#[cfg(windows)]
impl Drop for File {
    fn drop(&mut self) {
        unsafe {
            CloseHandle(self.handle);
        }
    }
}

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct FileIoErr(i32);

impl fmt::Debug for FileIoErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl FileIoErr {
    // https://docs.rs/libc/latest/libc/constant.ENOENT.html
    pub const NOT_FOUND: Self = Self(2);
}

//// OPEN FILE ////

#[cfg(unix)]
impl File {
    const O_WRONLY: c_int = 1;
    const O_RDWR: c_int = 2;

    /// source: https://github.com/apple-open-source/macos/blob/8038f956fee603c486e75adbf93cac7a66064f02/Libc/exclave/sys/fcntl.h#L104
    #[cfg(target_os = "macos")]
    const O_CREAT: c_int = 512;

    #[cfg(not(target_os = "macos"))]
    const O_CREAT: c_int = 64;

    #[cfg(target_os = "macos")]
    fn errno() -> FileIoErr {
        extern "C" {
            fn __error() -> *mut FileIoErr;
        }

        unsafe { *__error() }
    }

    #[cfg(not(target_os = "macos"))]
    fn errno() -> FileIoErr {
        extern "C" {
            fn __errno_location() -> *mut FileIoErr;
        }

        unsafe { errno = *__errno_location() }
    }

    pub fn open(path: &NativePath) -> Option<Self> {
        let fd = unsafe { open(path.inner.as_ptr(), Self::O_RDWR) };

        if fd != -1 {
            Some(File { fd })
        } else {
            None
        }
    }

    pub fn create(path: &NativePath) -> Option<Self> {
        let fd = unsafe {
            open(
                path.inner.as_ptr(),
                Self::O_CREAT | Self::O_WRONLY,
                0o644, // read/write mode
            )
        };

        if fd != -1 {
            Some(File { fd })
        } else {
            None
        }
    }
}

#[cfg(windows)]
impl File {
    const GENERIC_WRITE: u32 = 0x40000000;
    const CREATE_ALWAYS: u32 = 2;
    const FILE_SHARE_WRITE: u32 = 0x00000002;

    pub fn open(path: &NativePath) -> Option<Self> {
        let handle = unsafe {
            CreateFileW(
                path.inner.as_ptr(),
                Self::GENERIC_READ | Self::GENERIC_WRITE,
                Self::FILE_SHARE_READ | Self::FILE_SHARE_WRITE,
                core::ptr::null_mut(),
                Self::OPEN_EXISTING,
                0,
                0,
            )
        };

        if handle != -1 {
            Some(File { handle })
        } else {
            None
        }
    }

    pub fn create(path: &NativePath) -> Option<Self> {
        let handle = unsafe {
            CreateFileW(
                path.inner.as_ptr(),
                Self::GENERIC_READ | Self::GENERIC_WRITE,
                Self::FILE_SHARE_READ | Self::FILE_SHARE_WRITE,
                core::ptr::null_mut(),
                Self::CREATE_ALWAYS,
                0,
                0,
            )
        };

        if handle != -1 {
            Some(File { handle })
        } else {
            None
        }
    }
}

//// REMOVE ////

#[cfg(unix)]
impl File {
    /// Returns whether it succeeded.
    pub fn remove(path: &NativePath) -> bool {
        unsafe { unlink(path.inner.as_ptr()) == 0 }
    }
}

#[cfg(windows)]
impl File {
    /// Returns whether it succeeded.
    pub fn remove(path: &NativePath) -> bool {
        unsafe { DeleteFileW(path.inner.as_ptr()) != 0 }
    }
}

//// TEMPFILE ////

#[cfg(unix)]
impl File {
    /// Create a tempfile, open it as a File, pass that File and its generated path
    /// to the given function, and then delete it after the function returns.
    pub fn with_tempfile<T>(run: impl FnOnce(Option<Self>) -> T) -> T {
        const TEMPLATE: &[u8] = b"/tmp/roc_tempfile_XXXXXX\0";

        let mut template = [0; TEMPLATE.len()];
        template.copy_from_slice(TEMPLATE);

        // mkstemp replaces the Xs in the template with chars that result in a unique path.
        let fd = unsafe { mkstemp(template.as_mut_ptr().cast()) };

        if fd != -1 {
            // Unlink the file *before* we continue. This won't delete the file yet, because
            // we have an open file descriptor to it. Unlinking it up front ensures that if
            // the program crashes, it will get deleted properly anyway (once the fd gets closed).
            unsafe {
                unlink(template.as_ptr().cast());
            }

            // Since we pass an owned File, it will get closed automatically once dropped.
            // This in turn will result in the file getting deleted, since we already unlinked it.
            run(Some(File { fd }))
        } else {
            // Here we assume that since mkstemp errored out, the file was not created
            // and we shouldn't attempt to unlink it.
            run(None)
        }
    }

    #[cfg(windows)]
    /// Create a tempfile, open it as a File, pass that File and its generated path
    /// to the given function, and then delete it after the function returns.
    pub fn with_tempfile<T>(run: impl FnOnce(Option<Self>) -> T) -> T {
        let mut temp_path_buf = [0u16; 261];
        let temp_path_len =
            unsafe { GetTempPathW(temp_path_buf.len() as u32, temp_path_buf.as_mut_ptr()) };

        if temp_path_len == 0 || temp_path_len > temp_path_buf.len() as u32 {
            let native_path =
                NativePath::new(U16CString::from_vec_with_nul(temp_path_buf.to_vec()).unwrap());
            return run(&native_path, None);
        }

        let mut template = Vec::from(&temp_path_buf[..temp_path_len as usize]);
        template.extend_from_slice(&[
            b't' as u16,
            b'e' as u16,
            b'm' as u16,
            b'p' as u16,
            b'f' as u16,
            b'i' as u16,
            b'l' as u16,
            b'e' as u16,
            b'.' as u16,
            b't' as u16,
            b'm' as u16,
            b'p' as u16,
            0,
        ]);

        let mut temp_file_name = [0u16; 261];
        let result = unsafe {
            GetTempFileNameW(
                temp_path_buf.as_ptr(),
                U16CString::from_str("tmp").unwrap().as_ptr(),
                0,
                temp_file_name.as_mut_ptr(),
            )
        };

        if result == 0 {
            let native_path =
                NativePath::new(U16CString::from_vec_with_nul(temp_file_name.to_vec()).unwrap());
            return run(&native_path, None);
        }

        let handle = unsafe {
            CreateFileW(
                temp_file_name.as_ptr(),
                0x40000000, // GENERIC_WRITE
                0,
                core::ptr::null_mut(),
                1,    // CREATE_ALWAYS
                0x80, // FILE_ATTRIBUTE_TEMPORARY
                0,
            )
        };

        if handle != -1 {
            // Since we pass an owned File, it will get closed automatically once dropped.
            // This in turn will result in the file getting deleted, since Windows sets
            // tempfiles to delete once the handle is closed.
            run(Some(File { handle }))
        } else {
            // Here we assume that since CreateFileW errored out, the file was not created
            // and we shouldn't attempt to delete it.
            run(None)
        }
    }
}

//// READ FILE ////

impl File {
    #[cfg(unix)]
    pub fn read_into(&mut self, buf: &mut [MaybeUninit<u8>]) -> Result<usize, FileIoErr> {
        let bytes_read = unsafe { read(self.fd, buf.as_mut_ptr(), buf.len()) };

        if bytes_read >= 0 {
            Ok(bytes_read as usize)
        } else {
            Err(Self::errno())
        }
    }

    #[cfg(windows)]
    pub fn read_into(&mut self, buf: &mut [MaybeUninit<u8>]) -> Result<usize, FileIoErr> {
        let mut bytes_read: u32 = 0;

        let result = unsafe {
            ReadFile(
                self.handle,
                buf.as_mut_ptr(),
                buf.len() as u32,
                &mut bytes_read,
                core::ptr::null_mut(),
            )
        };

        if result >= 0 {
            Ok(bytes_read as usize)
        } else {
            Err(Self::errno())
        }
    }
}

//// WRITE FILE ////

impl File {
    #[cfg(unix)]
    pub fn write(&mut self, buf: &[u8]) -> Result<usize, FileIoErr> {
        let bytes_written = unsafe { write(self.fd, buf.as_ptr(), buf.len()) };

        if bytes_written >= 0 {
            Ok(bytes_written as usize)
        } else {
            Err(Self::errno())
        }
    }

    #[cfg(windows)]
    pub fn write(&mut self, buf: &[u8]) -> Result<usize, FileIoErr> {
        let mut bytes_written: u32 = 0;

        let result = unsafe {
            WriteFile(
                self.handle,
                buf.as_ptr(),
                buf.len() as u32,
                &mut bytes_written,
                core::ptr::null_mut(),
            )
        };

        if result != 0 {
            Ok(bytes_written as usize)
        } else {
            Err(Self::errno())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{File, FileIoErr};
    use crate::native_path::NativePath;
    use core::mem::MaybeUninit;

    #[cfg(unix)]
    use core::ffi::CStr;

    #[cfg(windows)]
    use widestring::U16CString;

    #[cfg(unix)]
    fn str_to_cstr(s: &str) -> &CStr {
        CStr::from_bytes_with_nul(s.as_bytes()).expect("CStr conversion failed")
    }

    #[cfg(windows)]
    fn str_to_u16cstr(s: &str) -> &widestring::U16CStr {
        widestring::U16CString::from_str(s)
            .expect("U16CStr conversion failed")
            .as_ref()
    }

    #[cfg(unix)]
    fn mock_path(path: &str) -> &NativePath {
        str_to_cstr(path).into()
    }

    #[cfg(windows)]
    fn mock_path(path: &str) -> &NativePath {
        str_to_u16cstr(path).into()
    }

    #[test]
    fn open_file_failed() {
        let path = mock_path("test_file_that_should_not_exist\0");
        let file = File::open(&path);
        assert!(
            file.is_none(),
            "File should not exist: test_file_that_should_not_exist"
        );
    }

    #[test]
    fn create_write_read_delete() {
        let path = mock_path("roc_test_read_file\0");
        let mut file = File::create(&path).unwrap();

        // Write some data to the file
        let write_data = [42u8; 10];
        let write_result = file.write(&write_data);
        assert!(
            write_result.is_ok(),
            "Failed to write data to the file: roc_test_read_file"
        );
        assert_eq!(
            write_result,
            Ok(write_data.len()),
            "Only wrote some of the bytes requested, not all of them."
        );

        // Close the file by dropping the file descriptor
        drop(file);

        // Reopen the file
        let mut file = File::open(&path).unwrap();
        let mut buffer = [MaybeUninit::uninit(); 10];
        let bytes_read = file.read_into(&mut buffer);
        assert!(
            bytes_read.is_ok(),
            "Failed to read data from the file: roc_test_read_file"
        );
        assert_eq!(
            bytes_read,
            Ok(write_data.len()),
            "Only read some of the bytes that were originally written."
        );

        // Close the file by dropping the file descriptor
        drop(file);

        // Remove the file
        let result = File::remove(&path);
        assert!(result, "Failed to remove the file: roc_test_read_file");

        // Verify that the file no longer exists
        let file_exists = File::open(&path).is_none();
        assert!(
            file_exists,
            "The file should not exist after removal: roc_test_read_file"
        );
    }

    #[test]
    fn write_to_tempfile() {
        File::with_tempfile(|opt_file| {
            let mut file = opt_file.unwrap();

            // Write some data to the file
            let mut buffer = [MaybeUninit::new(42); 10];
            let bytes_written = file.read_into(&mut buffer);

            assert!(
                bytes_written.is_ok(),
                "Failed to write data to the temporary file"
            );
        });
    }

    #[test]
    fn with_tempfile_execution() {
        let result = File::with_tempfile(|file| {
            if let Some(mut f) = file {
                let mut buffer = [MaybeUninit::uninit(); 10];
                f.read_into(&mut buffer).is_ok()
            } else {
                false
            }
        });
        assert!(result);
    }

    #[test]
    fn file_not_found_error() {
        let path = mock_path("non_existent_file\0");
        let file = File::open(&path);
        assert!(file.is_none(), "File should not exist: non_existent_file");

        #[cfg(unix)]
        {
            let error = File::errno();
            assert_eq!(error, FileIoErr::NOT_FOUND, "Expected NOT_FOUND error");
        }

        #[cfg(windows)]
        {
            let error = unsafe { FileIoErr(winapi::um::errhandlingapi::GetLastError() as i32) };
            assert_eq!(error, FileIoErr::NOT_FOUND, "Expected NOT_FOUND error");
        }
    }
}
