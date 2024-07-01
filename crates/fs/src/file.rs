use crate::native_path::NativePath;
use core::{ffi::CStr, mem::MaybeUninit};

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
    fn open(pathname: *const i8, flags: i32) -> i32;
    fn close(fd: i32) -> i32;
    fn read(fd: i32, buf: *mut MaybeUninit<u8>, count: usize) -> isize;
    fn mkstemp(template: *mut i8) -> i32;
    fn unlink(pathname: *const i8) -> i32;
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

//// OPEN FILE ////

#[cfg(unix)]
impl File {
    pub fn open(path: &NativePath) -> Option<Self> {
        const O_RDONLY: i32 = 0;

        let fd = unsafe { open(path.c_str.as_ptr(), O_RDONLY) };

        if fd != -1 {
            Some(File { fd })
        } else {
            None
        }
    }
}

#[cfg(windows)]
impl File {
    pub fn open(path: &NativePath) -> Option<Self> {
        const GENERIC_READ: u32 = 0x80000000;
        const OPEN_EXISTING: u32 = 3;
        const FILE_SHARE_READ: u32 = 0x00000001;

        let handle = unsafe {
            CreateFileW(
                path.u16_c_str.as_ptr(),
                GENERIC_READ,
                FILE_SHARE_READ,
                core::ptr::null_mut(),
                OPEN_EXISTING,
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
        unsafe { unlink(path.c_str.as_ptr()) == 0 }
    }
}

#[cfg(windows)]
impl File {
    /// Returns whether it succeeded.
    pub fn remove(path: &NativePath) -> bool {
        unsafe { DeleteFileW(path.u16_c_str.as_ptr()) != 0 }
    }
}

//// TEMPFILE ////

#[cfg(unix)]
impl File {
    /// Create a tempfile, open it as a File, pass that File and its generated path
    /// to the given function, and then delete it after the function returns.
    pub fn with_tempfile<T>(run: impl Fn(&NativePath, Option<Self>) -> T) -> T {
        const TEMPLATE: &[u8] = b"/tmp/tempdir_XXXXXX\0";

        let mut template = [0; TEMPLATE.len()];
        template.copy_from_slice(TEMPLATE);

        // mkstemp replaces the Xs in the template with chars that result in a unique path.
        let fd = unsafe { mkstemp(template.as_mut_ptr().cast()) };
        let native_path = NativePath::new(unsafe { CStr::from_ptr(template.as_ptr().cast()) });

        if fd != -1 {
            // Unlink the file *before* we continue. This won't delete the file yet, because
            // we have an open file descriptor to it. Unlinking it up front ensures that if
            // the program crashes, it will get deleted properly anyway (once the fd gets closed).
            unsafe {
                unlink(template.as_ptr().cast());
            }

            // Since we pass an owned File, it will get closed automatically once dropped.
            // This in turn will result in the file getting deleted, since we already unlinked it.
            run(native_path, Some(File { fd }))
        } else {
            // Here we assume that since mkstemp errored out, the file was not created
            // and we shouldn't attempt to unlink it.
            run(native_path, None)
        }
    }
}

//// READ FILE ////

pub trait ReadFile {
    /// Returns either the number of bytes read, or None if the read failed.
    /// (Does not specify what went wrong if the read failed.)
    fn read_into(&mut self, buf: &mut [MaybeUninit<u8>]) -> Option<usize>;
}

#[cfg(unix)]
impl ReadFile for File {
    fn read_into(&mut self, buf: &mut [MaybeUninit<u8>]) -> Option<usize> {
        let bytes_read = unsafe { read(self.fd, buf.as_mut_ptr(), buf.len()) };

        if bytes_read >= 0 {
            Some(bytes_read as usize)
        } else {
            None
        }
    }
}

#[cfg(windows)]
impl ReadFile for File {
    fn read_into(&mut self, buf: &mut [MaybeUninit<u8>]) -> Option<usize> {
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

        if result != 0 {
            Some(bytes_read as usize)
        } else {
            None
        }
    }
}
