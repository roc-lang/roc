use core::marker::PhantomData;
use std::fs::Permissions;
use std::io::{self, Error};
use std::num::NonZeroIsize;

#[repr(C)]
pub struct IoVec {
    iov_base: *mut u8,
    iov_len: usize,
}

/// Like a std::io::File, except guaranteed to have been opened with the necessary flags
/// for scatter I/O on each relevant OS. When it's dropped, it closes the underlying
/// file descriptor (on UNIX) or file handle (on Windows).
pub struct ScatterFile<Permissions> {
    #[cfg(unix)]
    fd: i32,

    // Windows often defines this as void*, but it never actually points to anything
    // (so dereferencing it would never work),and -1 is a special value here, so
    // isize is more convenient.
    #[cfg(windows)]
    handle: isize,

    _phantom: PhantomData<Permissions>,
}

impl<Permissions> Drop for ScatterFile<Permissions> {
    fn drop(&mut self) {
        // Explicitly ignore close errors.
        let _ = self.close_mut();
    }
}

#[cfg(windows)]
extern "system" {
    fn CreateFileW(
        lpFileName: *const u16,
        dwDesiredAccess: u32,
        dwShareMode: u32,
        lpSecurityAttributes: Option<*const c_void>,
        dwCreationDisposition: u32,
        dwFlagsAndAttributes: u32,
        hTemplateFile: Option<HANDLE>,
    ) -> HANDLE;
}

impl<Permissions> ScatterFile<Permissions> {
    /// Like std::fs::open, except behind the scenes it sets the necessary OS flags
    /// to enable scatter reads.
    pub fn open_read<P: AsRef<Path>>(path: P) -> io::Result<ScatterFile<Read>> {
        #[cfg(windows)]
        unsafe {
            // ReadFileScatter has some specific requirements for how this must be opened:
            // https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfilescatter
            let handle = CreateFileW(
                win32_path,
                0x80000000, // GENERIC_READ, required by ReadFileScatter
                0x00000001, // FILE_SHARE_READ (so other processes can read this)
                None, // This HANDLE cannot be inherited by child processes (that might be fine; this was just an easy default to choose)
                3,    // OPEN_EXISTING; error out if the file does not already exist
                0x40000000 // FILE_FLAG_OVERLAPPED, required by ReadFileScatter
                | 0x20000000, // FILE_FLAG_NO_BUFFERING, required by ReadFileScatter
                None, // No template file
            );

            if handle != HANDLE::INVALID {
                Ok(ScatterFile {
                    handle,
                    _phantom: PhantomData::default(),
                })
            } else {
                Err(Error::last_os_error())
            }
        }
    }

    /// Like std::fs::open, except behind the scenes it sets the necessary OS flags
    /// to enable scatter writes.
    pub fn open_write<P: AsRef<Path>>(path: P) -> io::Result<ScatterFile<Write>> {
        #[cfg(windows)]
        unsafe {
            // WriteFileGather has some specific requirements for how this must be opened:
            // https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-writefilegather
            let handle = CreateFileW(
                win32_path,
                0x40000000, // GENERIC_WRITE, required by WriteFileGather
                0x00000001, // FILE_SHARE_WRITE (so other processes can write to this)
                None, // This HANDLE cannot be inherited by child processes (that might be fine; this was just an easy default to choose)
                2,    // CREATE_ALWAYS, open if existing; otherwise, create and then open.
                0x40000000 // FILE_FLAG_OVERLAPPED, required by WriteFileGather
                | 0x20000000, // FILE_FLAG_NO_BUFFERING, required by WriteFileGather
                None, // No template file
            );

            if handle != HANDLE::INVALID {
                Ok(ScatterFile {
                    handle,
                    _phantom: PhantomData::default(),
                })
            } else {
                Err(Error::last_os_error())
            }
        }
    }

    pub fn close(mut self) -> io::Result<()> {
        self.close_mut()
    }

    /// This is not exposed because the ScatterFile must not be used anymore after it's been closed.
    /// Instead, close() should be used because it consumes the File.
    /// This signature is necessary so that drop() can use it, because drop() only has &mut self.
    fn close_mut(&mut self) -> io::Result<()> {
        #[cfg(unix)]
        unsafe {
            extern "C" {
                fn close(fd: i32) -> i32;
            }

            close(self.fd)
        }

        #[cfg(windows)]
        unsafe {
            extern "system" {
                fn CloseHandle(hObject: *mut std::os::raw::c_void) -> i32;
            }

            CloseHandle(self.handle)
        }
    }
}

pub struct Read;
pub struct Write;

#[cfg(unix)]
extern "C" {
    fn readv(fd: i32, iov: *const iovec, iovcnt: i32) -> isize;
    fn writev(fd: i32, iov: *const iovec, iovcnt: i32) -> isize;
}

#[cfg(windows)]
pub type HANDLE = NonZeroIsize;

#[cfg(windows)]
impl HANDLE {
    pub const INVALID: Self = unsafe { NonZeroIsize::new_unchecked(-1) };
}

// The OVERLAPPED structure contains information used in asynchronous (or overlapped)
// input and output (I/O).
#[cfg(windows)]
#[repr(C)]
struct OVERLAPPED {
    internal: usize,
    internal_high: usize,
    offset: u32,
    offset_high: u32,
    h_event: HANDLE,
}

#[cfg(windows)]
extern "system" {
    pub fn ReadFileScatter(
        h_file: HANDLE,
        a_segment_array: *mut c_void,
        n_number_of_bytes_to_read: u32,
        reserved: *mut u32,
        lp_overlapped: *mut OVERLAPPED,
    ) -> i32;

    pub fn WriteFileGather(
        h_file: HANDLE,
        a_segment_array: *const FILE_SEGMENT_ELEMENT,
        n_number_of_bytes_to_write: u32,
        reserved: *mut u32,
        lp_overlapped: *mut OVERLAPPED,
    ) -> i32;
}

pub fn read_scatter(file: &ScatterFile<Read>, vecs: &mut [IoVec]) -> Result<(), Error> {
    debug_assert!(vecs.len() < i32::MAX as usize);

    unsafe {
        #[cfg(unix)]
        if readv(file.fd, vecs.as_ptr(), vecs.len() as i32) < 0 {
            return Err(Error::last_os_error());
        }
    }

    Ok(())
}

pub fn write_gather(file: &ScatterFile<Write>, vecs: &[IoVec]) -> Result<(), Error> {
    #[cfg(unix)]
    unsafe {
        debug_assert!(vecs.len() < i32::MAX as usize);

        if writev(file.fd, vecs.as_ptr(), vecs.len() as i32) < 0 {
            return Err(Error::last_os_error());
        }
    }

    #[cfg(windows)]
    unsafe {
        //
    }

    Ok(())
}

#[cfg(windows)]
pub fn read_file_scatter(
    file_handle: HANDLE,
    buffers: &mut [FILE_SEGMENT_ELEMENT],
    num_bytes_to_read: u32,
    overlapped: &mut OVERLAPPED,
) -> Result<(), Error> {
    let result = unsafe {
        ReadFileScatter(
            file_handle,
            buffers.as_ptr(),
            num_bytes_to_read,
            null_mut(),
            overlapped,
        )
    };

    if result == 0 {
        Err(Error::last_os_error())
    } else {
        Ok(())
    }
}

#[cfg(windows)]
pub fn write_file_gather(
    file_handle: HANDLE,
    buffers: &[FILE_SEGMENT_ELEMENT],
    num_bytes_to_write: u32,
    overlapped: &mut OVERLAPPED,
) -> Result<(), Error> {
    let result = unsafe {
        WriteFileGather(
            file_handle,
            buffers.as_ptr(),
            num_bytes_to_write,
            null_mut(),
            overlapped,
        )
    };

    if result == 0 {
        Err(Error::last_os_error())
    } else {
        Ok(result)
    }
}
