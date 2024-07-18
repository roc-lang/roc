use crate::native_path::NativePath;
use core::{fmt, mem::MaybeUninit};

#[cfg(windows)]
use widestring::U16CStr;

#[cfg(unix)]
use core::ffi::{c_char, c_int, CStr};

#[cfg(unix)]
pub struct File {
    fd: i32,
}

#[cfg(windows)]
pub struct File {
    handle: isize,
}

// User and group can read and write (but not execute), but
// world cannot do anything. (This is a cache in your home dir!)
#[cfg(unix)]
const CACHE_DIR_MODE: u32 = 0o660;

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
    fn GetTempPath2W(nBufferLength: u32, lpBuffer: *mut u16) -> u32;
    fn GetTempFileNameW(
        lpPathName: *const u16,
        lpPrefixString: *const u16,
        uUnique: u32,
        lpTempFileName: *mut u16,
    ) -> u32;
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

#[cfg_attr(unix, repr(i32))]
#[cfg_attr(windows, repr(u32))]
#[derive(Copy, Clone, PartialEq, Eq)]
#[non_exhaustive] // There are tons of other error numbers that functions which return this might return!
pub enum FileIoErr {
    /// "File not found" is the same code (namely, 2) on both UNIX and Windows:
    /// ENOENT on UNIX: https://www.man7.org/linux/man-pages/man3/errno.3.html
    /// ERROR_FILE_NOT_FOUND on Windows:  // https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes--0-499-
    NotFound = 2,

    #[cfg(unix)]
    /// EACCES: https://www.man7.org/linux/man-pages/man3/errno.3.html
    AccessDenied = 13,

    #[cfg(windows)]
    /// ERROR_ACCESS_DENIED: https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes--0-499-
    AccessDenied = 5,

    #[cfg(unix)]
    /// EEXIST: https://www.man7.org/linux/man-pages/man3/errno.3.html
    AlreadyExists = 17,

    #[cfg(windows)]
    /// ERROR_FILE_EXISTS: https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes--0-499-
    AlreadyExists = 80,
}

impl fmt::Debug for FileIoErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FileIoErr::NotFound => write!(f, "NotFound ({})", *self as i32),
            #[cfg(unix)]
            FileIoErr::AccessDenied => write!(f, "AccessDenied ({})", *self as i32),
            #[cfg(windows)]
            FileIoErr::AccessDenied => write!(f, "AccessDenied ({})", *self as i32),
            FileIoErr::AlreadyExists => write!(f, "AlreadyExists ({})", *self as i32),
        }
    }
}

impl FileIoErr {
    #[cfg(target_os = "macos")]
    fn most_recent() -> FileIoErr {
        extern "C" {
            fn __error() -> *mut FileIoErr;
        }

        unsafe { *__error() }
    }

    #[cfg(all(target_family = "unix", not(target_os = "macos")))]
    fn most_recent() -> FileIoErr {
        extern "C" {
            fn __errno_location() -> *mut FileIoErr;
        }

        unsafe { *__errno_location() }
    }

    #[cfg(windows)]
    fn most_recent() -> FileIoErr {
        extern "system" {
            fn GetLastError() -> FileIoErr;
        }

        unsafe { GetLastError() }
    }
}

// OPEN FILE //

#[cfg(unix)]
impl File {
    const O_WRONLY: c_int = 1;
    const O_RDWR: c_int = 2;

    /// source: https://github.com/apple-open-source/macos/blob/8038f956fee603c486e75adbf93cac7a66064f02/Libc/exclave/sys/fcntl.h#L104
    #[cfg(target_os = "macos")]
    const O_CREAT: c_int = 512;

    #[cfg(not(target_os = "macos"))]
    const O_CREAT: c_int = 64;

    /// source: https://github.com/apple-open-source/macos/blob/8038f956fee603c486e75adbf93cac7a66064f02/Libc/exclave/sys/fcntl.h#L106
    #[cfg(target_os = "macos")]
    const O_EXCL: c_int = 2048;

    #[cfg(not(target_os = "macos"))]
    const O_EXCL: c_int = 128;

    pub fn open(path: &NativePath) -> Result<Self, FileIoErr> {
        let fd = unsafe { open(path.as_ptr().cast(), Self::O_RDWR) };

        if fd != -1 {
            Ok(File { fd })
        } else {
            Err(FileIoErr::most_recent())
        }
    }

    pub fn create(path: &NativePath) -> Result<Self, FileIoErr> {
        let fd = unsafe {
            open(
                path.as_ptr().cast(),
                Self::O_CREAT | Self::O_WRONLY | Self::O_EXCL, // O_EXCL means fail if it already exists
                // read/write mode
                0o644,
            )
        };

        if fd != -1 {
            Ok(File { fd })
        } else {
            Err(FileIoErr::most_recent())
        }
    }

    /// Open the given file. If it doesn't exist, create it and populate it
    /// using the given function. (If its parent directory didn't exist, create that too.)
    /// If it existed, write its contents into the given buffer and return it.
    ///
    /// If the returned buffer is empty, it means the cache file is currently empty;
    /// either becuase we just created it, or because it already existed but was empty.
    /// Since this function also returns the already-opened File, the caller can now write
    /// into it directly if the buffer is empty.
    ///
    /// This is designed to minimize the number of syscalls involved in this common operation.
    pub fn read_cache_file_into<'buf>(
        // The full path to the file we want to read.
        path: &NativePath,
        // The null-terminated path to the file's parent directory.
        // (We ask for this rather than inferring it because the caller likely
        // already has it in memory due to having used it to construct the file's path.)
        // Needs to be a mut reference because we use create_dir_all on it, which
        // uses mutable references to nul-terminate intermediate dirs in-place.
        parent_dir: &mut NativePath,
        buf: &'buf mut [MaybeUninit<u8>],
    ) -> Result<(Self, &'buf [u8]), FileIoErr> {
        // We may run this operation again if it fails the first time
        // due to the parent directory not existing yet.
        let get_file = || {
            let fd = unsafe {
                open(
                    path.as_ptr().cast(),
                    // Do *not* specify O_EXCL, as that would cause this to fail if it already existed!
                    Self::O_RDWR | Self::O_CREAT,
                    // read/write mode
                    0o644,
                )
            };

            if fd != -1 {
                Ok(File { fd })
            } else {
                Err(FileIoErr::most_recent())
            }
        };

        match get_file() {
            Ok(mut file) => {
                // The file already existed; read its contents and return them.
                let answer_buf = file.read_into(buf)?;

                Ok((file, answer_buf))
            }
            Err(io_err) => {
                match io_err {
                    FileIoErr::NotFound => {
                        // The parent directory didn't exist, so we need to create it.
                        // If creating the parent dir fails, we won't be able to write to it.
                        Self::create_dir_all(parent_dir)?;

                        // If trying to open the file again still fails after creating
                        // the parent directory, give up and return Err.
                        let file = get_file()?;

                        // Return an empty buffer because we just created the file;
                        // we don't need to read it to know that it's empty!
                        Ok((file, &[]))
                    }
                    other => {
                        // We got an access denied error or something; assume we won't
                        // be able to write the file.
                        Err(other)
                    }
                }
            }
        }
    }
}

#[cfg(windows)]
impl File {
    const GENERIC_READ: u32 = 0x80000000;
    const GENERIC_WRITE: u32 = 0x40000000;
    const FILE_SHARE_READ: u32 = 1;
    const FILE_SHARE_WRITE: u32 = 2;
    const CREATE_NEW: u32 = 1;
    const CREATE_ALWAYS: u32 = 2;
    const OPEN_EXISTING: u32 = 3;

    pub fn open(path: &NativePath) -> Result<Self, FileIoErr> {
        let handle = unsafe {
            CreateFileW(
                path.as_ptr(),
                Self::GENERIC_READ | Self::GENERIC_WRITE,
                Self::FILE_SHARE_READ | Self::FILE_SHARE_WRITE,
                core::ptr::null_mut(),
                Self::OPEN_EXISTING,
                0,
                0,
            )
        };

        if handle != -1 {
            Ok(File { handle })
        } else {
            Err(FileIoErr::most_recent())
        }
    }

    pub fn create(path: &NativePath) -> Result<Self, FileIoErr> {
        let handle = unsafe {
            CreateFileW(
                path.as_ptr(),
                Self::GENERIC_READ | Self::GENERIC_WRITE,
                Self::FILE_SHARE_READ | Self::FILE_SHARE_WRITE,
                core::ptr::null_mut(),
                Self::CREATE_ALWAYS,
                0,
                0,
            )
        };

        if handle != -1 {
            Ok(File { handle })
        } else {
            Err(FileIoErr::most_recent())
        }
    }
}

// REMOVE //

#[cfg(unix)]
impl File {
    /// Returns whether it succeeded.
    pub fn remove(path: &NativePath) -> bool {
        unsafe { unlink(path.as_ptr().cast()) == 0 }
    }
}

#[cfg(windows)]
impl File {
    /// Source: https://microsoft.github.io/windows-docs-rs/doc/windows/Win32/Foundation/constant.MAX_PATH.html
    const MAX_PATH: u32 = 260;

    /// Returns whether it succeeded.
    pub fn remove(path: &NativePath) -> bool {
        unsafe { DeleteFileW(path.as_ptr()) != 0 }
    }
}

// TEMPFILE //

#[cfg(unix)]
impl File {
    /// Create a tempfile, open it as a File, pass that File and its generated path
    /// to the given function, and then delete it after the function returns.
    pub fn with_tempfile<T>(
        run: impl FnOnce(Result<(&NativePath, &mut Self), FileIoErr>) -> T,
    ) -> T {
        const TEMPLATE: &[u8] = b"/tmp/roc_tempfile_XXXXXX\0";

        let mut template = [0; TEMPLATE.len()];
        template.copy_from_slice(TEMPLATE);

        // mkstemp replaces the Xs in the template with chars that result in a unique path.
        let fd = unsafe { mkstemp(template.as_mut_ptr().cast()) };

        if fd != -1 {
            let path_cstr = unsafe { CStr::from_bytes_with_nul_unchecked(&template) };
            let native_path: &NativePath = path_cstr.into();
            let mut file = File { fd };

            // Since we pass an owned File, it will get closed automatically once dropped.
            // This in turn will result in the file getting deleted, since we already unlinked it.
            let answer = run(Ok((native_path, &mut file)));

            // Unlink the file now that we're done with it.
            unsafe {
                unlink(template.as_ptr().cast());
            }

            answer
        } else {
            // Here we assume that since mkstemp errored out, the file was not created
            // and we shouldn't attempt to unlink it.
            run(Err(FileIoErr::most_recent()))
        }
    }
}

#[cfg(windows)]
impl File {
    /// Create a tempfile, open it as a File, pass that File and its generated path
    /// to the given function, and then delete it after the function returns.
    pub fn with_tempfile<T>(
        run: impl FnOnce(Result<(&NativePath, &mut Self), FileIoErr>) -> T,
    ) -> T {
        let tempdir_path: &mut [MaybeUninit<u16>] =
            &mut [MaybeUninit::uninit(); Self::MAX_PATH as usize + 1];

        let tempdir_path_len =
            unsafe { GetTempPath2W(tempdir_path.len() as u32, tempdir_path.as_mut_ptr().cast()) };

        if tempdir_path_len == 0 {
            return run(Err(FileIoErr::most_recent()));
        }

        let suffix =
            // Note: only up to the first 3 chars of this string will be used,
            // so we only give it 3.
            //
            // https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-gettempfilenamew
            widestring::u16cstr!("roc").as_slice_with_nul();

        let tempfile_path: &mut [MaybeUninit<u16>] =
            &mut [MaybeUninit::uninit(); Self::MAX_PATH as usize + 1];

        let result = unsafe {
            GetTempFileNameW(
                tempdir_path.as_ptr().cast(),
                suffix.as_ptr(),
                0,
                tempfile_path.as_mut_ptr().cast(),
            )
        };

        if result == 0 {
            return run(Err(FileIoErr::most_recent()));
        }

        let handle = unsafe {
            CreateFileW(
                tempfile_path.as_mut_ptr().cast(),
                Self::GENERIC_WRITE,
                Self::FILE_SHARE_READ | Self::FILE_SHARE_WRITE,
                core::ptr::null_mut(),
                Self::CREATE_ALWAYS,
                0x80, // FILE_ATTRIBUTE_TEMPORARY
                0,
            )
        };

        if handle != -1 {
            let mut file = File { handle };
            let native_path = unsafe { U16CStr::from_ptr_str(tempfile_path.as_ptr().cast()) };

            // Windows automatically deletes tempfiles when the last handle
            // to them is closed, so we don't need to delete this explicitly.
            run(Ok((native_path.into(), &mut file)))
        } else {
            run(Err(FileIoErr::most_recent()))
        }
    }
}

// READ FILE //

impl File {
    #[cfg(unix)]
    pub fn read_into<'buf>(
        &mut self,
        buf: &'buf mut [MaybeUninit<u8>],
    ) -> Result<&'buf mut [u8], FileIoErr> {
        let bytes_read = unsafe { read(self.fd, buf.as_mut_ptr(), buf.len()) };

        if bytes_read >= 0 {
            // Return the subset of the buf that we actually initialized.
            let buf_ptr = buf.as_mut_ptr();
            let len = bytes_read as usize;

            Ok(unsafe { core::slice::from_raw_parts_mut(buf_ptr.cast(), len) })
        } else {
            Err(FileIoErr::most_recent())
        }
    }

    #[cfg(windows)]
    pub fn read_into<'buf>(
        &mut self,
        buf: &'buf mut [MaybeUninit<u8>],
    ) -> Result<&'buf mut [u8], FileIoErr> {
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
            let buf_ptr = buf.as_mut_ptr();
            let len = bytes_read as usize;

            Ok(unsafe { core::slice::from_raw_parts_mut(buf_ptr.cast(), len) })
        } else {
            Err(FileIoErr::most_recent())
        }
    }
}

// CREATE_DIR_ALL //

impl File {
    /// This minimizes syscalls and performs no allocations. The way it avoids performing allocations
    /// is to take a mut reference to the path, so that it can swap in and out nul terminator bytes
    /// to create a nul-terminated "slice" in-place for the current directory it needs to mkdir.
    ///
    /// When it returns, the given path will reference the same bytes it originally did; the only
    /// reason the reference needs to be mutable is for thread safety, as it will be modified
    /// in the middle of the function (even though it will have been restored by the end).
    fn create_dir_all(dir: &mut NativePath) -> Result<(), FileIoErr> {
        #[cfg(unix)]
        {
            Self::create_dir_all_unix::<RealUnixFs>(dir)
        }

        #[cfg(windows)]
        {
            Self::create_dir_windows::<RealWindowsFs>(dir)
        }
    }
}

/// The actual filesystem implementations that we use when exposing functions.
/// Internally, tests can use fake trait implementations instead of this.
struct RealUnixFs;

trait UnixMkdir {
    unsafe fn mkdir(pathname: *const c_char, mode: u32) -> c_int;
}

impl UnixMkdir for RealUnixFs {
    unsafe fn mkdir(pathname: *const c_char, mode: u32) -> c_int {
        extern "C" {
            fn mkdir(pathname: *const c_char, mode: u32) -> c_int;
        }

        mkdir(pathname, mode)
    }
}

#[cfg(unix)]
impl File {
    fn create_dir_all_unix<Unix: UnixMkdir>(dir: &mut NativePath) -> Result<(), FileIoErr> {
        let original_ptr = dir.as_ptr() as *mut u8;
        let mut ch_ptr = original_ptr;
        let mut index: usize = 0;

        loop {
            let ch = unsafe { *ch_ptr };

            if (ch == 0 || ch == b'/')
                // Don't try to mkdir if the preceding char was '/', because
                // we just did a mkdir on that! So either we're in a "//" situation,
                // or else the string ended in '/'. In either case, we shouldn't mkdir.
                //
                // Use saturating_sub so we don't try to mkdir the root ('/') dir.
                && (unsafe { *original_ptr.add(index.saturating_sub(1)) } != b'/')
            {
                let outcome;

                // Temporarily swap out this slash for a nul terminator, then call mkdir.
                unsafe {
                    *ch_ptr = 0;

                    outcome = Unix::mkdir(
                        // The original path, except now nul-terminated at the current dir.
                        original_ptr.cast(),
                        CACHE_DIR_MODE,
                    );

                    // Swap the nul-terminator back to a slash before doing anything else.
                    *ch_ptr = b'/';
                }

                if outcome != 0 {
                    match FileIoErr::most_recent() {
                        FileIoErr::AlreadyExists => {
                            // The directory already exists. Great!
                            // Nothing more to do here; continue as normal.
                        }
                        other => {
                            // We faied to create this directory; error out.
                            return Err(other);
                        }
                    }
                }
            }

            if ch == 0 {
                // We've just processed the last component of the path. We're done!
                return Ok(());
            }

            index += 1;
            ch_ptr = unsafe { original_ptr.add(index) };
        }
    }
}

// WRITE FILE //

impl File {
    #[cfg(unix)]
    pub fn write(&mut self, buf: &[u8]) -> Result<usize, FileIoErr> {
        let bytes_written = unsafe { write(self.fd, buf.as_ptr(), buf.len()) };

        if bytes_written >= 0 {
            Ok(bytes_written as usize)
        } else {
            Err(FileIoErr::most_recent())
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
            Err(FileIoErr::most_recent())
        }
    }
}

// TESTS //

#[cfg(test)]
mod tests {
    use super::{File, FileIoErr};
    use core::mem::MaybeUninit;
    use std::cell::RefCell;

    #[cfg(unix)]
    use super::CACHE_DIR_MODE;

    #[cfg(unix)]
    struct FakeUnixFs;

    #[cfg(unix)]
    #[derive(Clone, Debug, PartialEq, Eq)]
    struct UnixMkdirCall {
        path: String,
        mode: u32,
        ret: c_int,
    }

    #[cfg(unix)]
    use core::ffi::{c_char, c_int, CStr};

    #[cfg(unix)]
    thread_local! {
        static MKDIR_CALLS: RefCell<Vec<UnixMkdirCall>> = RefCell::new(Vec::new());
    }

    #[cfg(unix)]
    impl super::UnixMkdir for FakeUnixFs {
        unsafe fn mkdir(pathname: *const c_char, mode: u32) -> c_int {
            const ANSWER: c_int = 0; // Note: if we want to return errors, we must mock errno() too

            MKDIR_CALLS.with(|calls| {
                calls.borrow_mut().push(UnixMkdirCall {
                    path: CStr::from_ptr(pathname).to_str().unwrap().to_string(),
                    mode,
                    ret: ANSWER,
                });
            });

            ANSWER
        }
    }

    impl FakeUnixFs {
        #[cfg(unix)]
        fn get_mkdir_calls() -> Vec<UnixMkdirCall> {
            MKDIR_CALLS.with(|calls| calls.borrow().clone())
        }
    }

    #[cfg(unix)]
    macro_rules! path {
        ($path:literal) => {
            unsafe {
                $crate::native_path::NativePath::from_slice_unchecked(
                    concat!($path, "\0").as_bytes(),
                )
            }
        };
    }

    #[cfg(unix)]
    macro_rules! with_path_mut {
        ($path:literal, $closure:expr) => {{
            const NUL_TERMINATED: &str = concat!($path, "\0");

            let mut array: core::mem::MaybeUninit<[u8; NUL_TERMINATED.len()]> =
                core::mem::MaybeUninit::uninit();

            unsafe {
                core::ptr::copy_nonoverlapping(
                    NUL_TERMINATED.as_ptr(),
                    array.as_mut_ptr().cast(),
                    NUL_TERMINATED.len(),
                );
            };

            $closure(unsafe {
                $crate::native_path::NativePath::from_slice_unchecked_mut(&mut array.assume_init())
            })
        }};
    }

    #[cfg(windows)]
    macro_rules! path {
        ($path:literal) => {
            $crate::native_path::NativePath::from(widestring::u16_str($path))
        };
    }

    #[cfg(windows)]
    macro_rules! with_path_mut {
        ($path:literal, $closure:expr) => {{
            const NUL_TERMINATED: &widestring::U16str = widestring::u16_str($path)

            let mut array: core::mem::MaybeUninit<[u16; NUL_TERMINATED.len()]> =
                core::mem::MaybeUninit::uninit();

            unsafe {
                core::ptr::copy_nonoverlapping(
                    NUL_TERMINATED.as_ptr(),
                    array.as_mut_ptr().cast(),
                    NUL_TERMINATED.len(),
                );
            };

            $closure(unsafe {
                $crate::native_path::NativePath::from_slice_unchecked_mut(&mut array.assume_init())
            })
        }};
    }

    #[test]
    fn file_not_found() {
        let file_result = File::open(path!("test_file_that_should_not_exist"));

        assert_eq!(
            file_result.map(|_| ()),
            Err(FileIoErr::NotFound),
            "File should not exist: test_file_that_should_not_exist"
        );
    }

    #[test]
    fn create_write_read_delete() {
        let path = path!("roc_test_read_file");
        let mut file = File::create(path).unwrap();

        // Write some data to the file
        let write_data: &[u8] = &[42u8; 10];
        let write_result = file.write(write_data);
        assert_eq!(
            write_result,
            Ok(write_data.len()),
            "Failed to write all data to the file: roc_test_read_file"
        );

        // Close the file by dropping the file descriptor
        drop(file);

        // Reopen the file
        let mut file = File::open(path).unwrap();
        let mut input_buf = [MaybeUninit::uninit(); 10];
        let result = file.read_into(&mut input_buf);
        assert_eq!(
            result.map(|mut_slice| &*mut_slice),
            Ok(write_data),
            "Failed to read all the data from the file: roc_test_read_file"
        );

        // Close the file by dropping the file descriptor
        drop(file);

        // Remove the file
        let result = File::remove(path);
        assert!(result, "Failed to remove the file: roc_test_read_file");

        // Verify that the file no longer exists
        let file_result = File::open(path);

        assert_eq!(
            file_result.map(|_| ()),
            Err(FileIoErr::NotFound),
            "File should not exist after removal: roc_test_read_file"
        );
    }

    #[test]
    fn tempfile() {
        const DATA: &[u8] = &[42; 10];
        let buf = &mut [MaybeUninit::uninit(); DATA.len()];

        let answer = File::with_tempfile(|result| {
            let (native_path, file) = result.unwrap();

            // Write some data to the tempfile
            assert_eq!(
                Ok(DATA.len()),
                file.write(DATA),
                "Failed to write all the bytes to the tempfile"
            );

            // Reopen the tempfile with a fresh file descriptor, and read the data back in from it
            File::open(native_path).unwrap().read_into(buf)
        });

        // The data read should match the data written
        assert_eq!(
            Ok(DATA),
            answer.map(|mut_slice| &*mut_slice),
            "Data read from the file does not match data written to the file"
        );
    }

    #[test]
    fn create_file_that_already_exists() {
        let path = path!("roc_test_already_exists");

        let first = File::create(path); // Create the file for the first time
        let second = File::create(path); // Attempt to create the same file again
        let removed = File::remove(path); // Remove the file now that we're done with it (before we do any assertions!)

        assert!(
            first.is_ok(),
            "Failed to create the file: roc_test_already_exists"
        );

        assert_eq!(
            second.map(|_| ()),
            Err(FileIoErr::AlreadyExists),
            "File should already exist: roc_test_already_exists"
        );

        assert!(
            removed,
            "Failed to remove the file: roc_test_already_exists"
        );
    }

    #[test]
    fn create_single_slash() {
        with_path_mut!("/", |path| {
            let result = File::create_dir_all(path);
            assert!(
                result.is_ok(),
                "Single slash should be treated as root and succeed"
            );
        });
    }

    #[test]
    fn create_double_slash() {
        with_path_mut!("//", |path| {
            let result = File::create_dir_all(path);
            assert!(result.is_ok(), "Double slash should succeed");
        });
    }

    #[test]
    fn create_triple_slash() {
        with_path_mut!("///", |path| {
            let result = File::create_dir_all(path);
            assert!(result.is_ok(), "Triple slash should succeed");
        });
    }

    #[test]
    fn create_quadruple_slash() {
        with_path_mut!("////", |path| {
            let result = File::create_dir_all(path);
            assert!(result.is_ok(), "Quadruple slash should succeed");
        });
    }

    #[test]
    fn create_multiple_components() {
        with_path_mut!("/tmp/roc/test_dir", |path| {
            let result = File::create_dir_all(path);
            assert!(result.is_ok(), "Multiple components should succeed");
        });
    }

    #[test]
    fn create_multiple_components_with_trailing_slash() {
        with_path_mut!("/tmp/roc/test_dir/", |path| {
            let result;

            #[cfg(unix)]
            {
                result = File::create_dir_all_unix::<FakeUnixFs>(path);
            }

            #[cfg(windows)]
            {
                result = File::create_dir_windows::<FakeWindowsFs>(path);
            }

            assert!(
                result.is_ok(),
                "Multiple components with trailing slash should succeed"
            );

            assert_eq!(
                &FakeUnixFs::get_mkdir_calls(),
                &[
                    UnixMkdirCall {
                        path: "/tmp".to_string(),
                        mode: CACHE_DIR_MODE,
                        ret: 0
                    },
                    UnixMkdirCall {
                        path: "/tmp/roc".to_string(),
                        mode: CACHE_DIR_MODE,
                        ret: 0
                    },
                    UnixMkdirCall {
                        path: "/tmp/roc/test_dir".to_string(),
                        mode: CACHE_DIR_MODE,
                        ret: 0
                    }
                ]
            );
        });
    }
}
