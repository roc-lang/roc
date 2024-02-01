use crate::error_windows::IoError;
use crate::file::File;

#[derive(Debug)]
#[repr(transparent)]
pub struct NeverClosedFile {
    handle: *mut c_void,
}

#[derive(Debug)]
#[repr(transparent)]
pub struct SelfClosingFile {
    file: NeverClosedFile,
}

impl Drop for SelfClosingFile {
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

impl SelfClosingFile {
    pub fn handle(&mut self) -> *mut c_void {
        self.file.fd()
    }
}

impl NeverClosedFile {
    // https://docs.rs/winapi/latest/winapi/um/handleapi/constant.INVALID_HANDLE_VALUE.html
    const INVALID_HANDLE_VALUE: *mut u8 = -1isize as _;

    pub fn handle(&mut self) -> *mut c_void {
        self.handle
    }

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
            Err(IoError(error_unix::last_error()))
        }
    }

    fn metadata(&mut self) -> Result<FileInfo, IoError> {
        extern "system" {
            // https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfileinformationbyhandle
            fn GetFileInformationByHandle(handle: *mut u8, buf: *mut FileInfo) -> i32;
        }

        let mut buf = MaybeUninit::uninit();

        unsafe {
            if GetFileInformationByHandle(self.handle, buf.as_mut_ptr) != 0 {
                Ok(buf.assume_init())
            } else {
                Err(IoError::most_recent())
            }
        }
    }
}

impl File for NeverClosedFile {
    fn open_read(path: &CStr) -> Result<Self, IoError> {
        Self::open(path, Self::O_RDONLY)
    }

    fn read(&mut self, buf: &mut [u8]) -> Result<usize, IoError> {
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
                self.handle,
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

    fn size_on_disk(&mut self) -> Result<u64, IoError> {
        let info = self.metadata()?;
        let size: u64 = ((info.nFileSizeHigh as u64) << 8) | info.nFileIndexLow as u64;

        Ok(size as u64)
    }
}

impl File for SelfClosingFile {
    fn open_read(path: &CStr) -> Result<Self, IoError> {
        match NeverClosedFile::open_read(path) {
            Ok(file) => Ok(Self { file }),
            Err(err) => Err(err),
        }
    }

    fn read(&mut self, buf: &mut [u8]) -> Result<usize, IoError> {
        self.file.read(buf)
    }

    fn size_on_disk(&mut self) -> Result<u64, IoError> {
        self.file.size_on_disk()
    }
}

/// https://learn.microsoft.com/en-us/windows/win32/api/fileapi/ns-fileapi-by_handle_file_information
#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct FileInfo {
    pub dwFileAttributes: u32,
    pub ftCreationTime: FileTime,
    pub ftLastAccessTime: FileTime,
    pub ftLastWriteTime: FileTime,
    pub dwVolumeSerialNumber: u32,
    pub nFileSizeHigh: u32,
    pub nFileSizeLow: u32,
    pub nNumberOfLinks: u32,
    pub nFileIndexHigh: u32,
    pub nFileIndexLow: u32,
}

/// https://docs.rs/winapi/latest/winapi/shared/minwindef/struct.FILETIME.html
#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct FileTime {
    pub dwLowDateTime: u32,
    pub dwHighDateTime: u32,
}
