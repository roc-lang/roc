#[derive(Debug, Clone, Copy)]
pub struct IoError(u32);

impl IoError {
    // https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes--0-499-
    pub const NOT_ENOUGH_MEMORY: Self = Self(8);

    // https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes--0-499-
    pub const ERROR_FILE_TOO_LARGE: Self = Self(233);

    pub fn write(&self, buf: &mut [u16]) -> usize {
        use core::ffi::c_void;

        // https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-formatmessagew
        extern "system" {
            fn FormatMessageW(
                dwFlags: u32,
                lpSource: *const c_void,
                dwMessageId: u32,
                dwLanguageId: u32,
                lpBuffer: *mut u16,
                nSize: u32,
                Arguments: *mut c_void, // We won't use this, so leave it as c_void0
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
            Err(IoError(error::last_error()))
        }
    }

    pub fn most_recent() -> Self {
        extern "system" {
            fn GetLastError() -> u32;
        }

        Self(unsafe { GetLastError() })
    }
}
