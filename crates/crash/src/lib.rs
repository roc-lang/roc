/// Takes a &str on UNIX (U16Str on Windows), prints it to stderr, and exits the program with a failure exit code.
#[macro_export]
macro_rules! unrecoverable {
    ($message:expr, $exit_code:expr) => {
        $crate::crash($exit_code as _, $message)
    };
    ($message:expr) => {
        unrecoverable!($message, 9 as _); // Default exit code for crashes
    };
}

/// Prints the given string to stderr and exits the program with a failure exit code.
#[cfg(unix)]
pub fn crash(exit_code: i32, message: &str) -> ! {
    extern "C" {
        fn write(fd: i32, buf: *const u8, len: usize) -> isize;
        fn exit(status: i32) -> !;
    }

    const FD_STDERR: i32 = 2;

    unsafe {
        write(FD_STDERR, message.as_ptr(), message.len());
        exit(exit_code);
    }
}

#[cfg(windows)]
pub unsafe fn crash(exit_code: i32, message: &widestring::U16Str) -> ! {
    // Allocation failed. We realistically can't recover from this, so print OOM and exit.
    extern "system" {
        fn GetStdHandle(nStdHandle: i32) -> *mut u8;
        fn WriteFile(
            hFile: *mut u8,
            lpBuffer: *const u8,
            nNumberOfBytesToWrite: u32,
            lpNumberOfBytesWritten: *mut u32,
            lpOverlapped: *mut u8,
        ) -> i32;
        fn ExitProcess(uExitCode: u32) -> !;
    }

    const STD_ERROR_HANDLE: i32 = -12;

    unsafe {
        use core::mem::MaybeUninit;

        // Write OOM_MESSAGE to stderr and exit
        let mut bytes_written = MaybeUninit::uninit();

        WriteFile(
            GetStdHandle(STD_ERROR_HANDLE),
            message.as_ptr(),
            // Multiply by 2 because this length is in u16 units, but this should be in bytes
            (message.len() * 2).min(u32::MAX as usize) as u32,
            bytes_written.as_mut_ptr(),
            core::ptr::null_mut(),
        );

        ExitProcess(exit_code);
    }
}

#[cfg(wasm32)]
fn crash(_exit_code: i32, ptr: *const u8, len: usize) -> ! {
    extern "C" {
        fn write(fd: i32, buf: *const u8, len: usize) -> isize;
        fn exit(status: i32) -> !;
    }

    const FD_STDERR: i32 = 2;

    unsafe {
        write(FD_STDERR, ptr, len);
        exit(exit_code);
    }
}
