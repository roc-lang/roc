// This should be no_std, but we want to be able to use dbg! in development and std conveniences in testing
// Having this be no_std isn't strictly necessary, but it reduces the risk of accidental heap allocations.
#![cfg_attr(not(any(debug_assertions, test)), no_std)]

/// Prints the given strings (or statically displayable things) to stderr, and exits the program with a failure exit code.
#[macro_export]
macro_rules! compiler_bug {
    // Accept one or more expressions, with an optional trailing comma
    ($($e:expr),* $(,)?) => {{
        // Use the default exit code of 9 (indicating a compiler bug)
        $crate::crash(9, concat!("A bug in the Roc compiler caused it to crash. The problem was:\n\n", $($e,)*, "\n\nIf you have time, it would be great if you could report this bug to <https://github.com/roc-lang/roc/issues>"))
    }};
}

#[macro_export]
macro_rules! unrecoverable {
    // Accept one or more expressions, with an optional trailing comma
    ($exit_code:expr, $($e:expr),* $(,)?) => {{
        // Use the default exit code of 9 (indicating a compiler bug)
        $crate::crash($exit_code, $($e,)*)
    }};
}

/// Writing to stderr can fail. If that happens, we exit with this code so at least there's some way to tell why the program exited
/// without printing anything: it was because the attempt to print something failed.
#[cfg(not(wasm32))]
const ERROR_WHILE_TRYING_TO_CRASH: i32 = 111;

/// Prints the given string to stderr and exits the process with the given exit code.
/// (On wasm, calls a primitive called `crash` passing the message. Exit code is ignored on wasm.)
pub fn crash(exit_code: i32, message: &str) -> ! {
    #[cfg(unix)]
    {
        extern "C" {
            fn write(fd: i32, buf: *const u8, len: usize) -> isize;
            fn exit(status: i32) -> !;
        }

        const FD_STDERR: i32 = 2;

        unsafe {
            let bytes_written = write(FD_STDERR, message.as_ptr(), message.len());

            exit(
                if bytes_written == -1 || (bytes_written as usize) < message.len() {
                    // Somehow we didn't successfully print the entire message to stderr.
                    // Return a different exit code to communicate this.
                    ERROR_WHILE_TRYING_TO_CRASH
                } else {
                    exit_code
                },
            );
        }
    }

    // On Windows we set the code page to UTF-8 before printing. Alternatively, we could try to convert the
    // UTF-8 string to UTF-16, but this requires either doing a heap allocation in userspace or else doing
    // a fixed-size stack allocation, both of which can fail (or result in a stack overflow, or not enough
    // space being available to print the entire message), and since we're about to exit, the priority here
    // is to minimize the chances that exiting with an error fails to print the message.
    #[cfg(windows)]
    {
        extern "system" {
            fn SetConsoleOutputCP(wCodePageID: u32) -> i32;
            fn GetStdHandle(nStdHandle: i32) -> *mut u8;
            fn WriteFile(
                hFile: *mut u8,
                lpBuffer: *const u8,
                nNumberOfBytesToWrite: u32,
                lpNumberOfBytesWritten: *mut u32,
                lpOverlapped: *mut u8,
            ) -> i32;
            fn ExitProcess(uExitCode: i32) -> !;
        }

        const STD_ERROR_HANDLE: i32 = -12;
        const CP_UTF8: u32 = 65001; // The UTF-8 code page, so UTF-8 chars can be written to the console handle directly.

        unsafe {
            let stderr_handle = unsafe { GetStdHandle(STD_ERROR_HANDLE) };

            if stderr_handle.is_null() || SetConsoleOutputCP(CP_UTF8) == 0 {
                // We couldn't get the stderr handle, or we couldn't set the code page to UTF-8.
                // Both of those are necessary in order to print this message to stderr, so exit now.
                ExitProcess(ERROR_WHILE_TRYING_TO_CRASH);
            }

            // Write message to stderr and exit
            let mut bytes_written = core::mem::MaybeUninit::uninit();

            WriteFile(
                stderr_handle,
                message.as_ptr(),
                message.len() as u32,
                bytes_written.as_mut_ptr(),
                core::ptr::null_mut(),
            );

            let bytes_written = bytes_written.assume_init() as usize;

            ExitProcess(if bytes_written < message.len() {
                // Somehow we didn't successfully print the entire message to stderr.
                // Return a different exit code to communicate this.
                ERROR_WHILE_TRYING_TO_CRASH
            } else {
                exit_code
            });
        }
    }

    #[cfg(wasm32)]
    {
        extern "C" {
            // Assume we've defined this primitive function, which is up to the wasm wrapper to interpret.
            fn crash(message_ptr: *const u8, message_len: usize) -> !;
        }

        // We don't use exit code in wasm; this just prevents an unused warning.
        let _ = exit_code;

        unsafe {
            crash(message.as_ptr(), message.len());
        }
    }
}
