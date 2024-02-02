// This should be no_std, but we want to be able to use dbg! in development and std conveniences in testing
// Having this be no_std isn't strictly necessary, but it reduces the risk of accidental heap allocations.
#![cfg_attr(not(any(debug_assertions, test)), no_std)]

mod path;
pub use path::Path;

// UNIX modules

#[cfg(unix)]
mod file_unix;

#[cfg(unix)]
pub use file_unix::File;

#[cfg(unix)]
mod error_unix;

#[cfg(unix)]
pub use error_unix::IoError;

// Windows modules

#[cfg(windows)]
mod file_windows;

#[cfg(windows)]
pub use file_windows::File;

#[cfg(windows)]
mod error_windows;

#[cfg(windows)]
pub use error_windows::IoError;
