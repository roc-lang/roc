// This should be no_std, but we want to be able to use dbg! in development and std conveniences in testing
// Having this be no_std isn't strictly necessary, but it reduces the risk of accidental heap allocations.
#![cfg_attr(not(any(debug_assertions, test)), no_std)]

mod file;
mod path;
pub use crate::file::{FileMetadata, OpenFile, ReadFile, WriteFile};
pub use crate::path::Path;

// UNIX modules

#[cfg(unix)]
mod file_unix;

#[cfg(unix)]
pub use crate::file_unix::Fd;

#[cfg(unix)]
mod error_unix;

#[cfg(unix)]
pub use crate::error_unix::IoError;

// Windows modules

#[cfg(windows)]
mod file_windows;

#[cfg(windows)]
pub use crate::file_windows::Handle;

#[cfg(windows)]
mod error_windows;

#[cfg(windows)]
pub use crate::error_windows::IoError;
