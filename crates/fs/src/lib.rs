#![cfg_attr(not(any(debug_assertions, test)), no_std)]

mod file;
mod native_path;

pub use file::*;
pub use native_path::*;
