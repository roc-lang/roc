#![cfg_attr(not(any(debug_assertions, test)), no_std)]

mod either_index;
mod soa_index;
mod soa_slice;

pub use either_index::*;
pub use soa_index::*;
pub use soa_slice::*;
