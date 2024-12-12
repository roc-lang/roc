#![cfg_attr(not(any(debug_assertions, test)), no_std)]

mod either_index;
mod soa_index;
mod soa_slice;
mod soa_slice2;
mod soa_slice3;

pub use either_index::*;
pub use soa_index::*;
pub use soa_slice::{NonEmptySlice, PairSlice, Slice};
pub use soa_slice2::Slice2;
pub use soa_slice3::Slice3;
