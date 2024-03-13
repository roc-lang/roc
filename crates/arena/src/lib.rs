// This should be no_std, but we want to be able to use dbg! in development and std conveniences in testing
// Having this be no_std isn't strictly necessary, but it reduces the risk of accidental heap allocations.
#![cfg_attr(not(any(debug_assertions, test)), no_std)]

mod alloc;
mod arena;
mod arena_ref;
mod soa;
mod string;
mod vec;

pub use crate::arena::*;
pub use crate::arena_ref::*;
pub use crate::soa::*;
pub use crate::string::*;
pub use crate::vec::*;
