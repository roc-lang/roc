#![cfg_attr(not(any(debug_assertions, test)), no_std)]

mod alloc;
mod arena;
mod byte_writer;
mod sized_vec;
mod vec;

#[cfg(unix)]
mod unix;

#[cfg(windows)]
mod windows;

#[cfg(target_family = "wasm")]
mod wasm;

pub use arena::Arena;
pub use sized_vec::SizedVec;
pub use vec::{BigVec, Vec};
