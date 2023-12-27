// This should not need anything from std (and if it does, that's a red flag
// that a design mistake has almost certainly been made), but it's nice to
// have dbg printing etc. in tests and in debug builds.
#![cfg_attr(not(any(test, debug_assertions)), no_std)]

mod arena;

pub use arena::{AllocErr, AllocOrInitError, Arena};
