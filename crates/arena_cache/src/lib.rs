// This should be no_std when the "io" feature is disabled, but we want to be able to use dbg!
// in development and std conveniences in testing. Having this be no_std isn't strictly necessary,
// but it reduces the risk of accidental heap allocations.
#![cfg_attr(not(any(feature = "io", debug_assertions, test)), no_std)]

pub mod arena;
