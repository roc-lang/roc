#![cfg(not(doctest))]
// re-export all inkwell members. This way we can switch
// inkwell versions by making changes in just one place.
pub use inkwell::*;
