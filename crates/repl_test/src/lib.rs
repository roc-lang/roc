//! Tests the roc REPL.
#[allow(unused_imports)]
#[macro_use]
extern crate lazy_static;

#[cfg(test)]
mod tests;

#[cfg(test)]
mod state;

#[cfg(all(test, not(feature = "wasm")))]
mod cli;

#[cfg(all(test, feature = "wasm"))]
mod wasm;
