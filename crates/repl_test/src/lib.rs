#[allow(unused_imports)]
#[macro_use]
extern crate lazy_static;

#[cfg(test)]
mod tests;

#[cfg(all(test, not(feature = "wasm")))]
mod cli;

#[cfg(all(test, feature = "wasm"))]
mod wasm;
