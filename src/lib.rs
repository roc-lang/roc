#![feature(box_patterns)]

// pub mod unify;
// pub mod interpret;
// pub mod repl;

pub mod solve;
mod ena;

#[macro_use]
extern crate log;

#[cfg(feature = "persistent")]
extern crate dogged;

