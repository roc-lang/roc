#![feature(box_patterns)]

// pub mod unify;
// pub mod interpret;
// pub mod repl;

pub mod solve;
mod expr;
mod constrain;
mod canonical;
mod name;
mod typ;
mod ena;

#[macro_use]
extern crate log;

#[cfg(feature = "persistent")]
extern crate dogged;

