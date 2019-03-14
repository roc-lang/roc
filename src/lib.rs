#![feature(box_syntax, box_patterns)]

// pub mod unify;
// pub mod interpret;
// pub mod repl;

pub mod solve;
pub mod expr;
pub mod constrain;
pub mod canonical;
pub mod name;
pub mod typ;
pub mod parse;
mod ena;

#[macro_use]
extern crate log;

#[cfg(feature = "persistent")]
extern crate dogged;

#[macro_use] extern crate combine;
