// Both of these limits are needed only for parser, and only for release builds.
// The parser functions that use #[cfg(not(debug_assertions))] are the
// ones that need this. They make builds take a lot longer, but they are more
// efficient than the alternative implementations we use in development.
//
// See https://bodil.lol/parser-combinators for more information; the parser
// is based on her design.
#![type_length_limit = "4343503439"]
#![recursion_limit = "128"]

pub mod can;
pub mod collections;
pub mod graph;
pub mod ident;
pub mod operator;
pub mod parse;
pub mod region;

pub mod string;

pub mod constrain;
pub mod ena;
pub mod fmt;
pub mod infer;
pub mod pretty_print_types;
pub mod solve;
pub mod subs;
pub mod types;
pub mod unify;

pub mod gen;

extern crate bumpalo;
extern crate fraction;
extern crate fxhash;
extern crate im_rc;
extern crate inkwell;
extern crate num;

#[macro_use]
extern crate log;
