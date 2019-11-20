// TODO reduce this limit by replacing more parser combinator functions with
// macros. It's the way these functions (but not the macros) interact that
//
// See https://bodil.lol/parser-combinators for more information; the parser
// is based on her design./ causes the need for higher limits.
#![type_length_limit = "3735510"]

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
