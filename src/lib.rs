pub mod expr;
pub mod parse;
pub mod parse_state;
pub mod parser;
// pub mod eval;
pub mod canonicalize;
pub mod collections;
pub mod graph;
pub mod operator;
pub mod region;

// pub mod string;

pub mod constrain;
pub mod ena;
pub mod infer;
pub mod pretty_print_types;
pub mod solve;
pub mod subs;
pub mod types;
pub mod unify;

extern crate bumpalo;
extern crate fraction;
extern crate fxhash;
extern crate im_rc;
extern crate num;

#[macro_use]
extern crate combine; // OBSOLETE
#[macro_use]
extern crate log;
