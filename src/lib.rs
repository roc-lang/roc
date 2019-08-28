pub mod expr;
pub mod parse;
pub mod parse_state;
// pub mod eval;
pub mod operator;
pub mod region;
pub mod canonicalize;
pub mod collections;
pub mod graph;

// pub mod string;

pub mod types;
pub mod subs;
pub mod constrain;
pub mod solve;
pub mod unify;
pub mod infer;
pub mod pretty_print_types;
pub mod ena;

extern crate im_rc;
extern crate fraction;
extern crate num;
extern crate fxhash;

#[macro_use] extern crate combine;
#[macro_use] extern crate log;
