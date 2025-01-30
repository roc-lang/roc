//! Roc's main intermediate representation (IR), which is responsible for
//! [monomorphization](https://en.wikipedia.org/wiki/Monomorphization),
//! defunctionalization, inserting [ref-count](https://en.wikipedia.org/wiki/Reference_counting)
//! instructions, and transforming a Roc program into a form that is easy to
//! consume by a backend.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant, clippy::upper_case_acronyms)]
// Not a useful lint for us
#![allow(clippy::too_many_arguments)]

pub mod borrow;
pub mod code_gen_help;
pub mod drop_specialization;
pub mod inc_dec;
pub mod ir;
pub mod layout;
pub mod low_level;
pub mod reset_reuse;
pub mod tail_recursion;

pub mod debug;
