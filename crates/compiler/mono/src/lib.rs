//! Roc's main intermediate representation (IR), which is responsible for
//! [monomorphization](https://en.wikipedia.org/wiki/Monomorphization),
//! defunctionalization, inserting [ref-count](https://en.wikipedia.org/wiki/Reference_counting)
//! instructions, and transforming a Roc program into a form that is easy to
//! consume by a backend.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant, clippy::upper_case_acronyms)]

pub mod borrow;
pub mod code_gen_help;
pub mod inc_dec;
pub mod ir;
pub mod layout;
pub mod layout_soa;
pub mod low_level;
pub mod reset_reuse;
pub mod tail_recursion;

// Temporary, while we can build up test cases and optimize the exhaustiveness checking.
// For now, following this warning's advice will lead to nasty type inference errors.
//#[allow(clippy::ptr_arg)]
pub mod decision_tree;
