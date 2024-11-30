//! Implements the Roc parser, which transforms a textual representation of a
//! Roc program to an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

#[macro_use]
pub mod parser;
pub mod ast;
pub mod blankspace;
pub mod expr;
pub mod header;
pub mod highlight;
pub mod ident;
pub mod keyword;
pub mod normalize;
pub mod number_literal;
pub mod pattern;
pub mod src64;
pub mod state;
pub mod string_literal;
pub mod test_helpers;
pub mod type_annotation;
