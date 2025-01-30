//! [Canonicalize](https://en.wikipedia.org/wiki/Canonicalization) a roc
//! [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree),
//! [resolving symbols](https://stackoverflow.com/a/1175493/4200103),
//! [re-ordering definitions](https://www.oreilly.com/library/view/c-high-performance/9781787120952/546b5677-9157-4333-bc90-16db696436ac.xhtml),
//! and preparing a module for [type inference](https://en.wikipedia.org/wiki/Type_inference).
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod abilities;
pub mod annotation;
pub mod builtins;
pub mod constraint;
pub mod copy;
pub mod def;
mod derive;
pub mod desugar;
pub mod effect_module;
pub mod env;
pub mod exhaustive;
pub mod expected;
pub mod expr;
pub mod module;
pub mod num;
pub mod pattern;
pub mod procedure;
pub mod scope;
pub mod traverse;

pub use derive::DERIVED_REGION;

pub mod debug;
