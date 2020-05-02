#![warn(clippy::all, clippy::dbg_macro)]
// I'm skeptical that clippy:large_enum_variant is a good lint to have globally enabled.
//
// It warns about a performance problem where the only quick remediation is
// to allocate more on the heap, which has lots of tradeoffs - including making it
// long-term unclear which allocations *need* to happen for compilation's sake
// (e.g. recursive structures) versus those which were only added to appease clippy.
//
// Effectively optimizing data struture memory layout isn't a quick fix,
// and encouraging shortcuts here creates bad incentives. I would rather temporarily
// re-enable this when working on performance optimizations than have it block PRs.
#![allow(clippy::large_enum_variant)]

#[macro_use]
pub mod parser;
pub mod ast;
pub mod blankspace;
pub mod expr;
pub mod header;
pub mod ident;
pub mod keyword;
pub mod module;
pub mod number_literal;
pub mod pattern;
pub mod problems;
pub mod string_literal;
pub mod type_annotation;
