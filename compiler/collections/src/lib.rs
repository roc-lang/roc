#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

pub mod all;
mod known_size_iterator;
mod reference_matrix;
mod small_string_interner;
pub mod soa;
mod vec_map;
mod vec_set;

pub use all::{default_hasher, BumpMap, ImEntry, ImMap, ImSet, MutMap, MutSet, SendMap};
pub use known_size_iterator::KnownSizeIterator;
pub use reference_matrix::{ReferenceMatrix, Sccs};
pub use small_string_interner::SmallStringInterner;
pub use vec_map::VecMap;
pub use vec_set::VecSet;
