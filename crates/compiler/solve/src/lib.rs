//! The entry point of Roc's [type inference](https://en.wikipedia.org/wiki/Type_inference)
//! system. Implements type inference and specialization of abilities.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
// TODO to be removed
#![allow(clippy::too_many_arguments)]

pub mod ability;
pub mod module;
pub mod solve;
pub mod specialize;

mod aliases;
mod deep_copy;
mod env;
mod kinds;
mod pools;
mod to_var;

pub use aliases::Aliases;
pub use env::{DerivedEnv, InferenceEnv, SolveEnv};
pub use kinds::FunctionKind;
pub use pools::Pools;
pub use to_var::type_to_var;
