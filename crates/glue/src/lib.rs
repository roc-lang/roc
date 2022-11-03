//! Generates code needed for platform hosts to communicate with Roc apps.
//! This tool is not necessary for writing a platform in another language,
//! however, it's a great convenience! Currently supports Rust platforms, and
//! the plan is to support any language via a plugin model.
pub mod enums;
pub mod load;
pub mod rust_glue;
pub mod structs;
pub mod types;

#[rustfmt::skip]
pub mod glue;

pub use load::generate;
