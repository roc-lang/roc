#![allow(dead_code)]
#![allow(clippy::too_many_arguments)]

#[macro_use]
mod util;

mod analyze;
mod api;
mod bindings;
mod ir;
mod name_cache;
mod preprocess;
mod render_api_ir;
mod type_cache;

pub use api::*;
