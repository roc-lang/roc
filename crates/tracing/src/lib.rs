//! Utilities for turning on tracing in user-facing or test executables of the Roc compiler.
//!
//! Tracing always writes to stderr, and is controlled with the ROC_LOG environment variable.
//!
//! Rather than using the Rust `tracing` crate (or any other tracing crate) directly,
//! you should use the exposed members of `roc_tracing` for your tracing needs.
//! This enables us to easily modify the tracing infrastructure without inducing sweeping changes.
//!
//! Tracing is only turned on in debug builds. Use the provided [setup_tracing] macro to turn on
//! tracing at an executable's entry point.

/// Sets up tracing of a Roc executable.
///
/// This macro should only be invoked at an executable's entry point.
/// Tracing will only be enabled in debug builds.
/// Tracing is controlled with the `ROC_LOG` environment variable. See [directive-syntax] for the filtering directive syntax.
///
/// [directive-syntax]: https://docs.rs/tracing-subscriber/latest/tracing_subscriber/filter/struct.EnvFilter.html#directives
#[macro_export]
macro_rules! setup_tracing {
    () => {
        if cfg!(debug_assertions) {
            $crate::setup_tracing();
        }
    };
}

pub use tracing::debug;
pub use tracing::info;

const ENV_FILTER: &str = "ROC_LOG";

use tracing_subscriber::{fmt, prelude::*, EnvFilter, Layer, Registry};

#[doc(hidden)]
pub fn setup_tracing() {
    let stderr_layer = fmt::Layer::default()
        .with_writer(std::io::stderr)
        .with_filter(EnvFilter::from_env(ENV_FILTER));

    Registry::default().with(stderr_layer).init();
}
