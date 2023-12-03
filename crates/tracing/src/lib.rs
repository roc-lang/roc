//! Utilities for turning on tracing in user-facing or test executables of the Roc compiler.
//!
//! Tracing is controlled with the ROC_LOG environment variable.
//! If ROC_LOG is specified, logs are written to stderr. If ROC_LOGTO=<filepath> is also specified,
//! logs are instead written to <filepath>.
//!
//! See [directive-syntax] for the filtering directive syntax.
//!
//! Rather than using the Rust `tracing` crate (or any other tracing crate) directly,
//! you should use the exposed members of `roc_tracing` for your tracing needs.
//! This enables us to easily modify the tracing infrastructure without inducing sweeping changes.
//!
//! Tracing is only turned on in debug builds. Use the provided [setup_tracing] macro to turn on
//! tracing at an executable's entry point.
//!
//! [directive-syntax]: https://docs.rs/tracing-subscriber/latest/tracing_subscriber/filter/struct.EnvFilter.html#directives

/// Sets up tracing of a Roc executable. The value of this macro must be bound to a variable that
/// is not dropped until tracing has completed.
///
/// This macro should only be invoked at an executable's entry point.
/// Tracing will only be enabled in debug builds.
#[macro_export]
macro_rules! setup_tracing {
    () => {
        if cfg!(debug_assertions) {
            $crate::setup_tracing()
        } else {
            $crate::TracingGuards::NONE
        }
    };
}

pub use tracing::debug;
pub use tracing::info;

const ENV_FILTER: &str = "ROC_LOG";
const LOGTO_VAR: &str = "ROC_LOGTO";

use tracing_subscriber::{fmt, prelude::*, EnvFilter, Layer, Registry};

/// Guards issued by the underlying library used for tracing.
/// Must not be dropped until all tracing is complete.
pub struct TracingGuards {
    _file_appender_guard: Option<tracing_appender::non_blocking::WorkerGuard>,
}

impl TracingGuards {
    pub const NONE: TracingGuards = TracingGuards {
        _file_appender_guard: None,
    };
}

#[must_use]
pub fn setup_tracing() -> TracingGuards {
    if let Ok(file) = std::env::var(LOGTO_VAR) {
        let _ = std::fs::remove_file(&file);
        let file_appender = tracing_appender::rolling::never(".", file);
        let (non_blocking, guard) = tracing_appender::non_blocking(file_appender);
        let file_layer = fmt::Layer::default()
            .with_writer(non_blocking)
            .with_ansi(false)
            .with_filter(EnvFilter::from_env(ENV_FILTER));

        Registry::default().with(file_layer).init();

        TracingGuards {
            _file_appender_guard: Some(guard),
        }
    } else {
        let stderr_layer = fmt::Layer::default()
            .with_writer(std::io::stderr)
            .with_filter(EnvFilter::from_env(ENV_FILTER));

        Registry::default().with(stderr_layer).init();

        TracingGuards::NONE
    }
}
