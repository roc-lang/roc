//! LSP integration spec root.
//!
//! This root intentionally lists the compiler-backed LSP specs consumed by the
//! parallel integration harness. It is expected to be slower than unit.zig
//! because these specs create SyntaxChecker, BuildEnv, real Roc source files,
//! compiled builtins, and platform/app checking state.
//!
//! Completion, hover, definition, document symbol, document highlight, parse
//! error, and diagnostic behavior that depends on checked Roc modules belongs
//! here. Keeping those tests separate makes the unit root a precise signal that
//! no compiler build environment is required.

pub const integration_spec = @import("integration_spec.zig");
pub const integration_env = @import("integration_env.zig");
const syntax_tests = @import("syntax_test.zig");
const parse_error_tests = @import("parse_error_test.zig");
const handler_integration_tests = @import("handler_integration_tests.zig");

pub const Spec = integration_spec.Spec;
pub const SpecError = integration_spec.SpecError;
pub const specs = parse_error_tests.specs ++ syntax_tests.specs ++ handler_integration_tests.specs;
