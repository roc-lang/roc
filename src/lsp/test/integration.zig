//! LSP integration test root.
//!
//! This root intentionally runs the compiler-backed LSP paths. It is expected
//! to be slower than unit.zig because these tests create SyntaxChecker,
//! BuildEnv, real Roc source files, compiled builtins, and platform/app checking
//! state.
//!
//! Completion, hover, definition, document symbol, document highlight, parse
//! error, and diagnostic behavior that depends on checked Roc modules belongs
//! here. Keeping those tests separate makes the unit root a precise signal that
//! no compiler build environment is required.

const std = @import("std");

test "lsp integration tests" {
    std.testing.refAllDecls(@import("syntax_test.zig"));
    std.testing.refAllDecls(@import("parse_error_test.zig"));
    std.testing.refAllDecls(@import("handler_integration_tests.zig"));
}

