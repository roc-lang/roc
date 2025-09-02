//! Roc code formatter module - provides formatting functionality for Roc source code.
//! This module re-exports the main formatting API from fmt.zig.

const std = @import("std");
const fmt = @import("fmt.zig");

pub const formatAst = fmt.formatAst;
pub const formatAstWithTokens = fmt.formatAstWithTokens;

test "fmt tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("fmt.zig"));
    std.testing.refAllDecls(@import("fmt.zig"));
}
