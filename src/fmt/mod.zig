//! Roc code formatter module - provides formatting functionality for Roc source code.
//! This module re-exports the main formatting API from fmt.zig and the new fmt2.zig.

const std = @import("std");
const fmt = @import("fmt.zig");
pub const fmt2 = @import("fmt2.zig");

pub const FormattingResult = fmt.FormattingResult;
pub const formatPath = fmt.formatPath;
pub const formatFilePath = fmt.formatFilePath;
pub const formatStdin = fmt.formatStdin;
pub const formatAst = fmt.formatAst;
pub const formatHeader = fmt.formatHeader;
pub const formatStatement = fmt.formatStatement;
pub const formatExpr = fmt.formatExpr;
pub const moduleFmtsStable = fmt.moduleFmtsStable;

// New formatter exports for AST2
pub const FormatOptions = fmt2.FormatOptions;
pub const formatAst2 = fmt2.formatAst;
pub const formatAst2WithOptions = fmt2.formatAstWithOptions;
// Legacy compatibility - these now require AST2
pub const formatSource = fmt2.formatSource;
pub const formatSourceWithOptions = fmt2.formatSourceWithOptions;

test "fmt tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("fmt.zig"));
    std.testing.refAllDecls(@import("fmt2.zig"));
}
