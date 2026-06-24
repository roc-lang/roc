//! Roc code formatter module - provides formatting functionality for Roc source code.
//! This module re-exports the main formatting API from fmt.zig.

const std = @import("std");
const fmt = @import("fmt.zig");

pub const FormattingResult = fmt.FormattingResult;
pub const FormatAstError = fmt.FormatAstError;
pub const FormatFileError = fmt.FormatFileError;
pub const FormatPathError = fmt.FormatPathError;
pub const FormatStdinError = fmt.FormatStdinError;
pub const FormatParseError = fmt.FormatParseError;
pub const FormatTestError = fmt.FormatTestError;
pub const formatPath = fmt.formatPath;
pub const formatFilePath = fmt.formatFilePath;
pub const formatStdin = fmt.formatStdin;
pub const formatAst = fmt.formatAst;
pub const formatHeader = fmt.formatHeader;
pub const formatStatement = fmt.formatStatement;
pub const formatExpr = fmt.formatExpr;
pub const moduleFmtsStable = fmt.moduleFmtsStable;

test "fmt tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("fmt.zig"));
}
