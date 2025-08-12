const std = @import("std");
const fmt = @import("fmt.zig");

pub const FormattingResult = fmt.FormattingResult;
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
