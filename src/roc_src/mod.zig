//! Working with Roc source code - whether in files or individual strings.
const std = @import("std");

pub const Span = @import("Span.zig");
pub const Lines = @import("Lines.zig");
pub const Bytes = [:'\n']align(16) u8;

test "roc_src tests" {
    std.testing.refAllDecls(@import("Lines.zig"));
}
