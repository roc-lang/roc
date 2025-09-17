//! Traversing raw source bytes to produce tokens and spans while validating UTF-8.

const std = @import("std");

pub const Token = @import("Token.zig");
pub const Scanner = @import("Bitmasks.zig");

test {
    std.testing.refAllDecls(@import("Bitmasks.zig"));
}
