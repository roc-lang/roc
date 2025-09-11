//! Scanning raw source bytes to produce tokens, regions, hashes, and UTF-8 validation.

const std = @import("std");

pub const Token = @import("Token.zig");
pub const Scanner = @import("Scanner.zig");

test "scanner tests" {
    std.testing.refAllDecls(@import("Scanner.zig"));
}
