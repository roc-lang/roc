const std = @import("std");

test "base tests" {
    std.testing.refAllDecls(@import("Ident.zig"));
}
