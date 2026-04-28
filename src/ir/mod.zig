//! Cor-style lowered executable IR.

const std = @import("std");

pub const Layout = @import("layout.zig");
pub const Ast = @import("ast.zig");

test "ir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Layout);
    std.testing.refAllDecls(Ast);
}
