//! Executable MIR.

const std = @import("std");

pub const Type = @import("type.zig");
pub const Ast = @import("ast.zig");
pub const Layouts = @import("layouts.zig");
pub const Build = @import("build.zig");

test "executable tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Type);
    std.testing.refAllDecls(Ast);
    std.testing.refAllDecls(Layouts);
    std.testing.refAllDecls(Build);
}
