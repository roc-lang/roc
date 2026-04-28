//! Lambda-solved MIR.

const std = @import("std");

pub const Type = @import("type.zig");
pub const Ast = @import("ast.zig");

test "lambda_solved tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Type);
    std.testing.refAllDecls(Ast);
}
