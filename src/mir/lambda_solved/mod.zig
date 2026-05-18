//! Lambda-solved MIR.

const std = @import("std");

pub const Type = @import("type.zig");
pub const Ast = @import("ast.zig");
pub const Representation = @import("representation.zig");
pub const Solve = @import("solve.zig");

test "lambda_solved tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(Type);
    std.testing.refAllDecls(Ast);
    std.testing.refAllDecls(Representation);
    std.testing.refAllDecls(Solve);
}
