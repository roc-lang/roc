//! Cor-style post-check pipeline from checked modules to LIR.

const std = @import("std");

pub const Common = @import("common.zig");
pub const Monotype = struct {
    pub const Ast = @import("monotype/ast.zig");
    pub const Type = @import("monotype/type.zig");
    pub const Lower = @import("monotype/lower.zig");
    pub const Specialize = @import("monotype/specialize.zig");
};
pub const MonotypeLifted = struct {
    pub const Ast = @import("monotype_lifted/ast.zig");
    pub const Lift = @import("monotype_lifted/lift.zig");
};
pub const LambdaSolved = struct {
    pub const Ast = @import("lambda_solved/ast.zig");
    pub const Type = @import("lambda_solved/type.zig");
    pub const Solve = @import("lambda_solved/solve.zig");
};
pub const LambdaMono = struct {
    pub const Ast = @import("lambda_mono/ast.zig");
    pub const Type = @import("lambda_mono/type.zig");
    pub const Lower = @import("lambda_mono/lower.zig");
    pub const Specialize = @import("lambda_mono/specialize.zig");
};
pub const LirLower = @import("lir_lower.zig");

test "postcheck declarations are referenced" {
    std.testing.refAllDecls(@This());
}
