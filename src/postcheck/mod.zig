//! Cor-style post-check pipeline from checked modules to LIR.

const std = @import("std");

/// Shared ids, inputs, and invariants for post-check stages.
pub const Common = @import("common.zig");
/// Closed source-shape IR after checking has removed dispatch syntax.
pub const Monotype = struct {
    pub const Ast = @import("monotype/ast.zig");
    pub const Type = @import("monotype/type.zig");
    pub const Lower = @import("monotype/lower.zig");
    pub const Serialize = @import("monotype/serialize.zig");
    pub const Solve = @import("monotype/solve.zig");
    pub const Specialize = @import("monotype/specialize.zig");
};
/// Monotype IR after nested function bodies are lifted.
pub const MonotypeLifted = struct {
    pub const Ast = @import("monotype_lifted/ast.zig");
    pub const Lift = @import("monotype_lifted/lift.zig");
    pub const SpecConstr = @import("monotype_lifted/spec_constr.zig");
};
/// Lifted IR with lambda-set relationships solved in the type store.
pub const LambdaSolved = struct {
    pub const Ast = @import("lambda_solved/ast.zig");
    pub const Type = @import("lambda_solved/type.zig");
    pub const Solve = @import("lambda_solved/solve.zig");
};
/// Lambda-solved IR after function values have concrete runtime encodings.
pub const LambdaMono = struct {
    pub const Ast = @import("lambda_mono/ast.zig");
    pub const Type = @import("lambda_mono/type.zig");
    pub const Lower = @import("lambda_mono/lower.zig");
    pub const Specialize = @import("lambda_mono/specialize.zig");
    pub const Eval = @import("lambda_mono/eval.zig");
};
/// Decision-tree match compiler shared by both LIR lowerers.
pub const MatchTree = @import("match_tree.zig");
pub const SolvedInline = @import("solved_inline.zig");
pub const SolvedLirLower = @import("solved_lir_lower.zig");
pub const StructuralTest = @import("structural_test.zig");

test "postcheck declarations are referenced" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("common.zig"));
    std.testing.refAllDecls(@import("monotype/ast.zig"));
    std.testing.refAllDecls(@import("monotype/type.zig"));
    std.testing.refAllDecls(@import("monotype/lower.zig"));
    std.testing.refAllDecls(@import("monotype/serialize.zig"));
    std.testing.refAllDecls(@import("monotype/solve.zig"));
    std.testing.refAllDecls(@import("monotype/specialize.zig"));
    std.testing.refAllDecls(@import("monotype_lifted/ast.zig"));
    std.testing.refAllDecls(@import("monotype_lifted/lift.zig"));
    std.testing.refAllDecls(@import("monotype_lifted/spec_constr.zig"));
    std.testing.refAllDecls(@import("lambda_solved/ast.zig"));
    std.testing.refAllDecls(@import("lambda_solved/type.zig"));
    std.testing.refAllDecls(@import("lambda_solved/solve.zig"));
    std.testing.refAllDecls(@import("lambda_mono/ast.zig"));
    std.testing.refAllDecls(@import("lambda_mono/type.zig"));
    std.testing.refAllDecls(@import("lambda_mono/lower.zig"));
    std.testing.refAllDecls(@import("lambda_mono/specialize.zig"));
    std.testing.refAllDecls(@import("lambda_mono/eval.zig"));
    std.testing.refAllDecls(@import("match_tree.zig"));
    std.testing.refAllDecls(@import("solved_inline.zig"));
    std.testing.refAllDecls(@import("solved_lir_lower.zig"));
    std.testing.refAllDecls(@import("structural_test.zig"));
}
