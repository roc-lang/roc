//! Given canonical IR and type info, evaluate it until it is a primitive canonical IR node.
const std = @import("std");
const CIR = @import("../check/canonicalize/CIR.zig");
const stack = @import("stack.zig");

pub const EvalError = error{
    Crash,
    OutOfMemory,
};

/// Evaluates the canonical IR expression node until it becomes a primitive CIR node,
/// or returns an error if it encounters a crash.
pub fn eval(allocator: std.mem.Allocator, cir: *CIR, expr_idx: CIR.Expr.Idx) EvalError!CIR.Expr.Idx {
    _ = allocator;

    const expr = cir.store.getExpr(expr_idx);

    // Check if the expression is already a primitive
    switch (expr) {
        // Numeric literals are primitives
        .num, .int, .float => return expr_idx,

        // String literals are primitives
        .str, .str_segment => return expr_idx,

        // Character literals are primitives
        .single_quote => return expr_idx,

        // Empty record is a primitive
        .empty_record => return expr_idx,

        // Zero-argument tags are primitives
        .zero_argument_tag => return expr_idx,

        // Runtime errors should return an error
        .runtime_error => return error.Crash,

        // Non-primitive expressions need evaluation (TODO: implement these)
        .lookup, .list, .when, .@"if", .call, .record, .record_access, .tag, .binop => {
            // For now, these are not implemented
            return expr_idx;
        },
    }
}

test {
    _ = @import("eval_test.zig");
}
