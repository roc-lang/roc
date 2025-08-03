//! Simplified evaluator for use in the shim
//! This avoids import conflicts by being a standalone module

const std = @import("std");
const compile = @import("compile");
const base = @import("base");

const ModuleEnv = compile.ModuleEnv;
const Expr = compile.Expr;
const Node = compile.Node;

pub const EvalResult = struct {
    value: i128,
    is_error: bool = false,
};

/// Simple evaluation function for basic arithmetic expressions
pub fn evalExpr(env: *ModuleEnv, expr_idx: u32) !EvalResult {
    const expr_idx_enum: Expr.Idx = @enumFromInt(expr_idx);
    const expr = env.store.getExpr(expr_idx_enum);
    
    switch (expr) {
        .e_int => |int_lit| {
            // Get the integer value from the literal
            return EvalResult{ .value = int_lit.value.toI128() };
        },
        .e_binop => |binop| {
            // Recursively evaluate left and right operands
            const left_result = try evalExpr(env, @intFromEnum(binop.lhs));
            const right_result = try evalExpr(env, @intFromEnum(binop.rhs));
            
            if (left_result.is_error or right_result.is_error) {
                return EvalResult{ .value = 0, .is_error = true };
            }
            
            // Apply the operation
            const result_value = switch (binop.op) {
                .add => left_result.value + right_result.value,
                .sub => left_result.value - right_result.value,
                .mul => left_result.value * right_result.value,
                .div => if (right_result.value != 0) 
                    @divTrunc(left_result.value, right_result.value) 
                else 
                    return EvalResult{ .value = 0, .is_error = true },
                else => return error.UnsupportedOperation,
            };
            
            return EvalResult{ .value = result_value };
        },
        else => {
            // For now, only support integer literals and basic binary operations
            return error.UnsupportedExpression;
        }
    }
}