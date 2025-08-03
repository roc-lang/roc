//! Simplified evaluator for use in the shim
//! This avoids import conflicts by being a standalone module

const std = @import("std");
const compile = @import("compile");
const base = @import("base");

const ModuleEnv = compile.ModuleEnv;
const Expr = compile.Expr;
const Node = compile.Node;

/// Result of evaluating a Roc expression, supporting multiple data types
pub const EvalResult = union(enum) {
    int: i128,
    float32: f32,
    float64: f64,
    string: []const u8,
    boolean: bool,
    error_result,

    pub fn isError(self: EvalResult) bool {
        return switch (self) {
            .error_result => true,
            else => false,
        };
    }
};

/// Simple evaluation function for basic arithmetic expressions
pub fn evalExpr(env: *ModuleEnv, expr_idx: u32) !EvalResult {
    const expr_idx_enum: Expr.Idx = @enumFromInt(expr_idx);
    const expr = env.store.getExpr(expr_idx_enum);

    switch (expr) {
        .e_int => |int_lit| {
            return EvalResult{ .int = int_lit.value.toI128() };
        },
        .e_frac_f32 => |float_lit| {
            return EvalResult{ .float32 = float_lit.value };
        },
        .e_frac_f64 => |float_lit| {
            return EvalResult{ .float64 = float_lit.value };
        },
        .e_str => |str| {
            // String expressions contain a span of segments
            // For simplicity, we'll handle single-segment strings
            if (str.span.span.len == 1) {
                const segment_idx: Expr.Idx = @enumFromInt(str.span.span.start);
                const segment_expr = env.store.getExpr(segment_idx);

                switch (segment_expr) {
                    .e_str_segment => |segment| {
                        // Get the actual string from the string literal store
                        const str_bytes = env.strings.get(segment.literal);
                        return EvalResult{ .string = str_bytes };
                    },
                    else => return error.UnsupportedExpression,
                }
            } else {
                // Multi-segment strings (interpolated) not supported yet
                return error.UnsupportedExpression;
            }
        },
        .e_str_segment => |segment| {
            // Direct string segment (shouldn't normally be evaluated alone)
            const str_bytes = env.strings.get(segment.literal);
            return EvalResult{ .string = str_bytes };
        },
        .e_binop => |binop| {
            // Recursively evaluate left and right operands
            const left_result = try evalExpr(env, @intFromEnum(binop.lhs));
            const right_result = try evalExpr(env, @intFromEnum(binop.rhs));

            if (left_result.isError() or right_result.isError()) {
                return EvalResult.error_result;
            }

            // Handle different combinations of types
            return evalBinaryOperation(binop.op, left_result, right_result);
        },
        else => {
            // Return error for unsupported expressions for now
            return error.UnsupportedExpression;
        },
    }
}

/// Evaluate binary operations between two results
fn evalBinaryOperation(op: Expr.Binop.Op, left: EvalResult, right: EvalResult) EvalResult {
    switch (op) {
        .add => return performAdd(left, right),
        .sub => return performSub(left, right),
        .mul => return performMul(left, right),
        .div => return performDiv(left, right),
        .rem => return performRem(left, right),
        .eq => return performEq(left, right),
        .ne => return performNeq(left, right),
        .lt => return performLt(left, right),
        .le => return performLte(left, right),
        .gt => return performGt(left, right),
        .ge => return performGte(left, right),
        .@"and" => return performAnd(left, right),
        .@"or" => return performOr(left, right),
        else => return EvalResult.error_result,
    }
}

/// Perform addition operation between two EvalResults
/// Supports type coercion following the hierarchy: int -> float32 -> float64
pub fn performAdd(left: EvalResult, right: EvalResult) EvalResult {
    switch (left) {
        .int => |l_val| switch (right) {
            .int => |r_val| return EvalResult{ .int = l_val + r_val },
            .float32 => |r_val| return EvalResult{ .float32 = @as(f32, @floatFromInt(l_val)) + r_val },
            .float64 => |r_val| return EvalResult{ .float64 = @as(f64, @floatFromInt(l_val)) + r_val },
            else => return EvalResult.error_result,
        },
        .float32 => |l_val| switch (right) {
            .int => |r_val| return EvalResult{ .float32 = l_val + @as(f32, @floatFromInt(r_val)) },
            .float32 => |r_val| return EvalResult{ .float32 = l_val + r_val },
            .float64 => |r_val| return EvalResult{ .float64 = @as(f64, l_val) + r_val },
            else => return EvalResult.error_result,
        },
        .float64 => |l_val| switch (right) {
            .int => |r_val| return EvalResult{ .float64 = l_val + @as(f64, @floatFromInt(r_val)) },
            .float32 => |r_val| return EvalResult{ .float64 = l_val + @as(f64, r_val) },
            .float64 => |r_val| return EvalResult{ .float64 = l_val + r_val },
            else => return EvalResult.error_result,
        },
        else => return EvalResult.error_result,
    }
}

/// Perform subtraction operation between two EvalResults
/// Supports type coercion following the hierarchy: int -> float32 -> float64
pub fn performSub(left: EvalResult, right: EvalResult) EvalResult {
    switch (left) {
        .int => |l_val| switch (right) {
            .int => |r_val| return EvalResult{ .int = l_val - r_val },
            .float32 => |r_val| return EvalResult{ .float32 = @as(f32, @floatFromInt(l_val)) - r_val },
            .float64 => |r_val| return EvalResult{ .float64 = @as(f64, @floatFromInt(l_val)) - r_val },
            else => return EvalResult.error_result,
        },
        .float32 => |l_val| switch (right) {
            .int => |r_val| return EvalResult{ .float32 = l_val - @as(f32, @floatFromInt(r_val)) },
            .float32 => |r_val| return EvalResult{ .float32 = l_val - r_val },
            .float64 => |r_val| return EvalResult{ .float64 = @as(f64, l_val) - r_val },
            else => return EvalResult.error_result,
        },
        .float64 => |l_val| switch (right) {
            .int => |r_val| return EvalResult{ .float64 = l_val - @as(f64, @floatFromInt(r_val)) },
            .float32 => |r_val| return EvalResult{ .float64 = l_val - @as(f64, r_val) },
            .float64 => |r_val| return EvalResult{ .float64 = l_val - r_val },
            else => return EvalResult.error_result,
        },
        else => return EvalResult.error_result,
    }
}

/// Perform multiplication operation between two EvalResults
/// Supports type coercion following the hierarchy: int -> float32 -> float64
pub fn performMul(left: EvalResult, right: EvalResult) EvalResult {
    switch (left) {
        .int => |l_val| switch (right) {
            .int => |r_val| return EvalResult{ .int = l_val * r_val },
            .float32 => |r_val| return EvalResult{ .float32 = @as(f32, @floatFromInt(l_val)) * r_val },
            .float64 => |r_val| return EvalResult{ .float64 = @as(f64, @floatFromInt(l_val)) * r_val },
            else => return EvalResult.error_result,
        },
        .float32 => |l_val| switch (right) {
            .int => |r_val| return EvalResult{ .float32 = l_val * @as(f32, @floatFromInt(r_val)) },
            .float32 => |r_val| return EvalResult{ .float32 = l_val * r_val },
            .float64 => |r_val| return EvalResult{ .float64 = @as(f64, l_val) * r_val },
            else => return EvalResult.error_result,
        },
        .float64 => |l_val| switch (right) {
            .int => |r_val| return EvalResult{ .float64 = l_val * @as(f64, @floatFromInt(r_val)) },
            .float32 => |r_val| return EvalResult{ .float64 = l_val * @as(f64, r_val) },
            .float64 => |r_val| return EvalResult{ .float64 = l_val * r_val },
            else => return EvalResult.error_result,
        },
        else => return EvalResult.error_result,
    }
}

/// Perform division operation between two EvalResults
/// Returns error_result for division by zero. Supports type coercion.
pub fn performDiv(left: EvalResult, right: EvalResult) EvalResult {
    switch (left) {
        .int => |l_val| switch (right) {
            .int => |r_val| {
                if (r_val == 0) return EvalResult.error_result;
                return EvalResult{ .int = @divTrunc(l_val, r_val) };
            },
            .float32 => |r_val| {
                if (r_val == 0.0) return EvalResult.error_result;
                return EvalResult{ .float32 = @as(f32, @floatFromInt(l_val)) / r_val };
            },
            .float64 => |r_val| {
                if (r_val == 0.0) return EvalResult.error_result;
                return EvalResult{ .float64 = @as(f64, @floatFromInt(l_val)) / r_val };
            },
            else => return EvalResult.error_result,
        },
        .float32 => |l_val| switch (right) {
            .int => |r_val| {
                if (r_val == 0) return EvalResult.error_result;
                return EvalResult{ .float32 = l_val / @as(f32, @floatFromInt(r_val)) };
            },
            .float32 => |r_val| {
                if (r_val == 0.0) return EvalResult.error_result;
                return EvalResult{ .float32 = l_val / r_val };
            },
            .float64 => |r_val| {
                if (r_val == 0.0) return EvalResult.error_result;
                return EvalResult{ .float64 = @as(f64, l_val) / r_val };
            },
            else => return EvalResult.error_result,
        },
        .float64 => |l_val| switch (right) {
            .int => |r_val| {
                if (r_val == 0) return EvalResult.error_result;
                return EvalResult{ .float64 = l_val / @as(f64, @floatFromInt(r_val)) };
            },
            .float32 => |r_val| {
                if (r_val == 0.0) return EvalResult.error_result;
                return EvalResult{ .float64 = l_val / @as(f64, r_val) };
            },
            .float64 => |r_val| {
                if (r_val == 0.0) return EvalResult.error_result;
                return EvalResult{ .float64 = l_val / r_val };
            },
            else => return EvalResult.error_result,
        },
        else => return EvalResult.error_result,
    }
}

/// Perform remainder (modulo) operation between two EvalResults
/// Only supports integer operands, returns error_result for non-integers or zero divisor
pub fn performRem(left: EvalResult, right: EvalResult) EvalResult {
    switch (left) {
        .int => |l_val| switch (right) {
            .int => |r_val| {
                if (r_val == 0) return EvalResult.error_result;
                return EvalResult{ .int = @rem(l_val, r_val) };
            },
            else => return EvalResult.error_result,
        },
        else => return EvalResult.error_result,
    }
}

/// Perform equality comparison between two EvalResults
/// Returns boolean result, supports comparison across compatible types
pub fn performEq(left: EvalResult, right: EvalResult) EvalResult {
    const equal = switch (left) {
        .int => |l_val| switch (right) {
            .int => |r_val| l_val == r_val,
            .float32 => |r_val| @as(f32, @floatFromInt(l_val)) == r_val,
            .float64 => |r_val| @as(f64, @floatFromInt(l_val)) == r_val,
            else => false,
        },
        .float32 => |l_val| switch (right) {
            .int => |r_val| l_val == @as(f32, @floatFromInt(r_val)),
            .float32 => |r_val| l_val == r_val,
            .float64 => |r_val| @as(f64, l_val) == r_val,
            else => false,
        },
        .float64 => |l_val| switch (right) {
            .int => |r_val| l_val == @as(f64, @floatFromInt(r_val)),
            .float32 => |r_val| l_val == @as(f64, r_val),
            .float64 => |r_val| l_val == r_val,
            else => false,
        },
        .boolean => |l_val| switch (right) {
            .boolean => |r_val| l_val == r_val,
            else => false,
        },
        .string => |l_val| switch (right) {
            .string => |r_val| std.mem.eql(u8, l_val, r_val),
            else => false,
        },
        else => false,
    };
    return EvalResult{ .boolean = equal };
}

/// Perform inequality comparison between two EvalResults
/// Returns the negation of equality comparison
pub fn performNeq(left: EvalResult, right: EvalResult) EvalResult {
    const eq_result = performEq(left, right);
    return EvalResult{ .boolean = !eq_result.boolean };
}

/// Perform less-than comparison between two EvalResults
/// Supports numeric types and string lexicographic comparison
pub fn performLt(left: EvalResult, right: EvalResult) EvalResult {
    const less_than = switch (left) {
        .int => |l_val| switch (right) {
            .int => |r_val| l_val < r_val,
            .float32 => |r_val| @as(f32, @floatFromInt(l_val)) < r_val,
            .float64 => |r_val| @as(f64, @floatFromInt(l_val)) < r_val,
            else => return EvalResult.error_result,
        },
        .float32 => |l_val| switch (right) {
            .int => |r_val| l_val < @as(f32, @floatFromInt(r_val)),
            .float32 => |r_val| l_val < r_val,
            .float64 => |r_val| @as(f64, l_val) < r_val,
            else => return EvalResult.error_result,
        },
        .float64 => |l_val| switch (right) {
            .int => |r_val| l_val < @as(f64, @floatFromInt(r_val)),
            .float32 => |r_val| l_val < @as(f64, r_val),
            .float64 => |r_val| l_val < r_val,
            else => return EvalResult.error_result,
        },
        .string => |l_val| switch (right) {
            .string => |r_val| std.mem.order(u8, l_val, r_val) == .lt,
            else => return EvalResult.error_result,
        },
        else => return EvalResult.error_result,
    };
    return EvalResult{ .boolean = less_than };
}

/// Perform less-than-or-equal comparison between two EvalResults
/// Supports numeric types and string lexicographic comparison
pub fn performLte(left: EvalResult, right: EvalResult) EvalResult {
    const less_than_or_equal = switch (left) {
        .int => |l_val| switch (right) {
            .int => |r_val| l_val <= r_val,
            .float32 => |r_val| @as(f32, @floatFromInt(l_val)) <= r_val,
            .float64 => |r_val| @as(f64, @floatFromInt(l_val)) <= r_val,
            else => return EvalResult.error_result,
        },
        .float32 => |l_val| switch (right) {
            .int => |r_val| l_val <= @as(f32, @floatFromInt(r_val)),
            .float32 => |r_val| l_val <= r_val,
            .float64 => |r_val| @as(f64, l_val) <= r_val,
            else => return EvalResult.error_result,
        },
        .float64 => |l_val| switch (right) {
            .int => |r_val| l_val <= @as(f64, @floatFromInt(r_val)),
            .float32 => |r_val| l_val <= @as(f64, r_val),
            .float64 => |r_val| l_val <= r_val,
            else => return EvalResult.error_result,
        },
        .string => |l_val| switch (right) {
            .string => |r_val| std.mem.order(u8, l_val, r_val) != .gt,
            else => return EvalResult.error_result,
        },
        else => return EvalResult.error_result,
    };
    return EvalResult{ .boolean = less_than_or_equal };
}

/// Perform greater-than comparison between two EvalResults
/// Supports numeric types and string lexicographic comparison
pub fn performGt(left: EvalResult, right: EvalResult) EvalResult {
    const greater_than = switch (left) {
        .int => |l_val| switch (right) {
            .int => |r_val| l_val > r_val,
            .float32 => |r_val| @as(f32, @floatFromInt(l_val)) > r_val,
            .float64 => |r_val| @as(f64, @floatFromInt(l_val)) > r_val,
            else => return EvalResult.error_result,
        },
        .float32 => |l_val| switch (right) {
            .int => |r_val| l_val > @as(f32, @floatFromInt(r_val)),
            .float32 => |r_val| l_val > r_val,
            .float64 => |r_val| @as(f64, l_val) > r_val,
            else => return EvalResult.error_result,
        },
        .float64 => |l_val| switch (right) {
            .int => |r_val| l_val > @as(f64, @floatFromInt(r_val)),
            .float32 => |r_val| l_val > @as(f64, r_val),
            .float64 => |r_val| l_val > r_val,
            else => return EvalResult.error_result,
        },
        .string => |l_val| switch (right) {
            .string => |r_val| std.mem.order(u8, l_val, r_val) == .gt,
            else => return EvalResult.error_result,
        },
        else => return EvalResult.error_result,
    };
    return EvalResult{ .boolean = greater_than };
}

/// Perform greater-than-or-equal comparison between two EvalResults
/// Supports numeric types and string lexicographic comparison
pub fn performGte(left: EvalResult, right: EvalResult) EvalResult {
    const greater_than_or_equal = switch (left) {
        .int => |l_val| switch (right) {
            .int => |r_val| l_val >= r_val,
            .float32 => |r_val| @as(f32, @floatFromInt(l_val)) >= r_val,
            .float64 => |r_val| @as(f64, @floatFromInt(l_val)) >= r_val,
            else => return EvalResult.error_result,
        },
        .float32 => |l_val| switch (right) {
            .int => |r_val| l_val >= @as(f32, @floatFromInt(r_val)),
            .float32 => |r_val| l_val >= r_val,
            .float64 => |r_val| @as(f64, l_val) >= r_val,
            else => return EvalResult.error_result,
        },
        .float64 => |l_val| switch (right) {
            .int => |r_val| l_val >= @as(f64, @floatFromInt(r_val)),
            .float32 => |r_val| l_val >= @as(f64, r_val),
            .float64 => |r_val| l_val >= r_val,
            else => return EvalResult.error_result,
        },
        .string => |l_val| switch (right) {
            .string => |r_val| std.mem.order(u8, l_val, r_val) != .lt,
            else => return EvalResult.error_result,
        },
        else => return EvalResult.error_result,
    };
    return EvalResult{ .boolean = greater_than_or_equal };
}

/// Perform logical AND operation between two boolean EvalResults
/// Returns error_result if either operand is not a boolean
pub fn performAnd(left: EvalResult, right: EvalResult) EvalResult {
    switch (left) {
        .boolean => |l_val| switch (right) {
            .boolean => |r_val| return EvalResult{ .boolean = l_val and r_val },
            else => return EvalResult.error_result,
        },
        else => return EvalResult.error_result,
    }
}

/// Perform logical OR operation between two boolean EvalResults
/// Returns error_result if either operand is not a boolean
pub fn performOr(left: EvalResult, right: EvalResult) EvalResult {
    switch (left) {
        .boolean => |l_val| switch (right) {
            .boolean => |r_val| return EvalResult{ .boolean = l_val or r_val },
            else => return EvalResult.error_result,
        },
        else => return EvalResult.error_result,
    }
}
