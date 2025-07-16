//! Evaluates canonicalized Roc expressions
//!
//! This module implements a stack-based interpreter for evaluating Roc expressions.
//! Values are pushed directly onto a stack, and operations pop their operands and
//! push results. No heap allocations are used for intermediate results.

const std = @import("std");
const CIR = @import("../check/canonicalize/CIR.zig");
const types = @import("../types/types.zig");
const types_store = @import("../types/store.zig");
const layout = @import("../layout/layout.zig");
const layout_store = @import("../layout/store.zig");
const stack = @import("stack.zig");
const target = @import("../base/target.zig");
const base = @import("../base.zig");
const collections = @import("../collections.zig");

pub const EvalError = error{
    Crash,
    OutOfMemory,
    StackOverflow,
    LayoutError,
    InvalidBranchNode,
    TypeMismatch,
    ZeroSizedType,
    TypeContainedMismatch,
    InvalidRecordExtension,
    BugUnboxedFlexVar,
    DivisionByZero,
    InvalidStackState,
};

/// Result of evaluating an expression
pub const EvalResult = struct {
    layout: layout.Layout,
    ptr: *anyopaque,
};

// Work item for the iterative evaluation stack
const WorkKind = enum {
    eval_expr,
    binop_add,
    binop_sub,
    binop_mul,
    binop_div,
    binop_eq,
    binop_ne,
    binop_gt,
    binop_lt,
    binop_ge,
    binop_le,
    if_check_condition,
};

pub const WorkItem = struct {
    kind: WorkKind,
    expr_idx: CIR.Expr.Idx,
};

// Represents a branch with condition and body
const BranchData = struct {
    cond: CIR.Expr.Idx,
    body: CIR.Expr.Idx,
};

/// Evaluates a CIR expression and returns the result
pub fn eval(
    allocator: std.mem.Allocator,
    cir: *const CIR,
    expr_idx: CIR.Expr.Idx,
    stack_memory: *stack.Stack,
    layout_cache: *layout_store.Store,
    type_store: *types_store.Store,
    work_stack: *std.ArrayList(WorkItem),
) EvalError!EvalResult {
    // Ensure work_stack is empty before we start. (stack_memory might not be, and that's fine!)
    std.debug.assert(work_stack.items.len == 0);
    errdefer work_stack.clearRetainingCapacity();

    // Save the initial stack pointer so we can find our result
    const initial_stack_ptr = @as([*]u8, @ptrCast(stack_memory.start)) + stack_memory.used;

    // Track layouts of values on the stack
    var layout_stack = std.ArrayList(layout.Layout).init(allocator);
    defer layout_stack.deinit();

    // Push initial work item
    try work_stack.append(.{
        .kind = .eval_expr,
        .expr_idx = expr_idx,
    });

    // Main evaluation loop
    while (work_stack.pop()) |work| {
        switch (work.kind) {
            .eval_expr => try evalExpr(
                allocator,
                cir,
                work.expr_idx,
                stack_memory,
                &layout_stack,
                layout_cache,
                type_store,
                work_stack,
            ),
            .binop_add, .binop_sub, .binop_mul, .binop_div, .binop_eq, .binop_ne, .binop_gt, .binop_lt, .binop_ge, .binop_le => {
                try completeBinop(
                    work.kind,
                    stack_memory,
                    &layout_stack,
                    layout_cache,
                );
            },
            .if_check_condition => {
                // The expr_idx encodes both the if expression and the branch index
                // Lower 16 bits: if expression index
                // Upper 16 bits: branch index
                const if_expr_idx: CIR.Expr.Idx = @enumFromInt(@intFromEnum(work.expr_idx) & 0xFFFF);
                const branch_index: u16 = @intCast((@intFromEnum(work.expr_idx) >> 16) & 0xFFFF);
                try checkIfCondition(
                    allocator,
                    cir,
                    if_expr_idx,
                    branch_index,
                    stack_memory,
                    &layout_stack,
                    work_stack,
                );
            },
        }
    }

    // The final result should be the only thing left on the layout stack
    if (layout_stack.items.len != 1) {
        return error.InvalidStackState;
    }

    const final_layout = layout_stack.items[0];

    // Ensure work stack is empty at the end - if not, it's a bug!
    std.debug.assert(work_stack.items.len == 0);

    // The result is at the beginning of our stack frame
    return EvalResult{
        .layout = final_layout,
        .ptr = @as(*anyopaque, @ptrCast(initial_stack_ptr)),
    };
}

fn evalExpr(
    allocator: std.mem.Allocator,
    cir: *const CIR,
    expr_idx: CIR.Expr.Idx,
    eval_stack: *stack.Stack,
    layout_stack: *std.ArrayList(layout.Layout),
    layout_cache: *layout_store.Store,
    type_store: *types_store.Store,
    work_stack: *std.ArrayList(WorkItem),
) EvalError!void {
    _ = allocator;
    _ = type_store;

    const expr = cir.store.getExpr(expr_idx);

    // Check for runtime errors first
    switch (expr) {
        .e_runtime_error => return error.Crash,
        else => {},
    }

    // Get the type variable for this expression
    const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(expr_idx)));

    // Get the real layout from the type checker
    const layout_idx = layout_cache.addTypeVar(expr_var) catch |err| switch (err) {
        error.ZeroSizedType => return error.ZeroSizedType,
        error.BugUnboxedRigidVar => return error.BugUnboxedFlexVar,
        else => |e| return e,
    };
    const expr_layout = layout_cache.getLayout(layout_idx);

    // Calculate size and alignment
    const size = layout_cache.layoutSize(expr_layout);
    const alignment = expr_layout.alignment(target.TargetUsize.native);

    // Handle different expression types
    switch (expr) {
        // Runtime errors are handled at the beginning
        .e_runtime_error => unreachable,

        // Numeric literals - push directly to stack
        .e_int => |int_lit| {
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };

            if (expr_layout.tag == .scalar and expr_layout.data.scalar.tag == .int) {
                const precision = expr_layout.data.scalar.data.int;
                writeIntToMemory(@as([*]u8, @ptrCast(ptr)), int_lit.value.toI128(), precision);
            } else {
                return error.LayoutError;
            }

            try layout_stack.append(expr_layout);
        },

        .e_frac_f64 => |float_lit| {
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };

            const typed_ptr = @as(*f64, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = float_lit.value;

            try layout_stack.append(expr_layout);
        },

        // Zero-argument tags (e.g., True, False)
        .e_zero_argument_tag => |tag| {
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };

            const tag_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
            const tag_name = cir.env.idents.getText(tag.name);
            if (std.mem.eql(u8, tag_name, "True")) {
                tag_ptr.* = 1;
            } else if (std.mem.eql(u8, tag_name, "False")) {
                tag_ptr.* = 0;
            } else {
                tag_ptr.* = 0; // TODO: get actual tag discriminant
            }

            try layout_stack.append(expr_layout);
        },

        // Empty record
        .e_empty_record => {
            // Empty record has no bytes
            try layout_stack.append(expr_layout);
        },

        // Empty list
        .e_empty_list => {
            // Empty list has no bytes
            try layout_stack.append(expr_layout);
        },

        // Binary operations
        .e_binop => |binop| {
            // Push work to complete the binop after operands are evaluated
            const binop_kind: WorkKind = switch (binop.op) {
                .add => .binop_add,
                .sub => .binop_sub,
                .mul => .binop_mul,
                .div => .binop_div,
                .eq => .binop_eq,
                .ne => .binop_ne,
                .gt => .binop_gt,
                .lt => .binop_lt,
                .ge => .binop_ge,
                .le => .binop_le,
                else => return error.Crash,
            };

            try work_stack.append(.{
                .kind = binop_kind,
                .expr_idx = expr_idx,
            });

            // Push operands in reverse order (right, then left)
            try work_stack.append(.{
                .kind = .eval_expr,
                .expr_idx = binop.rhs,
            });

            try work_stack.append(.{
                .kind = .eval_expr,
                .expr_idx = binop.lhs,
            });
        },

        // If expressions
        .e_if => |if_expr| {
            if (if_expr.branches.span.len > 0) {
                // Push work to check condition after it's evaluated
                // Encode branch index (0) in upper 16 bits
                const encoded_idx: CIR.Expr.Idx = @enumFromInt(@intFromEnum(expr_idx));
                try work_stack.append(.{
                    .kind = .if_check_condition,
                    .expr_idx = encoded_idx,
                });

                // Push work to evaluate the first condition
                const branches = cir.store.sliceIfBranches(if_expr.branches);
                const branch = cir.store.getIfBranch(branches[0]);

                try work_stack.append(.{
                    .kind = .eval_expr,
                    .expr_idx = branch.cond,
                });
            } else {
                // No branches, evaluate final_else directly
                try work_stack.append(.{
                    .kind = .eval_expr,
                    .expr_idx = if_expr.final_else,
                });
            }
        },

        // Pattern lookup
        .e_lookup_local => |lookup| {
            const defs = cir.store.sliceDefs(cir.all_defs);
            for (defs) |def_idx| {
                const def = cir.store.getDef(def_idx);
                if (@intFromEnum(def.pattern) == @intFromEnum(lookup.pattern_idx)) {
                    // Found the definition, evaluate its expression
                    try work_stack.append(.{
                        .kind = .eval_expr,
                        .expr_idx = def.expr,
                    });
                    return;
                }
            }
            return error.LayoutError; // Pattern not found
        },

        // Nominal expressions
        .e_nominal => |nominal| {
            // Evaluate the backing expression
            try work_stack.append(.{
                .kind = .eval_expr,
                .expr_idx = nominal.backing_expr,
            });
        },

        // Tags with arguments
        .e_tag => |tag| {
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };

            // For now, handle boolean tags (True/False) as u8
            const tag_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
            const tag_name = cir.env.idents.getText(tag.name);
            if (std.mem.eql(u8, tag_name, "True")) {
                tag_ptr.* = 1;
            } else if (std.mem.eql(u8, tag_name, "False")) {
                tag_ptr.* = 0;
            } else {
                tag_ptr.* = 0; // TODO: get actual tag discriminant
            }

            try layout_stack.append(expr_layout);
        },

        // Not yet implemented
        .e_str, .e_str_segment, .e_list, .e_tuple, .e_record, .e_dot_access, .e_block, .e_lambda, .e_call, .e_lookup_external, .e_match, .e_frac_dec, .e_dec_small, .e_crash, .e_dbg, .e_expect, .e_ellipsis => {
            return error.LayoutError;
        },
    }
}

fn completeBinop(
    kind: WorkKind,
    eval_stack: *stack.Stack,
    layout_stack: *std.ArrayList(layout.Layout),
    layout_cache: *layout_store.Store,
) EvalError!void {
    // Pop two layouts (right, then left)
    const right_layout = layout_stack.pop() orelse return error.InvalidStackState;
    const left_layout = layout_stack.pop() orelse return error.InvalidStackState;

    // For now, only support integer operations
    if (left_layout.tag != .scalar or right_layout.tag != .scalar) {
        return error.LayoutError;
    }

    const lhs_scalar = left_layout.data.scalar;
    const rhs_scalar = right_layout.data.scalar;

    if (lhs_scalar.tag != .int or rhs_scalar.tag != .int) {
        return error.LayoutError;
    }

    // The values are on the stack in order: left, then right
    // We need to calculate where they are based on their layouts
    const right_size = layout_cache.layoutSize(right_layout);
    const left_size = layout_cache.layoutSize(left_layout);

    // Get pointers to the values
    const right_ptr = @as([*]u8, @ptrCast(eval_stack.start)) + eval_stack.used - right_size;
    const left_ptr = right_ptr - left_size;

    // Read the values
    const lhs_val = readIntFromMemory(@as([*]u8, @ptrCast(left_ptr)), lhs_scalar.data.int);
    const rhs_val = readIntFromMemory(@as([*]u8, @ptrCast(right_ptr)), rhs_scalar.data.int);

    // Pop the operands from the stack
    eval_stack.used -= @as(u32, @intCast(left_size + right_size));

    // Determine result layout
    const result_layout = switch (kind) {
        .binop_add, .binop_sub, .binop_mul, .binop_div => left_layout, // Numeric result
        .binop_eq, .binop_ne, .binop_gt, .binop_lt, .binop_ge, .binop_le => blk: {
            // Boolean result
            const bool_layout = layout.Layout{
                .tag = .scalar,
                .data = .{ .scalar = .{
                    .tag = .int,
                    .data = .{ .int = .u8 },
                } },
            };
            break :blk bool_layout;
        },
        else => unreachable,
    };

    // Allocate space for result
    const result_size = layout_cache.layoutSize(result_layout);
    const result_alignment = result_layout.alignment(target.TargetUsize.native);
    const result_ptr = eval_stack.alloca(result_size, result_alignment) catch |err| switch (err) {
        error.StackOverflow => return error.StackOverflow,
    };

    // Perform the operation
    switch (kind) {
        .binop_add => {
            const result_val: i128 = lhs_val + rhs_val;
            writeIntToMemory(@as([*]u8, @ptrCast(result_ptr)), result_val, lhs_scalar.data.int);
        },
        .binop_sub => {
            const result_val: i128 = lhs_val - rhs_val;
            writeIntToMemory(@as([*]u8, @ptrCast(result_ptr)), result_val, lhs_scalar.data.int);
        },
        .binop_mul => {
            const result_val: i128 = lhs_val * rhs_val;
            writeIntToMemory(@as([*]u8, @ptrCast(result_ptr)), result_val, lhs_scalar.data.int);
        },
        .binop_div => {
            if (rhs_val == 0) {
                return error.DivisionByZero;
            }
            const result_val: i128 = @divTrunc(lhs_val, rhs_val);
            writeIntToMemory(@as([*]u8, @ptrCast(result_ptr)), result_val, lhs_scalar.data.int);
        },
        .binop_eq => {
            const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
            bool_ptr.* = if (lhs_val == rhs_val) 1 else 0;
        },
        .binop_ne => {
            const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
            bool_ptr.* = if (lhs_val != rhs_val) 1 else 0;
        },
        .binop_gt => {
            const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
            bool_ptr.* = if (lhs_val > rhs_val) 1 else 0;
        },
        .binop_lt => {
            const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
            bool_ptr.* = if (lhs_val < rhs_val) 1 else 0;
        },
        .binop_ge => {
            const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
            bool_ptr.* = if (lhs_val >= rhs_val) 1 else 0;
        },
        .binop_le => {
            const bool_ptr = @as(*u8, @ptrCast(@alignCast(result_ptr)));
            bool_ptr.* = if (lhs_val <= rhs_val) 1 else 0;
        },
        else => unreachable,
    }

    // Push result layout
    try layout_stack.append(result_layout);
}

fn checkIfCondition(
    allocator: std.mem.Allocator,
    cir: *const CIR,
    expr_idx: CIR.Expr.Idx,
    branch_index: u16,
    eval_stack: *stack.Stack,
    layout_stack: *std.ArrayList(layout.Layout),
    work_stack: *std.ArrayList(WorkItem),
) EvalError!void {
    _ = allocator;

    // Pop the condition layout
    _ = layout_stack.pop() orelse return error.InvalidStackState; // Remove condition layout

    // Read the condition value
    const cond_size = 1; // Boolean is u8
    const cond_ptr = @as([*]u8, @ptrCast(eval_stack.start)) + eval_stack.used - cond_size;
    const cond_val = @as(*u8, @ptrCast(@alignCast(cond_ptr))).*;

    // Pop the condition from the stack
    eval_stack.used -= cond_size;

    // Get the if expression
    const if_expr = switch (cir.store.getExpr(expr_idx)) {
        .e_if => |e| e,
        else => return error.InvalidBranchNode,
    };

    const branches = cir.store.sliceIfBranches(if_expr.branches);

    if (branch_index >= branches.len) {
        return error.InvalidBranchNode;
    }

    const current_branch = cir.store.getIfBranch(branches[branch_index]);

    if (cond_val == 1) {
        // Condition is true, evaluate this branch's body
        try work_stack.append(.{
            .kind = .eval_expr,
            .expr_idx = current_branch.body,
        });
    } else {
        // Condition is false, check if there's another branch
        if (branch_index + 1 < branches.len) {
            // Evaluate the next branch
            const next_branch = cir.store.getIfBranch(branches[branch_index + 1]);

            // Push work to check next condition after it's evaluated
            // Encode branch index in upper 16 bits
            const next_branch_idx = branch_index + 1;
            const encoded_idx: CIR.Expr.Idx = @enumFromInt(@intFromEnum(expr_idx) | (@as(u32, next_branch_idx) << 16));
            try work_stack.append(.{
                .kind = .if_check_condition,
                .expr_idx = encoded_idx,
            });

            // Push work to evaluate the next condition
            try work_stack.append(.{
                .kind = .eval_expr,
                .expr_idx = next_branch.cond,
            });
        } else {
            // No more branches, evaluate final_else
            try work_stack.append(.{
                .kind = .eval_expr,
                .expr_idx = if_expr.final_else,
            });
        }
    }
}

// Helper function to write an integer to memory with the correct precision
fn writeIntToMemory(ptr: [*]u8, value: i128, precision: types.Num.Int.Precision) void {
    switch (precision) {
        .u8 => @as(*u8, @ptrCast(@alignCast(ptr))).* = @as(u8, @intCast(value)),
        .u16 => @as(*u16, @ptrCast(@alignCast(ptr))).* = @as(u16, @intCast(value)),
        .u32 => @as(*u32, @ptrCast(@alignCast(ptr))).* = @as(u32, @intCast(value)),
        .u64 => @as(*u64, @ptrCast(@alignCast(ptr))).* = @as(u64, @intCast(value)),
        .u128 => @as(*u128, @ptrCast(@alignCast(ptr))).* = @as(u128, @intCast(value)),
        .i8 => @as(*i8, @ptrCast(@alignCast(ptr))).* = @as(i8, @intCast(value)),
        .i16 => @as(*i16, @ptrCast(@alignCast(ptr))).* = @as(i16, @intCast(value)),
        .i32 => @as(*i32, @ptrCast(@alignCast(ptr))).* = @as(i32, @intCast(value)),
        .i64 => @as(*i64, @ptrCast(@alignCast(ptr))).* = @as(i64, @intCast(value)),
        .i128 => @as(*i128, @ptrCast(@alignCast(ptr))).* = value,
    }
}

// Helper function to read an integer from memory with the correct precision
fn readIntFromMemory(ptr: [*]u8, precision: types.Num.Int.Precision) i128 {
    return switch (precision) {
        .u8 => @as(i128, @as(*u8, @ptrCast(@alignCast(ptr))).*),
        .u16 => @as(i128, @as(*u16, @ptrCast(@alignCast(ptr))).*),
        .u32 => @as(i128, @as(*u32, @ptrCast(@alignCast(ptr))).*),
        .u64 => @as(i128, @as(*u64, @ptrCast(@alignCast(ptr))).*),
        .u128 => @as(i128, @intCast(@as(*u128, @ptrCast(@alignCast(ptr))).*)),
        .i8 => @as(i128, @as(*i8, @ptrCast(@alignCast(ptr))).*),
        .i16 => @as(i128, @as(*i16, @ptrCast(@alignCast(ptr))).*),
        .i32 => @as(i128, @as(*i32, @ptrCast(@alignCast(ptr))).*),
        .i64 => @as(i128, @as(*i64, @ptrCast(@alignCast(ptr))).*),
        .i128 => @as(*i128, @ptrCast(@alignCast(ptr))).*,
    };
}

test {
    _ = @import("eval_test.zig");
}

test "stack-based binary operations" {
    // Test that the stack-based interpreter correctly evaluates binary operations
    const allocator = std.testing.allocator;

    // Create a simple stack for testing
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Track layouts
    var layout_stack = std.ArrayList(layout.Layout).init(allocator);
    defer layout_stack.deinit();

    // Test addition: 2 + 3 = 5
    {
        // Push 2
        const int_layout = layout.Layout{
            .tag = .scalar,
            .data = .{ .scalar = .{
                .tag = .int,
                .data = .{ .int = .i64 },
            } },
        };
        const size = @sizeOf(i64);
        const alignment: std.mem.Alignment = .@"8";

        const ptr1 = eval_stack.alloca(size, alignment) catch unreachable;
        @as(*i64, @ptrCast(@alignCast(ptr1))).* = 2;
        try layout_stack.append(int_layout);

        // Push 3
        const ptr2 = eval_stack.alloca(size, alignment) catch unreachable;
        @as(*i64, @ptrCast(@alignCast(ptr2))).* = 3;
        try layout_stack.append(int_layout);

        // Perform addition
        try completeBinop(.binop_add, &eval_stack, &layout_stack, undefined);

        // Check result
        try std.testing.expectEqual(@as(usize, 1), layout_stack.items.len);
        const result_ptr = @as([*]u8, @ptrCast(eval_stack.start)) + eval_stack.used - size;
        const result = @as(*i64, @ptrCast(@alignCast(result_ptr))).*;
        try std.testing.expectEqual(@as(i64, 5), result);
    }
}

test "stack-based comparisons" {
    // Test that comparisons produce boolean results
    const allocator = std.testing.allocator;

    // Create a simple stack for testing
    var eval_stack = try stack.Stack.initCapacity(allocator, 1024);
    defer eval_stack.deinit();

    // Track layouts
    var layout_stack = std.ArrayList(layout.Layout).init(allocator);
    defer layout_stack.deinit();

    // Test 5 > 3 = True (1)
    {
        // Push 5
        const int_layout = layout.Layout{
            .tag = .scalar,
            .data = .{ .scalar = .{
                .tag = .int,
                .data = .{ .int = .i64 },
            } },
        };
        const size = @sizeOf(i64);
        const alignment: std.mem.Alignment = .@"8";

        const ptr1 = eval_stack.alloca(size, alignment) catch unreachable;
        @as(*i64, @ptrCast(@alignCast(ptr1))).* = 5;
        try layout_stack.append(int_layout);

        // Push 3
        const ptr2 = eval_stack.alloca(size, alignment) catch unreachable;
        @as(*i64, @ptrCast(@alignCast(ptr2))).* = 3;
        try layout_stack.append(int_layout);

        // Perform comparison
        try completeBinop(.binop_gt, &eval_stack, &layout_stack, undefined);

        // Check result - should be a u8 with value 1 (true)
        try std.testing.expectEqual(@as(usize, 1), layout_stack.items.len);
        const bool_layout = layout_stack.items[0];
        try std.testing.expect(bool_layout.tag == .scalar);
        try std.testing.expect(bool_layout.data.scalar.tag == .int);
        try std.testing.expect(bool_layout.data.scalar.data.int == .u8);

        const result_ptr = @as([*]u8, @ptrCast(eval_stack.start)) + eval_stack.used - 1;
        const result = result_ptr[0];
        try std.testing.expectEqual(@as(u8, 1), result);
    }
}
