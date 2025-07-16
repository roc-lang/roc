//! Given canonical IR and type info, evaluate it until it is a primitive canonical IR node.
const std = @import("std");
const CIR = @import("../check/canonicalize/CIR.zig");
const stack = @import("stack.zig");
const layout = @import("../layout/layout.zig");
const layout_store = @import("../layout/store.zig");
const types = @import("../types.zig");
const base = @import("../base.zig");
const target = @import("../base/target.zig");
const Node = @import("../check/canonicalize/Node.zig");
const CalledVia = base.CalledVia;

/// Errors that can occur during expression evaluation
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
    BugUnboxedRigidVar,
};

/// Result of evaluating an expression, containing the memory layout and pointer to the value
pub const EvalResult = struct {
    layout: layout.Layout,
    ptr: *anyopaque,
};

// Work item types for the evaluation stack machine
const WorkKind = enum {
    eval_expr,
    complete_binop,
    complete_if_condition,
    complete_call,
};

const WorkItem = struct {
    kind: WorkKind,
    expr_idx: CIR.Expr.Idx,
    result_location: *EvalResult,
};

// Context structures for different operations
const BinopContext = struct {
    op: CIR.Expr.Binop.Op,
    left_result: *EvalResult,
    right_result: *EvalResult,
    result_location: *EvalResult,
};

const IfContext = struct {
    expr_idx: CIR.Expr.Idx,
    branch_index: usize,
    cond_result: *EvalResult,
    result_location: *EvalResult,
};

const CallContext = struct {
    expr_idx: CIR.Expr.Idx,
    args: []const CIR.Expr.Idx,
    arg_results: []*EvalResult,
    result_location: *EvalResult,
};

/// Evaluates the canonical IR expression node, allocates memory on the stack for the result,
/// writes the actual bytes into that memory, and returns both the Layout and a pointer to the memory.
pub fn eval(
    allocator: std.mem.Allocator,
    cir: *const CIR,
    expr_idx: CIR.Expr.Idx,
    eval_stack: *stack.Stack,
    layout_cache: *layout_store.Store,
    type_store: *types.Store,
) EvalError!EvalResult {
    var final_result: EvalResult = undefined;

    // Initialize work stack
    var work_stack = std.ArrayList(WorkItem).init(allocator);
    defer work_stack.deinit();

    // Initialize context stacks for different work types
    var binop_contexts = std.ArrayList(BinopContext).init(allocator);
    defer binop_contexts.deinit();

    var if_contexts = std.ArrayList(IfContext).init(allocator);
    defer if_contexts.deinit();

    var call_contexts = std.ArrayList(CallContext).init(allocator);
    defer call_contexts.deinit();

    // Push initial work item
    try work_stack.append(.{
        .kind = .eval_expr,
        .expr_idx = expr_idx,
        .result_location = &final_result,
    });

    // Main evaluation loop
    while (work_stack.items.len > 0) {
        const work = work_stack.pop() orelse unreachable;
        switch (work.kind) {
            .eval_expr => try evalExprIterative(
                allocator,
                cir,
                work.expr_idx,
                work.result_location,
                eval_stack,
                layout_cache,
                type_store,
                &work_stack,
                &binop_contexts,
                &if_contexts,
                &call_contexts,
            ),
            .complete_binop => {
                const context = binop_contexts.pop() orelse unreachable;
                try completeBinop(
                    cir,
                    context,
                    eval_stack,
                    layout_cache,
                    type_store,
                );
            },
            .complete_if_condition => {
                const context = if_contexts.pop() orelse unreachable;
                try completeIfCondition(
                    allocator,
                    cir,
                    context,
                    eval_stack,
                    layout_cache,
                    type_store,
                    &work_stack,
                    &if_contexts,
                );
            },
            .complete_call => {
                const context = call_contexts.pop() orelse unreachable;
                try completeCall(
                    allocator,
                    cir,
                    context,
                    eval_stack,
                    layout_cache,
                    type_store,
                );
            },
        }
    }

    return final_result;
}

fn evalExprIterative(
    allocator: std.mem.Allocator,
    cir: *const CIR,
    expr_idx: CIR.Expr.Idx,
    result_location: *EvalResult,
    eval_stack: *stack.Stack,
    layout_cache: *layout_store.Store,
    type_store: *types.Store,
    work_stack: *std.ArrayList(WorkItem),
    binop_contexts: *std.ArrayList(BinopContext),
    if_contexts: *std.ArrayList(IfContext),
    call_contexts: *std.ArrayList(CallContext),
) EvalError!void {
    _ = type_store;
    _ = call_contexts;
    const expr = cir.store.getExpr(expr_idx);

    // Check for runtime errors first, before trying to get type info
    switch (expr) {
        // Runtime errors should return an error immediately
        .e_runtime_error => return error.Crash,
        else => {},
    }

    // Get the type variable for this expression
    // In CIR, expression indices and type variables are kept in sync
    const expr_var = @as(types.Var, @enumFromInt(@intFromEnum(expr_idx)));

    // Get the real layout from the type checker
    const layout_idx = layout_cache.addTypeVar(expr_var) catch |err| switch (err) {
        error.ZeroSizedType => {
            // Zero-sized types don't need any allocation
            // We can't create a proper layout for them, so return a special error
            // that the caller can handle appropriately
            return error.ZeroSizedType;
        },
        else => return err,
    };
    const expr_layout = layout_cache.getLayout(layout_idx);

    // Calculate size and alignment using the layout store
    const size = layout_cache.layoutSize(expr_layout);
    const alignment = expr_layout.alignment(target.TargetUsize.native);

    // Allocate space on the stack
    const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
        error.StackOverflow => return error.StackOverflow,
    };

    // Check if the expression is already a primitive and write its bytes
    switch (expr) {
        // Numeric literals are primitives
        .e_int => |int_lit| {
            // Write integer literal to memory based on actual layout precision
            if (expr_layout.tag == .scalar and expr_layout.data.scalar.tag == .int) {
                const precision = expr_layout.data.scalar.data.int;
                writeIntToMemory(@as([*]u8, @ptrCast(ptr)), int_lit.value.toI128(), precision);
            } else {
                return error.LayoutError;
            }
        },
        .e_frac_f64 => |float_lit| {
            // Write float literal to memory
            const typed_ptr = @as(*f64, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = float_lit.value;
        },
        .e_str, .e_str_segment => {
            // TODO: Handle string allocation on heap
            return error.LayoutError;
        },

        // Empty record
        .e_empty_record => {
            // Empty record has no bytes to write
        },

        // Zero-argument tags
        .e_zero_argument_tag => |tag| {
            // Write the tag discriminant as a u8
            // For boolean tags: True = 1, False = 0
            const tag_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
            const tag_name = cir.env.idents.getText(tag.name);
            if (std.mem.eql(u8, tag_name, "True")) {
                tag_ptr.* = 1;
            } else if (std.mem.eql(u8, tag_name, "False")) {
                tag_ptr.* = 0;
            } else {
                // TODO: get actual tag discriminant for other tags
                tag_ptr.* = 0;
            }
        },

        // Tags with arguments
        .e_tag => |tag| {
            // Use the layout_idx we already computed at the beginning of the function
            const tag_layout = layout_cache.getLayout(layout_idx);

            // Write the discriminant based on the layout
            switch (tag_layout.tag) {
                .scalar => switch (tag_layout.data.scalar.tag) {
                    .int => switch (tag_layout.data.scalar.data.int) {
                        .u8 => {
                            const tag_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
                            // For boolean tags: True = 1, False = 0
                            const tag_name = cir.env.idents.getText(tag.name);
                            if (std.mem.eql(u8, tag_name, "True")) {
                                tag_ptr.* = 1;
                            } else if (std.mem.eql(u8, tag_name, "False")) {
                                tag_ptr.* = 0;
                            } else {
                                tag_ptr.* = 0; // TODO: get actual tag discriminant for other tags
                            }
                        },
                        .u16 => {
                            const tag_ptr = @as(*u16, @ptrCast(@alignCast(ptr)));
                            tag_ptr.* = 0; // TODO: get actual tag discriminant
                        },
                        .u32 => {
                            const tag_ptr = @as(*u32, @ptrCast(@alignCast(ptr)));
                            tag_ptr.* = 0; // TODO: get actual tag discriminant
                        },
                        else => return error.LayoutError,
                    },
                    .bool => {
                        // Bool tags (True/False) as u8
                        const bool_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
                        // Check the tag name to determine if it's True or False
                        const tag_name = cir.env.idents.getText(tag.name);
                        bool_ptr.* = if (std.mem.eql(u8, tag_name, "True")) 1 else 0;
                    },
                    else => return error.LayoutError,
                },
                else => return error.LayoutError,
            }
        },

        // Runtime errors are handled at the beginning of the function
        .e_runtime_error => unreachable,

        // If expressions - evaluate branches based on conditions
        .e_if => |if_expr| {
            // Start evaluating the first branch condition
            const branches = cir.store.sliceIfBranches(if_expr.branches);
            if (branches.len > 0) {
                const branch = extractBranchData(cir, branches[0]) catch |err| switch (err) {
                    error.InvalidBranchNode => return error.LayoutError,
                    else => |e| return e,
                };

                // Allocate space for condition result
                const cond_result = try allocator.create(EvalResult);
                errdefer allocator.destroy(cond_result);

                // Save context for completing the if expression
                try if_contexts.append(.{
                    .expr_idx = expr_idx,
                    .branch_index = 0,
                    .cond_result = cond_result,
                    .result_location = result_location,
                });

                // Push work to complete the if after condition is evaluated
                try work_stack.append(.{
                    .kind = .complete_if_condition,
                    .expr_idx = expr_idx,
                    .result_location = result_location,
                });

                // Push work to evaluate the condition
                try work_stack.append(.{
                    .kind = .eval_expr,
                    .expr_idx = branch.condition,
                    .result_location = cond_result,
                });
            } else {
                // No branches, evaluate final_else directly
                try work_stack.append(.{
                    .kind = .eval_expr,
                    .expr_idx = if_expr.final_else,
                    .result_location = result_location,
                });
            }
            return;
        },

        // Non-primitive expressions need evaluation
        .e_lookup_local => |lookup| {
            // Look up the pattern in the definitions
            const defs = cir.store.sliceDefs(cir.all_defs);
            for (defs) |def_idx| {
                const def = cir.store.getDef(def_idx);
                if (@intFromEnum(def.pattern) == @intFromEnum(lookup.pattern_idx)) {
                    // Found the definition, evaluate its expression
                    try work_stack.append(.{
                        .kind = .eval_expr,
                        .expr_idx = def.expr,
                        .result_location = result_location,
                    });
                    return;
                }
            }
            return error.LayoutError; // Pattern not found
        },
        .e_binop => |binop| {
            // Allocate space for left and right results
            const left_result = try allocator.create(EvalResult);
            const right_result = try allocator.create(EvalResult);

            // Save context for completing the binop
            try binop_contexts.append(.{
                .op = binop.op,
                .left_result = left_result,
                .right_result = right_result,
                .result_location = result_location,
            });

            // Push work items in reverse order (complete, right, left)
            try work_stack.append(.{
                .kind = .complete_binop,
                .expr_idx = expr_idx,
                .result_location = result_location,
            });

            try work_stack.append(.{
                .kind = .eval_expr,
                .expr_idx = binop.rhs,
                .result_location = right_result,
            });

            try work_stack.append(.{
                .kind = .eval_expr,
                .expr_idx = binop.lhs,
                .result_location = left_result,
            });

            return;
        },
        .e_call => {
            // TODO: Implement iterative call evaluation
            // For now, function calls are not supported in the iterative evaluator
            return error.LayoutError;
        },

        // TODO: implement these
        .e_lookup_external, .e_list, .e_match, .e_record, .e_dot_access, .e_block, .e_lambda => {
            // For now, these are not implemented
            return error.LayoutError;
        },

        // Additional numeric types
        .e_frac_dec => |dec_lit| {
            // High-precision decimal - not yet implemented
            _ = dec_lit;
            return error.LayoutError;
        },
        .e_dec_small => |small_dec| {
            // Small decimal representation - not yet implemented
            _ = small_dec;
            return error.LayoutError;
        },

        // Collection types
        .e_empty_list => {
            // Empty list has no bytes to write
        },
        .e_tuple => |tuple| {
            // Tuple evaluation not yet implemented
            _ = tuple;
            return error.LayoutError;
        },

        // Advanced constructs
        .e_nominal => |nominal| {
            // Evaluate the backing expression
            try work_stack.append(.{
                .kind = .eval_expr,
                .expr_idx = nominal.backing_expr,
                .result_location = result_location,
            });
            return;
        },
        .e_crash => |crash| {
            // Crash expression - should halt execution
            _ = crash;
            return error.Crash;
        },
        .e_dbg => |dbg| {
            // Debug expression not yet implemented
            _ = dbg;
            return error.LayoutError;
        },
        .e_expect => |expect| {
            // Expect expression not yet implemented
            _ = expect;
            return error.LayoutError;
        },
        .e_ellipsis => {
            // Ellipsis placeholder - not implemented
            return error.LayoutError;
        },
    }

    result_location.* = EvalResult{
        .layout = expr_layout,
        .ptr = ptr,
    };
}

fn completeBinop(
    cir: *const CIR,
    context: BinopContext,
    eval_stack: *stack.Stack,
    layout_cache: *layout_store.Store,
    type_store: *types.Store,
) EvalError!void {
    _ = type_store;
    const left_result = context.left_result.*;
    const right_result = context.right_result.*;

    // Clean up allocated results
    defer cir.env.gpa.destroy(context.left_result);
    defer cir.env.gpa.destroy(context.right_result);

    // For now, only support integer operations
    if (left_result.layout.tag != .scalar or right_result.layout.tag != .scalar) {
        return error.LayoutError;
    }

    const lhs_scalar = left_result.layout.data.scalar;
    const rhs_scalar = right_result.layout.data.scalar;

    if (lhs_scalar.tag != .int or rhs_scalar.tag != .int) {
        return error.LayoutError;
    }

    // Read integer values based on precision
    const lhs_precision = lhs_scalar.data.int;
    const rhs_precision = rhs_scalar.data.int;

    // Read values based on precision
    const lhs_val: i128 = switch (lhs_precision) {
        .u8 => @as(*u8, @ptrCast(@alignCast(left_result.ptr))).*,
        .i8 => @as(*i8, @ptrCast(@alignCast(left_result.ptr))).*,
        .u16 => @as(*u16, @ptrCast(@alignCast(left_result.ptr))).*,
        .i16 => @as(*i16, @ptrCast(@alignCast(left_result.ptr))).*,
        .u32 => @as(*u32, @ptrCast(@alignCast(left_result.ptr))).*,
        .i32 => @as(*i32, @ptrCast(@alignCast(left_result.ptr))).*,
        .u64 => @as(*u64, @ptrCast(@alignCast(left_result.ptr))).*,
        .i64 => @as(*i64, @ptrCast(@alignCast(left_result.ptr))).*,
        .u128 => @intCast(@as(*u128, @ptrCast(@alignCast(left_result.ptr))).*),
        .i128 => @as(*i128, @ptrCast(@alignCast(left_result.ptr))).*,
    };

    const rhs_val: i128 = switch (rhs_precision) {
        .u8 => @as(*u8, @ptrCast(@alignCast(right_result.ptr))).*,
        .i8 => @as(*i8, @ptrCast(@alignCast(right_result.ptr))).*,
        .u16 => @as(*u16, @ptrCast(@alignCast(right_result.ptr))).*,
        .i16 => @as(*i16, @ptrCast(@alignCast(right_result.ptr))).*,
        .u32 => @as(*u32, @ptrCast(@alignCast(right_result.ptr))).*,
        .i32 => @as(*i32, @ptrCast(@alignCast(right_result.ptr))).*,
        .u64 => @as(*u64, @ptrCast(@alignCast(right_result.ptr))).*,
        .i64 => @as(*i64, @ptrCast(@alignCast(right_result.ptr))).*,
        .u128 => @intCast(@as(*u128, @ptrCast(@alignCast(right_result.ptr))).*),
        .i128 => @as(*i128, @ptrCast(@alignCast(right_result.ptr))).*,
    };

    // Determine result layout based on operation
    const result_layout = switch (context.op) {
        .add, .sub, .mul, .div, .rem, .pow, .div_trunc => left_result.layout,
        .eq, .ne, .lt, .gt, .le, .ge => layout.Layout.boolType(),
        .@"and", .@"or" => layout.Layout.boolType(),
        else => return error.LayoutError,
    };

    // Perform the operation
    switch (context.op) {
        .add => {
            const result_val: i128 = lhs_val + rhs_val;
            const size = layout_cache.layoutSize(result_layout);
            const alignment = result_layout.alignment(target.TargetUsize.native);
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };
            writeIntToMemory(@as([*]u8, @ptrCast(ptr)), result_val, lhs_precision);
            context.result_location.* = EvalResult{
                .layout = result_layout,
                .ptr = ptr,
            };
        },
        .sub => {
            const result_val: i128 = lhs_val - rhs_val;
            const size = layout_cache.layoutSize(result_layout);
            const alignment = result_layout.alignment(target.TargetUsize.native);
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };
            writeIntToMemory(@as([*]u8, @ptrCast(ptr)), result_val, lhs_precision);
            context.result_location.* = EvalResult{
                .layout = result_layout,
                .ptr = ptr,
            };
        },
        .mul => {
            const result_val: i128 = lhs_val * rhs_val;
            const size = layout_cache.layoutSize(result_layout);
            const alignment = result_layout.alignment(target.TargetUsize.native);
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };
            writeIntToMemory(@as([*]u8, @ptrCast(ptr)), result_val, lhs_precision);
            context.result_location.* = EvalResult{
                .layout = result_layout,
                .ptr = ptr,
            };
        },
        .div => {
            if (rhs_val == 0) {
                return error.DivisionByZero;
            }
            const result_val: i128 = @divTrunc(lhs_val, rhs_val);
            const size = layout_cache.layoutSize(result_layout);
            const alignment = result_layout.alignment(target.TargetUsize.native);
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };
            writeIntToMemory(@as([*]u8, @ptrCast(ptr)), result_val, lhs_precision);
            context.result_location.* = EvalResult{
                .layout = result_layout,
                .ptr = ptr,
            };
        },
        .eq => {
            const result_val: bool = lhs_val == rhs_val;
            const size = layout_cache.layoutSize(result_layout);
            const alignment = result_layout.alignment(target.TargetUsize.native);
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };
            const typed_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = if (result_val) 1 else 0;
            context.result_location.* = EvalResult{
                .layout = result_layout,
                .ptr = ptr,
            };
        },
        .ne => {
            const result_val: bool = lhs_val != rhs_val;
            const size = layout_cache.layoutSize(result_layout);
            const alignment = result_layout.alignment(target.TargetUsize.native);
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };
            const typed_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = if (result_val) 1 else 0;
            context.result_location.* = EvalResult{
                .layout = result_layout,
                .ptr = ptr,
            };
        },
        .gt => {
            const result_val: bool = lhs_val > rhs_val;
            const size = layout_cache.layoutSize(result_layout);
            const alignment = result_layout.alignment(target.TargetUsize.native);
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };
            const typed_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = if (result_val) 1 else 0;
            context.result_location.* = EvalResult{
                .layout = result_layout,
                .ptr = ptr,
            };
        },
        .lt => {
            const result_val: bool = lhs_val < rhs_val;
            const size = layout_cache.layoutSize(result_layout);
            const alignment = result_layout.alignment(target.TargetUsize.native);
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };
            const typed_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = if (result_val) 1 else 0;
            context.result_location.* = EvalResult{
                .layout = result_layout,
                .ptr = ptr,
            };
        },
        .ge => {
            const result_val: bool = lhs_val >= rhs_val;
            const size = layout_cache.layoutSize(result_layout);
            const alignment = result_layout.alignment(target.TargetUsize.native);
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };
            const typed_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = if (result_val) 1 else 0;
            context.result_location.* = EvalResult{
                .layout = result_layout,
                .ptr = ptr,
            };
        },
        .le => {
            const result_val: bool = lhs_val <= rhs_val;
            const size = layout_cache.layoutSize(result_layout);
            const alignment = result_layout.alignment(target.TargetUsize.native);
            const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
                error.StackOverflow => return error.StackOverflow,
            };
            const typed_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = if (result_val) 1 else 0;
            context.result_location.* = EvalResult{
                .layout = result_layout,
                .ptr = ptr,
            };
        },
        else => {
            return error.LayoutError;
        },
    }
}

fn completeIfCondition(
    allocator: std.mem.Allocator,
    cir: *const CIR,
    context: IfContext,
    eval_stack: *stack.Stack,
    layout_cache: *layout_store.Store,
    type_store: *types.Store,
    work_stack: *std.ArrayList(WorkItem),
    if_contexts: *std.ArrayList(IfContext),
) EvalError!void {
    _ = eval_stack;
    _ = layout_cache;
    _ = type_store;

    // Free the condition result (do this before any error-returning calls)
    defer allocator.destroy(context.cond_result);

    // Check if condition is true
    const is_true = evaluateBooleanCondition(context.cond_result.*) catch |err| switch (err) {
        error.TypeMismatch => return error.LayoutError,
        else => |e| return e,
    };

    // Get the if expression from the expr_idx
    const expr = cir.store.getExpr(context.expr_idx);
    const if_expr = switch (expr) {
        .e_if => |e| e,
        else => return error.LayoutError,
    };

    if (is_true) {
        // Condition is true, evaluate the branch body
        const branches = cir.store.sliceIfBranches(if_expr.branches);
        const branch = extractBranchData(cir, branches[context.branch_index]) catch |err| switch (err) {
            error.InvalidBranchNode => return error.LayoutError,
            else => |e| return e,
        };

        try work_stack.append(.{
            .kind = .eval_expr,
            .expr_idx = branch.body,
            .result_location = context.result_location,
        });
    } else {
        // Try next branch
        const branches = cir.store.sliceIfBranches(if_expr.branches);
        const next_branch_index = context.branch_index + 1;

        if (next_branch_index < branches.len) {
            // Evaluate next branch condition
            const branch = extractBranchData(cir, branches[next_branch_index]) catch |err| switch (err) {
                error.InvalidBranchNode => return error.LayoutError,
                else => |e| return e,
            };

            // Allocate space for next condition result
            const cond_result = try allocator.create(EvalResult);

            // Save context for next branch
            try if_contexts.append(.{
                .expr_idx = context.expr_idx,
                .branch_index = next_branch_index,
                .cond_result = cond_result,
                .result_location = context.result_location,
            });

            // Push work to complete the if after condition is evaluated
            try work_stack.append(.{
                .kind = .complete_if_condition,
                .expr_idx = context.expr_idx,
                .result_location = context.result_location,
            });

            // Push work to evaluate the condition
            try work_stack.append(.{
                .kind = .eval_expr,
                .expr_idx = branch.condition,
                .result_location = cond_result,
            });
        } else {
            // No more branches, evaluate final_else
            try work_stack.append(.{
                .kind = .eval_expr,
                .expr_idx = if_expr.final_else,
                .result_location = context.result_location,
            });
        }
    }
}

fn completeCall(
    allocator: std.mem.Allocator,
    cir: *const CIR,
    context: CallContext,
    eval_stack: *stack.Stack,
    layout_cache: *layout_store.Store,
    type_store: *types.Store,
) EvalError!void {
    _ = eval_stack;
    _ = layout_cache;
    _ = type_store;
    // This is a placeholder - the full implementation would handle function calls
    _ = allocator;
    _ = cir;
    _ = context;
    _ = eval_stack;
    _ = layout_cache;
    _ = type_store;
    return error.LayoutError;
}

fn writeIntToMemory(ptr: [*]u8, value: i128, precision: types.Num.Int.Precision) void {
    switch (precision) {
        .u8 => {
            const typed_ptr = @as(*u8, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = @intCast(value);
        },
        .i8 => {
            const typed_ptr = @as(*i8, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = @intCast(value);
        },
        .u16 => {
            const typed_ptr = @as(*u16, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = @intCast(value);
        },
        .i16 => {
            const typed_ptr = @as(*i16, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = @intCast(value);
        },
        .u32 => {
            const typed_ptr = @as(*u32, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = @intCast(value);
        },
        .i32 => {
            const typed_ptr = @as(*i32, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = @intCast(value);
        },
        .u64 => {
            const typed_ptr = @as(*u64, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = @intCast(value);
        },
        .i64 => {
            const typed_ptr = @as(*i64, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = @intCast(value);
        },
        .u128 => {
            const typed_ptr = @as(*u128, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = @intCast(value);
        },
        .i128 => {
            const typed_ptr = @as(*i128, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = value;
        },
    }
}

/// Evaluates an if expression by checking conditions and executing the appropriate branch.
///
/// The evaluation strategy:
/// 1. If there are no branches (empty branches list), evaluate final_else directly
/// 2. Otherwise, evaluate each branch condition in order:
///    - The condition must evaluate to a boolean type
///    - If the condition evaluates to true, evaluate and return that branch's body
///    - If the condition is false, continue to the next branch
/// 3. If no branch condition is true, evaluate and return final_else
///
/// Type Requirements:
/// - Only boolean types are allowed as conditions (nominal Bool type)
/// - Booleans are represented as u8 values: True = 1, False = 0
/// - Type mismatch error if condition evaluates to any non-boolean type
/// - With nominal booleans, True and False are wrapped in nominal expressions
///
/// Branch Storage:
/// - Branches are stored as nodes with data in extra_data
/// - data_1: Index into extra_data where branch data starts
/// - extra_data[data_1]: Condition expression index
/// - extra_data[data_1 + 1]: Body expression index
///
/// Current Limitations:
/// - Complex boolean expressions (like `1 == 1`) not fully canonicalized yet
/// - Boolean tags (`True`, `False`) as part of the `[True, False]` tag union
/// - The `getIfBranch` helper function is not yet implemented in NodeStore
///
/// Future Work:
/// - Complete canonicalization support for boolean expressions
/// - Implement `getIfBranch` helper function in NodeStore
/// - Add support for pattern matching in if expression conditions
/// Represents the extracted data from an if branch node
const BranchData = struct {
    condition: CIR.Expr.Idx,
    body: CIR.Expr.Idx,
};

/// Extracts condition and body indices from a branch node.
///
/// Each branch is stored as a node with condition and body indices in extra_data.
/// Layout: [condition_idx, body_idx] starting at data_1
///
/// Returns:
/// - BranchData with condition and body indices on success
/// - error.InvalidBranchNode if the node type is wrong or data is out of bounds
fn extractBranchData(cir: *const CIR, branch_idx: CIR.Expr.IfBranch.Idx) error{InvalidBranchNode}!BranchData {
    // Convert branch index to node index
    const branch_node_idx: Node.Idx = @enumFromInt(@intFromEnum(branch_idx));
    const branch_node = cir.store.nodes.get(branch_node_idx);

    // Validate node type
    if (branch_node.tag != .if_branch) {
        return error.InvalidBranchNode;
    }

    // Extract indices from node data
    // Based on getIfBranch in NodeStore.zig:
    // data_1 contains the condition expression index
    // data_2 contains the body expression index
    return BranchData{
        .condition = @enumFromInt(branch_node.data_1),
        .body = @enumFromInt(branch_node.data_2),
    };
}

/// Evaluates a boolean condition result and returns whether it's true.
///
/// This function enforces strict type checking - only boolean types are allowed.
/// With nominal booleans, Bool is a nominal type wrapping u8 values.
///
/// Returns:
/// - true if the value is 1 (True)
/// - false if the value is 0 (False)
/// - error.TypeMismatch if the condition is not a boolean type
fn evaluateBooleanCondition(cond_result: EvalResult) error{TypeMismatch}!bool {
    // Check if this is a boolean scalar layout
    if (cond_result.layout.tag == .scalar and cond_result.layout.data.scalar.tag == .bool) {
        // Direct boolean value as u8
        const bool_ptr = @as(*const u8, @ptrCast(@alignCast(cond_result.ptr)));
        return bool_ptr.* == 1;
    }

    // With nominal booleans, True and False are u8 values
    // wrapped in nominal expressions (True = 1, False = 0)
    if (cond_result.layout.tag != .scalar or cond_result.layout.data.scalar.tag != .int) {
        // Type mismatch: condition must be a tag (represented as int)
        return error.TypeMismatch;
    }

    // For zero-argument tags, we expect a u8 layout
    const int_precision = cond_result.layout.data.scalar.data.int;
    if (int_precision != .u8) {
        return error.TypeMismatch;
    }

    // Read the tag discriminant
    const tag_ptr = @as(*u8, @ptrCast(@alignCast(cond_result.ptr)));
    const discriminant = tag_ptr.*;

    // In the nominal Bool type:
    // - True has value 1
    // - False has value 0
    return discriminant == 1;
}

test {
    _ = @import("eval_test.zig");
    _ = @import("nested_if_test.zig");
}

test "evaluateBooleanCondition - valid True tag" {
    const testing = std.testing;

    // Create a True tag (discriminant 1)
    var tag_value: u8 = 1;
    const result = EvalResult{
        .layout = layout.Layout.int(.u8),
        .ptr = &tag_value,
    };

    const is_true = try evaluateBooleanCondition(result);
    try testing.expect(is_true);
}

test "evaluateBooleanCondition - valid False tag" {
    const testing = std.testing;

    // Create a False tag (discriminant 0)
    var tag_value: u8 = 0;
    const result = EvalResult{
        .layout = layout.Layout.int(.u8),
        .ptr = &tag_value,
    };

    const is_false = try evaluateBooleanCondition(result);
    try testing.expect(!is_false);
}

test "evaluateBooleanCondition - wrong integer precision error" {
    const testing = std.testing;

    // Create an integer value with wrong precision (not u8)
    var int_value: i128 = 42;
    const result = EvalResult{
        .layout = layout.Layout.int(.i128),
        .ptr = &int_value,
    };

    const err = evaluateBooleanCondition(result);
    try testing.expectError(error.TypeMismatch, err);
}

test "evaluateBooleanCondition - string type error" {
    const testing = std.testing;

    // Create a string layout
    const result = EvalResult{
        .layout = layout.Layout.str(),
        .ptr = undefined, // Doesn't matter for this test
    };

    const err = evaluateBooleanCondition(result);
    try testing.expectError(error.TypeMismatch, err);
}

test "extractBranchData - valid branch node" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const owned_source = try gpa.dupe(u8, "");
    var module_env = try base.ModuleEnv.init(gpa, owned_source);
    defer module_env.deinit();

    var cir = try CIR.init(&module_env, "test");
    defer cir.deinit();

    // Create a branch node with condition and body indices
    const node = Node{
        .data_1 = 123, // condition idx
        .data_2 = 456, // body idx
        .data_3 = 0,

        .tag = .if_branch,
    };
    const node_idx = try cir.store.nodes.append(gpa, node);

    // Extract branch data
    const branch_idx: CIR.Expr.IfBranch.Idx = @enumFromInt(@intFromEnum(node_idx));
    const branch_data = try extractBranchData(&cir, branch_idx);

    try testing.expectEqual(@as(u32, 123), @intFromEnum(branch_data.condition));
    try testing.expectEqual(@as(u32, 456), @intFromEnum(branch_data.body));
}

test "extractBranchData - wrong node type" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const owned_source = try gpa.dupe(u8, "");
    var module_env = try base.ModuleEnv.init(gpa, owned_source);
    defer module_env.deinit();

    var cir = try CIR.init(&module_env, "test");
    defer cir.deinit();

    // Create a non-branch node
    const node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,

        .tag = .expr_int, // Wrong type
    };
    const node_idx = try cir.store.nodes.append(gpa, node);

    // Try to extract branch data from wrong node type
    const branch_idx: CIR.Expr.IfBranch.Idx = @enumFromInt(@intFromEnum(node_idx));
    const err = extractBranchData(&cir, branch_idx);

    try testing.expectError(error.InvalidBranchNode, err);
}

test "extractBranchData - invalid branch node" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const owned_source = try gpa.dupe(u8, "");
    var module_env = try base.ModuleEnv.init(gpa, owned_source);
    defer module_env.deinit();

    var cir = try CIR.init(&module_env, "test");
    defer cir.deinit();

    // Create a node that's not an if_branch
    const node = Node{
        .data_1 = 123,
        .data_2 = 456,
        .data_3 = 0,

        .tag = .expr_int, // Not an if_branch
    };
    const node_idx = try cir.store.nodes.append(gpa, node);

    // Extract branch data should fail
    const branch_idx: CIR.Expr.IfBranch.Idx = @enumFromInt(@intFromEnum(node_idx));
    const err = extractBranchData(&cir, branch_idx);

    try testing.expectError(error.InvalidBranchNode, err);
}
