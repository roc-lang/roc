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

pub const EvalError = error{
    Crash,
    OutOfMemory,
    StackOverflow,
    LayoutError,
    InvalidBranchNode,
    TypeMismatch,
};

pub const EvalResult = struct {
    layout: layout.Layout,
    ptr: *anyopaque,
};

/// Evaluates the canonical IR expression node, allocates memory on the stack for the result,
/// writes the actual bytes into that memory, and returns both the Layout and a pointer to the memory.
pub fn eval(
    allocator: std.mem.Allocator,
    cir: *CIR,
    expr_idx: CIR.Expr.Idx,
    eval_stack: *stack.Stack,
    layout_cache: *layout_store.Store,
) EvalError!EvalResult {
    const expr = cir.store.getExpr(expr_idx);

    // Check for runtime errors first, before trying to get type info
    switch (expr) {
        // Runtime errors should return an error immediately
        .runtime_error => return error.Crash,
        else => {},
    }

    // For now, we'll use simple layouts based on the expression type
    // In a real implementation, this would come from the type checker
    const expr_layout = switch (expr) {
        .num, .int => layout.Layout.int(.i128), // Default to i128 for now
        .float => layout.Layout.frac(.f64),
        .single_quote => layout.Layout.int(.u32),
        .empty_record => layout.Layout.record(std.mem.Alignment.@"1", .{ .int_idx = 0 }),
        .zero_argument_tag => layout.Layout.int(.u16),
        .tag => layout.Layout.int(.u16), // Tags with args not fully supported yet
        .str, .str_segment => return error.LayoutError, // Skip strings for now
        else => return error.LayoutError, // Not implemented yet
    };

    // Calculate size and alignment
    const size = switch (expr_layout.tag) {
        .scalar => switch (expr_layout.data.scalar.tag) {
            .int => expr_layout.data.scalar.data.int.size(),
            .frac => expr_layout.data.scalar.data.frac.size(),
            .bool => 1,
            else => return error.LayoutError,
        },
        .record => 0, // Empty record
        else => return error.LayoutError,
    };

    const alignment = expr_layout.alignment(target.TargetUsize.native);

    // Allocate space on the stack
    const ptr = eval_stack.alloca(size, alignment) catch |err| switch (err) {
        error.StackOverflow => return error.StackOverflow,
    };

    // Check if the expression is already a primitive and write its bytes
    switch (expr) {
        // Numeric literals are primitives
        .num => |num| {
            // Write the number bytes to memory based on the layout
            writeIntToMemory(ptr, num.value.toI128(), .i128);
        },
        .int => |int_lit| {
            // Write integer literal to memory
            writeIntToMemory(ptr, int_lit.value.toI128(), .i128);
        },
        .float => |float_lit| {
            // Write float literal to memory
            const typed_ptr = @as(*f64, @ptrCast(@alignCast(ptr)));
            typed_ptr.* = float_lit.value;
        },

        // String literals - commented out for now as requested
        .str, .str_segment => {
            // TODO: Handle string allocation on heap
            return error.LayoutError;
        },

        // Character literals
        .single_quote => |char_lit| {
            // Characters are stored as u32 in Roc
            const char_ptr = @as(*u32, @ptrCast(@alignCast(ptr)));
            char_ptr.* = char_lit.value;
        },

        // Empty record
        .empty_record => {
            // Empty record has no bytes to write
        },

        // Zero-argument tags
        .zero_argument_tag => |tag| {
            // Write the tag discriminant as a u16
            // For boolean tags: True = 0, False = 1
            const tag_ptr = @as(*u16, @ptrCast(@alignCast(ptr)));
            const tag_name = cir.env.idents.getText(tag.name);
            if (std.mem.eql(u8, tag_name, "True")) {
                tag_ptr.* = 0;
            } else if (std.mem.eql(u8, tag_name, "False")) {
                tag_ptr.* = 1;
            } else {
                // TODO: get actual tag discriminant for other tags
                tag_ptr.* = 0;
            }
        },

        // Tags with arguments
        .tag => |tag| {
            // For now, just write 0 as tag discriminant
            _ = tag;
            const tag_ptr = @as(*u16, @ptrCast(@alignCast(ptr)));
            tag_ptr.* = 0; // TODO: get actual tag discriminant and handle payload
        },

        // Runtime errors are handled at the beginning of the function
        .runtime_error => unreachable,

        // If expressions - evaluate branches based on conditions
        .@"if" => |if_expr| {
            return evalIfExpression(allocator, cir, if_expr, eval_stack, layout_cache);
        },

        // Non-primitive expressions need evaluation (TODO: implement these)
        .lookup, .list, .when, .call, .record, .record_access, .binop, .block, .lambda => {
            // For now, these are not implemented
            return error.LayoutError;
        },
    }

    return EvalResult{
        .layout = expr_layout,
        .ptr = ptr,
    };
}

fn writeIntToMemory(ptr: *anyopaque, value: i128, precision: types.Num.Int.Precision) void {
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
/// - Only boolean tag union types are allowed as conditions
/// - A boolean is the tag union [True, False]
/// - Type mismatch error if condition evaluates to any non-boolean type
/// - Tag evaluation: True (discriminant 0) is true, False (discriminant 1) is false
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
fn evalIfExpression(
    allocator: std.mem.Allocator,
    cir: *CIR,
    if_expr: anytype,
    eval_stack: *stack.Stack,
    layout_cache: *layout_store.Store,
) EvalError!EvalResult {
    const branches = cir.store.sliceIfBranch(if_expr.branches);

    // Fast path: no branches, directly evaluate final_else
    if (branches.len == 0) {
        return eval(allocator, cir, if_expr.final_else, eval_stack, layout_cache);
    }

    // Evaluate each branch condition in order
    for (branches) |branch_idx| {
        const branch = extractBranchData(cir, branch_idx) catch |err| switch (err) {
            error.InvalidBranchNode => return error.LayoutError,
            else => |e| return e,
        };

        // Evaluate the condition
        const cond_result = try eval(allocator, cir, branch.condition, eval_stack, layout_cache);

        // Check if condition is true
        const is_true = evaluateBooleanCondition(cond_result) catch |err| switch (err) {
            error.TypeMismatch => return error.LayoutError,
            else => |e| return e,
        };

        if (is_true) {
            // Condition is true, evaluate and return the branch body
            return eval(allocator, cir, branch.body, eval_stack, layout_cache);
        }
    }

    // No branch condition was true, evaluate the final_else
    return eval(allocator, cir, if_expr.final_else, eval_stack, layout_cache);
}

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
fn extractBranchData(cir: *CIR, branch_idx: CIR.IfBranch.Idx) !BranchData {
    // Convert branch index to node index
    const branch_node_idx: Node.Idx = @enumFromInt(@intFromEnum(branch_idx));
    const branch_node = cir.store.nodes.get(branch_node_idx);

    // Validate node type
    if (branch_node.tag != .if_branch) {
        return error.InvalidBranchNode;
    }

    // Extract indices from extra_data
    const extra_data_start = branch_node.data_1;
    const extra_data = cir.store.extra_data.items;

    // Bounds check to ensure we have both condition and body indices
    if (extra_data_start + 1 >= extra_data.len) {
        return error.InvalidBranchNode;
    }

    return BranchData{
        .condition = @enumFromInt(extra_data[extra_data_start]),
        .body = @enumFromInt(extra_data[extra_data_start + 1]),
    };
}

/// Evaluates a boolean condition result and returns whether it's true.
///
/// This function enforces strict type checking - only boolean tag union types are allowed.
/// A boolean in Roc is represented as the tag union `[True, False]`.
///
/// Returns:
/// - true if the tag is `True` (discriminant 0)
/// - false if the tag is `False` (discriminant 1)
/// - error.TypeMismatch if the condition is not a boolean tag union
fn evaluateBooleanCondition(cond_result: EvalResult) !bool {
    // Boolean values in Roc are represented as tags (zero-argument tag union)
    // The tag union [True, False] is represented as a u16 discriminant
    if (cond_result.layout.tag != .scalar or cond_result.layout.data.scalar.tag != .int) {
        // Type mismatch: condition must be a tag (represented as int)
        return error.TypeMismatch;
    }

    // For zero-argument tags, we expect a u16 layout
    const int_precision = cond_result.layout.data.scalar.data.int;
    if (int_precision != .u16) {
        return error.TypeMismatch;
    }

    // Read the tag discriminant
    const tag_ptr = @as(*u16, @ptrCast(@alignCast(cond_result.ptr)));
    const discriminant = tag_ptr.*;

    // In the tag union [True, False]:
    // - True has discriminant 0
    // - False has discriminant 1
    return discriminant == 0;
}

test {
    _ = @import("eval_test.zig");
}

test "evaluateBooleanCondition - valid True tag" {
    const testing = std.testing;

    // Create a True tag (discriminant 0)
    var tag_value: u16 = 0;
    const result = EvalResult{
        .layout = layout.Layout.int(.u16),
        .ptr = &tag_value,
    };

    const is_true = try evaluateBooleanCondition(result);
    try testing.expect(is_true);
}

test "evaluateBooleanCondition - valid False tag" {
    const testing = std.testing;

    // Create a False tag (discriminant 1)
    var tag_value: u16 = 1;
    const result = EvalResult{
        .layout = layout.Layout.int(.u16),
        .ptr = &tag_value,
    };

    const is_false = try evaluateBooleanCondition(result);
    try testing.expect(!is_false);
}

test "evaluateBooleanCondition - wrong integer precision error" {
    const testing = std.testing;

    // Create an integer value with wrong precision (not u16)
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

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var cir = CIR.init(&module_env);
    defer cir.deinit();

    // Add some dummy data to extra_data
    cir.store.extra_data.append(gpa, 123) catch unreachable; // condition idx
    cir.store.extra_data.append(gpa, 456) catch unreachable; // body idx

    // Create a branch node
    const node = Node{
        .data_1 = 0, // Points to start of extra_data
        .data_2 = 0,
        .data_3 = 0,
        .region = base.Region.zero(),
        .tag = .if_branch,
    };
    const node_idx = cir.store.nodes.append(gpa, node);

    // Extract branch data
    const branch_idx: CIR.IfBranch.Idx = @enumFromInt(@intFromEnum(node_idx));
    const branch_data = try extractBranchData(&cir, branch_idx);

    try testing.expectEqual(@as(u32, 123), @intFromEnum(branch_data.condition));
    try testing.expectEqual(@as(u32, 456), @intFromEnum(branch_data.body));
}

test "extractBranchData - wrong node type" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var cir = CIR.init(&module_env);
    defer cir.deinit();

    // Create a non-branch node
    const node = Node{
        .data_1 = 0,
        .data_2 = 0,
        .data_3 = 0,
        .region = base.Region.zero(),
        .tag = .expr_int, // Wrong type
    };
    const node_idx = cir.store.nodes.append(gpa, node);

    // Try to extract branch data from wrong node type
    const branch_idx: CIR.IfBranch.Idx = @enumFromInt(@intFromEnum(node_idx));
    const err = extractBranchData(&cir, branch_idx);

    try testing.expectError(error.InvalidBranchNode, err);
}

test "extractBranchData - out of bounds extra_data" {
    const testing = std.testing;
    const gpa = testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var cir = CIR.init(&module_env);
    defer cir.deinit();

    // Create a branch node with invalid extra_data index
    const node = Node{
        .data_1 = 999, // Out of bounds
        .data_2 = 0,
        .data_3 = 0,
        .region = base.Region.zero(),
        .tag = .if_branch,
    };
    const node_idx = cir.store.nodes.append(gpa, node);

    // Try to extract branch data with out of bounds index
    const branch_idx: CIR.IfBranch.Idx = @enumFromInt(@intFromEnum(node_idx));
    const err = extractBranchData(&cir, branch_idx);

    try testing.expectError(error.InvalidBranchNode, err);
}
