//! Given canonical IR and type info, evaluate it until it is a primitive canonical IR node.
const std = @import("std");
const CIR = @import("../check/canonicalize/CIR.zig");
const stack = @import("stack.zig");
const layout = @import("../layout/layout.zig");
const layout_store = @import("../layout/store.zig");
const types = @import("../types.zig");
const base = @import("../base.zig");
const target = @import("../base/target.zig");

pub const EvalError = error{
    Crash,
    OutOfMemory,
    StackOverflow,
    LayoutError,
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
            // For now, write the tag index as a u16
            _ = tag;
            const tag_ptr = @as(*u16, @ptrCast(@alignCast(ptr)));
            tag_ptr.* = 0; // TODO: get actual tag discriminant
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

        // If expressions - evaluate the final_else branch
        .@"if" => |if_expr| {
            // Recursively evaluate the final_else branch
            return eval(allocator, cir, if_expr.final_else, eval_stack, layout_cache);
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

test {
    _ = @import("eval_test.zig");
}
