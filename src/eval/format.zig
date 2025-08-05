//! Stack result formatting utilities for the interpreter
//! Provides functions to format interpreter results into human-readable strings

const std = @import("std");
const builtins = @import("builtins");
const eval = @import("interpreter.zig");
const layout_store = @import("../layout/store.zig");

const RocStr = builtins.str.RocStr;
const RocOps = builtins.host_abi.RocOps;

/// Buffer size for formatting results
pub const RESULT_BUFFER_SIZE = 1024;

/// Format a stack result into a human-readable string
pub fn formatStackResult(
    stack_result: eval.Interpreter.StackValue,
    layout_cache: *layout_store.Store,
    buf: []u8,
    ops: *RocOps,
) []const u8 {
    if (stack_result.layout.tag == .scalar) {
        return formatScalar(stack_result, buf, ops);
    } else if (stack_result.layout.tag == .record) {
        return formatRecord(stack_result, layout_cache, buf);
    } else if (stack_result.layout.tag == .tuple) {
        return formatTuple(stack_result, layout_cache, buf);
    } else if (stack_result.layout.tag == .list) {
        return formatList(stack_result, buf);
    } else if (stack_result.layout.tag == .list_of_zst) {
        return formatListOfZst(stack_result, buf);
    } else if (stack_result.layout.tag == .box) {
        return formatBox(buf);
    } else if (stack_result.layout.tag == .box_of_zst) {
        return formatBoxOfZst(buf);
    } else if (stack_result.layout.tag == .closure) {
        return formatClosure(buf);
    } else {
        return std.fmt.bufPrint(buf, "Unknown layout type: {}", .{stack_result.layout.tag}) catch "Error";
    }
}

/// Format scalar values (numbers, booleans, strings, etc.)
fn formatScalar(stack_result: eval.Interpreter.StackValue, buf: []u8, ops: *RocOps) []const u8 {
    const scalar_data = stack_result.layout.data.scalar;
    
    switch (scalar_data.tag) {
        .int => {
            const precision = scalar_data.data.int;
            const int_val = eval.readIntFromMemory(@ptrCast(stack_result.ptr.?), precision);
            return std.fmt.bufPrint(buf, "{}", .{int_val}) catch "Error formatting";
        },
        .bool => {
            const bool_val = @as(*const bool, @ptrCast(@alignCast(stack_result.ptr.?))).*;
            return std.fmt.bufPrint(buf, "{}", .{bool_val}) catch "Error formatting";
        },
        .frac => {
            return formatFloat(stack_result, scalar_data.data.frac, buf, ops);
        },
        .str => {
            const str_ptr = @as(*const RocStr, @ptrCast(@alignCast(stack_result.ptr.?)));
            return std.fmt.bufPrint(buf, "{s}", .{str_ptr.asSlice()}) catch "Error formatting";
        },
        .opaque_ptr => {
            return formatOpaquePtr(stack_result, buf);
        },
    }
}

/// Format floating point values
fn formatFloat(stack_result: eval.Interpreter.StackValue, float_precision: anytype, buf: []u8, ops: *RocOps) []const u8 {
    switch (float_precision) {
        .f32 => {
            const float_val = @as(*const f32, @ptrCast(@alignCast(stack_result.ptr.?))).*;
            return std.fmt.bufPrint(buf, "{d}", .{float_val}) catch "Error formatting";
        },
        .f64 => {
            const float_val = @as(*const f64, @ptrCast(@alignCast(stack_result.ptr.?))).*;
            return std.fmt.bufPrint(buf, "{d}", .{float_val}) catch "Error formatting";
        },
        .dec => {
            // Decimal is a 128-bit fixed-point number with 18 decimal places
            const dec_ptr = @as(*const builtins.dec.RocDec, @ptrCast(@alignCast(stack_result.ptr.?)));
            const dec_str = dec_ptr.to_str(ops);
            defer dec_str.decref(ops);
            return std.fmt.bufPrint(buf, "{s}", .{dec_str.asSlice()}) catch "Error formatting";
        },
    }
}

/// Format opaque pointer values
fn formatOpaquePtr(stack_result: eval.Interpreter.StackValue, buf: []u8) []const u8 {
    const ptr_val = @as(*const ?*anyopaque, @ptrCast(@alignCast(stack_result.ptr.?))).*;
    if (ptr_val) |ptr| {
        return std.fmt.bufPrint(buf, "<opaque pointer: 0x{x}>", .{@intFromPtr(ptr)}) catch "Error formatting";
    } else {
        return std.fmt.bufPrint(buf, "<null opaque pointer>", .{}) catch "Error formatting";
    }
}

/// Format record values
fn formatRecord(stack_result: eval.Interpreter.StackValue, layout_cache: *layout_store.Store, buf: []u8) []const u8 {
    const record_data = layout_cache.getRecordData(stack_result.layout.data.record.idx);
    const num_fields = record_data.fields.count;
    return std.fmt.bufPrint(buf, "<record with {} fields, size {} bytes>", .{ num_fields, record_data.size }) catch "Error";
}

/// Format tuple values
fn formatTuple(stack_result: eval.Interpreter.StackValue, layout_cache: *layout_store.Store, buf: []u8) []const u8 {
    const tuple_data = layout_cache.getTupleData(stack_result.layout.data.tuple.idx);
    const num_elems = tuple_data.fields.count;
    return std.fmt.bufPrint(buf, "<tuple with {} elements, size {} bytes>", .{ num_elems, tuple_data.size }) catch "Error";
}

/// Format list values
fn formatList(stack_result: eval.Interpreter.StackValue, buf: []u8) []const u8 {
    const list_ptr = @as(*const builtins.list.RocList, @ptrCast(@alignCast(stack_result.ptr.?)));
    return std.fmt.bufPrint(buf, "<list with {} elements>", .{list_ptr.len()}) catch "Error";
}

/// Format list of zero-sized types
fn formatListOfZst(stack_result: eval.Interpreter.StackValue, buf: []u8) []const u8 {
    const list_ptr = @as(*const builtins.list.RocList, @ptrCast(@alignCast(stack_result.ptr.?)));
    return std.fmt.bufPrint(buf, "<list of {} zero-sized elements>", .{list_ptr.len()}) catch "Error";
}

/// Format box values
fn formatBox(buf: []u8) []const u8 {
    return std.fmt.bufPrint(buf, "<box>", .{}) catch "Error";
}

/// Format box of zero-sized type
fn formatBoxOfZst(buf: []u8) []const u8 {
    return std.fmt.bufPrint(buf, "<box of zero-sized type>", .{}) catch "Error";
}

/// Format closure values
fn formatClosure(buf: []u8) []const u8 {
    return std.fmt.bufPrint(buf, "<closure>", .{}) catch "Error";
}