//! LLVM Object File Execution Module
//!
//! This module handles the execution of LLVM-compiled object files and result formatting.
//! It extracts the code section from object files and executes it using the same JIT
//! infrastructure as the dev backend (mmap/mprotect + function pointer call).
//!
//! The typical workflow is:
//! 1. LLVM evaluator generates bitcode
//! 2. llvm_compile compiles bitcode to object file
//! 3. This module extracts code, executes it, and formats the result

const std = @import("std");
const backend = @import("backend");
const object_reader = backend.object_reader;
const layout_mod = @import("layout");

const Allocator = std.mem.Allocator;
const JitCode = backend.JitCode;
const LayoutIdx = layout_mod.Idx;

/// Errors that can occur during LLVM object file execution.
pub const Error = error{
    OutOfMemory,
    /// Failed to extract code section from object file
    InvalidObjectFile,
    TextSectionNotFound,
    UnsupportedFormat,
    /// JIT execution failed
    JitExecutionFailed,
    /// The layout is not supported for execution
    UnsupportedLayout,
};

/// Dec (fixed-point decimal) has 18 decimal places.
/// The internal representation is i128 scaled by 10^18.
const dec_decimal_places: u5 = 18;
const dec_scale_factor: i128 = blk: {
    var result: i128 = 1;
    for (0..dec_decimal_places) |_| {
        result *= 10;
    }
    break :blk result;
};

/// Format a Dec value (i128 with 18 decimal places) as a string.
pub fn formatDec(allocator: Allocator, num: i128) Allocator.Error![]const u8 {
    if (num == 0) {
        return try allocator.dupe(u8, "0");
    }

    var out = std.array_list.Managed(u8).init(allocator);
    errdefer out.deinit();

    const is_negative = num < 0;
    // Use @abs which handles i128 min correctly by returning u128
    const abs_value: u128 = @abs(num);

    if (is_negative) {
        try out.append('-');
    }

    const integer_part = @divTrunc(abs_value, @as(u128, @intCast(dec_scale_factor)));
    const fractional_part = @rem(abs_value, @as(u128, @intCast(dec_scale_factor)));

    // Format integer part
    var int_buf: [40]u8 = undefined;
    const int_str = std.fmt.bufPrint(&int_buf, "{d}", .{integer_part}) catch unreachable;
    try out.appendSlice(int_str);

    if (fractional_part == 0) {
        return try out.toOwnedSlice();
    }

    try out.append('.');

    // Format fractional part with leading zeros preserved
    var digits: [dec_decimal_places]u8 = undefined;
    @memset(&digits, '0');
    var remaining = fractional_part;
    var idx: usize = dec_decimal_places;
    while (idx > 0) : (idx -= 1) {
        const digit: u8 = @intCast(@mod(remaining, 10));
        digits[idx - 1] = digit + '0';
        remaining = @divTrunc(remaining, 10);
    }

    // Trim trailing zeros
    var end: usize = dec_decimal_places;
    while (end > 1 and digits[end - 1] == '0') {
        end -= 1;
    }

    try out.appendSlice(digits[0..end]);
    return try out.toOwnedSlice();
}

/// Execute code from an object file and format the result.
///
/// This extracts the .text section from the object file, loads it into
/// executable memory, calls it as a function, and formats the result
/// based on the output layout.
pub fn executeAndFormat(
    allocator: Allocator,
    object_bytes: []const u8,
    output_layout: LayoutIdx,
    is_list: bool,
    is_record: bool,
    record_field_names: ?[]const u8,
) Error![]const u8 {
    // Extract the code section from the object file
    const code = object_reader.extractCodeSection(object_bytes) catch |err| switch (err) {
        error.InvalidObjectFile => return Error.InvalidObjectFile,
        error.TextSectionNotFound => return Error.TextSectionNotFound,
        error.UnsupportedFormat => return Error.UnsupportedFormat,
    };

    // Load code into executable memory
    var jit = JitCode.init(code) catch return Error.JitExecutionFailed;
    defer jit.deinit();

    // Execute and format based on output type
    return formatResult(allocator, &jit, output_layout, is_list, is_record, record_field_names);
}

/// Format the result of a JIT-executed function.
fn formatResult(
    allocator: Allocator,
    jit: *const JitCode,
    output_layout: LayoutIdx,
    is_list: bool,
    is_record: bool,
    record_field_names: ?[]const u8,
) Error![]const u8 {
    // Handle list output specially
    if (is_list) {
        return formatListResult(allocator, jit);
    }

    // Handle record output specially
    if (is_record) {
        return formatRecordResult(allocator, jit, record_field_names);
    }

    // Format scalar types
    return formatScalarResult(allocator, jit, output_layout);
}

/// Format a list result.
fn formatListResult(allocator: Allocator, jit: *const JitCode) Error![]const u8 {
    // Max 16 elements (8 bytes length + 16*8 bytes elements = 136 bytes)
    var result_buffer: [136]u8 = undefined;
    jit.callWithResultPtr(&result_buffer);

    // Read the length (first 8 bytes)
    const length = std.mem.readInt(u64, result_buffer[0..8], .little);
    if (length == 0) {
        return allocator.dupe(u8, "[]") catch return Error.OutOfMemory;
    }

    // Format: [elem0, elem1, ...]
    var output = std.ArrayListUnmanaged(u8){};
    output.append(allocator, '[') catch return Error.OutOfMemory;

    var i: usize = 0;
    while (i < length and i < 16) : (i += 1) {
        if (i > 0) {
            output.appendSlice(allocator, ", ") catch return Error.OutOfMemory;
        }
        // Read element at offset 8 + i*8
        const offset = 8 + i * 8;
        const value = std.mem.readInt(i64, result_buffer[offset..][0..8], .little);
        const value_str = std.fmt.allocPrint(allocator, "{d}", .{value}) catch return Error.OutOfMemory;
        defer allocator.free(value_str);
        output.appendSlice(allocator, value_str) catch return Error.OutOfMemory;
    }

    output.append(allocator, ']') catch return Error.OutOfMemory;
    return output.toOwnedSlice(allocator) catch return Error.OutOfMemory;
}

/// Format a record result.
fn formatRecordResult(
    allocator: Allocator,
    jit: *const JitCode,
    record_field_names: ?[]const u8,
) Error![]const u8 {
    const field_names = record_field_names orelse "";
    if (field_names.len == 0) {
        return allocator.dupe(u8, "{}") catch return Error.OutOfMemory;
    }

    // Count fields
    var field_count: usize = 1;
    for (field_names) |c| {
        if (c == ',') field_count += 1;
    }

    // Allocate buffer for field values (8 bytes per field)
    const buffer_size = field_count * 8;
    var result_buffer: [64]u8 = undefined; // Max 8 fields
    if (buffer_size > result_buffer.len) {
        return allocator.dupe(u8, "{ <too many fields> }") catch return Error.OutOfMemory;
    }

    // Call the function
    jit.callWithResultPtr(&result_buffer);

    // Format output: { a: 10, b: 20, c: 30 }
    var output = std.ArrayListUnmanaged(u8){};
    output.appendSlice(allocator, "{ ") catch return Error.OutOfMemory;

    var name_iter = std.mem.splitScalar(u8, field_names, ',');
    var field_idx: usize = 0;
    while (name_iter.next()) |name| : (field_idx += 1) {
        if (field_idx > 0) {
            output.appendSlice(allocator, ", ") catch return Error.OutOfMemory;
        }
        output.appendSlice(allocator, name) catch return Error.OutOfMemory;
        output.appendSlice(allocator, ": ") catch return Error.OutOfMemory;

        // Read the i64 value at this offset
        const offset = field_idx * 8;
        const value = std.mem.readInt(i64, result_buffer[offset..][0..8], .little);
        const value_str = std.fmt.allocPrint(allocator, "{d}", .{value}) catch return Error.OutOfMemory;
        defer allocator.free(value_str);
        output.appendSlice(allocator, value_str) catch return Error.OutOfMemory;
    }

    output.appendSlice(allocator, " }") catch return Error.OutOfMemory;
    return output.toOwnedSlice(allocator) catch return Error.OutOfMemory;
}

/// Format a scalar result based on layout.
fn formatScalarResult(
    allocator: Allocator,
    jit: *const JitCode,
    output_layout: LayoutIdx,
) Error![]const u8 {
    switch (output_layout) {
        // Signed integers that fit in i64
        .i8, .i16, .i32, .i64 => {
            var result: i64 = undefined;
            jit.callWithResultPtr(&result);
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return Error.OutOfMemory;
        },
        // Unsigned integers that fit in u64
        .u8, .u16, .u32, .u64 => {
            var result: u64 = undefined;
            jit.callWithResultPtr(&result);
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return Error.OutOfMemory;
        },
        .i128 => {
            var result: i128 = undefined;
            jit.callWithResultPtr(&result);
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return Error.OutOfMemory;
        },
        .u128 => {
            var result: i128 = undefined;
            jit.callWithResultPtr(&result);
            return std.fmt.allocPrint(allocator, "{d}", .{@as(u128, @bitCast(result))}) catch return Error.OutOfMemory;
        },
        .f32, .f64 => {
            var result: f64 = undefined;
            jit.callWithResultPtr(&result);
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return Error.OutOfMemory;
        },
        .dec => {
            var result: i128 = undefined;
            jit.callWithResultPtr(&result);
            return formatDec(allocator, result) catch return Error.OutOfMemory;
        },
        .bool => {
            // Bool is stored as i64 (extended from i1/i8) in the eval function
            var result: i64 = undefined;
            jit.callWithResultPtr(&result);
            const str = if (result != 0) "True" else "False";
            return allocator.dupe(u8, str) catch return Error.OutOfMemory;
        },
        .str => {
            // RocStr is a 24-byte struct on 64-bit platforms
            // For small strings (< 24 bytes), content is stored inline with length in last byte
            const RocStrBytes = [24]u8;
            var result: RocStrBytes = undefined;
            jit.callWithResultPtr(&result);

            // Check if it's a small string (last byte has high bit set)
            const last_byte = result[23];
            if (last_byte & 0x80 != 0) {
                // Small string - length is in the last byte (without high bit)
                const length = last_byte & 0x7F;
                const content = result[0..length];
                // Format as quoted string
                return std.fmt.allocPrint(allocator, "\"{s}\"", .{content}) catch return Error.OutOfMemory;
            } else {
                // Big string - we'd need to dereference a pointer which is complex
                // For now, return a placeholder
                return allocator.dupe(u8, "\"<heap string>\"") catch return Error.OutOfMemory;
            }
        },
        else => return Error.UnsupportedLayout,
    }
}
