//! LLVM JIT Compilation and Execution Module
//!
//! This module provides JIT-based LLVM execution:
//! 1. Parse bitcode into an LLVM module
//! 2. JIT compile using LLVM ORC
//! 3. Call the roc_eval function directly and format the result
//!
//! This approach avoids the LLD linker entirely, which is important because
//! LLD's Mach-O port has global state issues that cause corruption when
//! invoked multiple times in the same process.
//!
//! ## i128 ABI Considerations
//!
//! When the roc_eval function returns i128/u128, the calling convention is
//! handled by Zig and LLVM automatically. Since we generate the LLVM IR using
//! Zig's LLVM builder and call it from Zig code, both sides use the same ABI.
//!
//! Platform-specific i128 representations (<2 x i64> on Windows, [2 x i64] on
//! macOS ARM64) would only be relevant when calling into *separately compiled*
//! Zig builtins. When that support is added, appropriate ABI conversion helpers
//! will need to be implemented.

const std = @import("std");
const builtin = @import("builtin");
const bindings = @import("bindings.zig");

const Allocator = std.mem.Allocator;

/// Type of the result value for JIT execution.
///
/// NOTE: This enum is duplicated in llvm_evaluator.zig because of module boundary
/// constraints. The eval module may be compiled without LLVM linked, while this
/// module (llvm_compile) requires LLVM. The two definitions must stay in sync,
/// which is enforced by comptime validation in both files.
pub const ResultType = enum {
    i64,
    u64,
    i128,
    u128,
    f64,
    dec,

    /// Compile-time validation that this enum matches the expected structure.
    /// This helps catch accidental divergence from llvm_evaluator.ResultType.
    pub fn validate() void {
        comptime {
            const fields = @typeInfo(ResultType).@"enum".fields;
            if (fields.len != 6) @compileError("ResultType must have exactly 6 variants");
            if (!std.mem.eql(u8, fields[0].name, "i64")) @compileError("ResultType[0] must be i64");
            if (!std.mem.eql(u8, fields[1].name, "u64")) @compileError("ResultType[1] must be u64");
            if (!std.mem.eql(u8, fields[2].name, "i128")) @compileError("ResultType[2] must be i128");
            if (!std.mem.eql(u8, fields[3].name, "u128")) @compileError("ResultType[3] must be u128");
            if (!std.mem.eql(u8, fields[4].name, "f64")) @compileError("ResultType[4] must be f64");
            if (!std.mem.eql(u8, fields[5].name, "dec")) @compileError("ResultType[5] must be dec");
        }
    }
};

comptime {
    ResultType.validate();
}

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
fn formatDec(allocator: Allocator, num: i128) Allocator.Error![]const u8 {
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

/// Errors that can occur during LLVM JIT compilation and execution.
pub const Error = error{
    OutOfMemory,
    JITCreationFailed,
    ModuleAddFailed,
    SymbolNotFound,
    ExecutionFailed,
    BitcodeParseError,
};

/// JIT compile LLVM bitcode and execute it, returning the formatted result
pub fn compileAndExecute(
    allocator: Allocator,
    bitcode: []const u32,
    result_type: ResultType,
) Error![]const u8 {
    // Convert u32 slice to u8 slice for the bindings
    const bitcode_bytes: []const u8 = @as([*]const u8, @ptrCast(bitcode.ptr))[0 .. bitcode.len * 4];

    // Initialize all targets (needed for JIT)
    bindings.initializeAllTargets();

    // Create thread-safe context
    // Note: We don't dispose the context here - the JIT takes ownership through the ThreadSafeModule
    const ts_context = bindings.OrcThreadSafeContext.create();

    // Get the underlying LLVM context for parsing
    const context = ts_context.getContext();

    // Create memory buffer from bitcode
    const mem_buf = bindings.MemoryBuffer.createMemoryBufferWithMemoryRange(
        bitcode_bytes.ptr,
        bitcode_bytes.len,
        "roc_bitcode",
        bindings.Bool.False,
    );

    // Parse bitcode into module (don't defer dispose - ownership transfers to ThreadSafeModule)
    var module: *bindings.Module = undefined;
    const parse_result = context.parseBitcodeInContext2(mem_buf, &module);
    if (parse_result.toBool()) {
        mem_buf.dispose();
        return error.BitcodeParseError;
    }
    // Note: mem_buf is consumed by parseBitcodeInContext2, don't dispose

    // Wrap module in thread-safe module (takes ownership of module)
    const ts_module = bindings.OrcThreadSafeModule.create(module, ts_context);
    // Note: Don't dispose ts_module - ownership transfers to LLJIT

    // Create LLJIT builder
    const builder = bindings.OrcLLJITBuilder.create();
    // Note: builder is consumed by createLLJIT

    // Create LLJIT instance
    var jit: *bindings.OrcLLJIT = undefined;
    if (bindings.createLLJIT(&jit, builder)) |err| {
        bindings.consumeError(err);
        ts_module.dispose();
        return error.JITCreationFailed;
    }
    defer {
        if (jit.dispose()) |err| {
            bindings.consumeError(err);
        }
    }

    // Get main JIT dylib
    const dylib = jit.getMainJITDylib();

    // Add a generator so the JIT can find symbols from the current process
    // (not needed for roc_eval since it doesn't call external functions,
    // but keep it for future flexibility)
    const global_prefix: u8 = if (builtin.os.tag == .macos) '_' else 0;
    var process_syms_generator: *bindings.OrcDefinitionGenerator = undefined;
    if (bindings.createDynamicLibrarySearchGeneratorForProcess(
        &process_syms_generator,
        global_prefix,
        null, // no filter
        null, // no filter context
    )) |err| {
        bindings.consumeError(err);
        return error.JITCreationFailed;
    }
    bindings.jitDylibAddGenerator(dylib, process_syms_generator);

    // Add module to JIT (takes ownership of ts_module)
    if (jit.addLLVMIRModule(dylib, ts_module)) |err| {
        bindings.consumeError(err);
        return error.ModuleAddFailed;
    }

    // Look up the roc_eval function
    // The bitcode uses \x01_roc_eval on macOS (which becomes _roc_eval verbatim) or roc_eval elsewhere
    const symbol_name: [*:0]const u8 = if (builtin.os.tag == .macos) "_roc_eval" else "roc_eval";

    var eval_addr: bindings.OrcExecutorAddress = 0;
    if (jit.lookup(&eval_addr, symbol_name)) |err| {
        bindings.consumeError(err);
        return error.SymbolNotFound;
    }

    if (eval_addr == 0) {
        return error.SymbolNotFound;
    }

    // Call the function and format the result based on the return type
    switch (result_type) {
        .f64 => {
            // Function returns f64
            const EvalFn = *const fn () callconv(.c) f64;
            const eval_fn: EvalFn = @ptrFromInt(eval_addr);
            const result = eval_fn();

            // Format the float result
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return error.OutOfMemory;
        },
        .i128 => {
            // Function returns i128 (signed)
            const EvalFn = *const fn () callconv(.c) i128;
            const eval_fn: EvalFn = @ptrFromInt(eval_addr);
            const result = eval_fn();

            // Format the i128 result
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return error.OutOfMemory;
        },
        .u128 => {
            // Function returns u128 (unsigned)
            // At the LLVM level, i128 and u128 have the same representation,
            // but we interpret the bits as unsigned here
            const EvalFn = *const fn () callconv(.c) u128;
            const eval_fn: EvalFn = @ptrFromInt(eval_addr);
            const result = eval_fn();

            // Format the u128 result
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return error.OutOfMemory;
        },
        .i64 => {
            // Function returns i64 (signed)
            const EvalFn = *const fn () callconv(.c) i64;
            const eval_fn: EvalFn = @ptrFromInt(eval_addr);
            const result = eval_fn();

            // Format the integer result
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return error.OutOfMemory;
        },
        .u64 => {
            // Function returns u64 (unsigned)
            // At the LLVM level, i64 and u64 have the same representation,
            // but we interpret the bits as unsigned here
            const EvalFn = *const fn () callconv(.c) u64;
            const eval_fn: EvalFn = @ptrFromInt(eval_addr);
            const result = eval_fn();

            // Format the u64 result
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return error.OutOfMemory;
        },
        .dec => {
            // Function returns i128 representing a Dec (fixed-point with 18 decimal places)
            const EvalFn = *const fn () callconv(.c) i128;
            const eval_fn: EvalFn = @ptrFromInt(eval_addr);
            const result = eval_fn();

            // Format the Dec result with proper decimal formatting
            return formatDec(allocator, result) catch return error.OutOfMemory;
        },
    }
}
