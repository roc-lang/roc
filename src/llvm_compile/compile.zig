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

const std = @import("std");
const builtin = @import("builtin");
const bindings = @import("bindings.zig");

const Allocator = std.mem.Allocator;

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
    is_float: bool,
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

    // Call the function and format the result
    if (is_float) {
        // Function returns f64
        const EvalFn = *const fn () callconv(.c) f64;
        const eval_fn: EvalFn = @ptrFromInt(eval_addr);
        const result = eval_fn();

        // Format the float result
        return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return error.OutOfMemory;
    } else {
        // Function returns i64
        const EvalFn = *const fn () callconv(.c) i64;
        const eval_fn: EvalFn = @ptrFromInt(eval_addr);
        const result = eval_fn();

        // Format the integer result
        return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return error.OutOfMemory;
    }
}
