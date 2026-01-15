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
//! macOS ARM64) are handled by the I128Arg type and associated helper functions
//! (normalizeI128Return, prepareI128Arg, etc.). These helpers should be used
//! when calling into separately compiled Zig builtins that use i128/u128 types.

const std = @import("std");
const builtin = @import("builtin");
const bindings = @import("bindings.zig");
const layout = @import("layout");

const Allocator = std.mem.Allocator;
const LayoutIdx = layout.Idx;

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

// Platform-specific i128 ABI Handling
//
// When calling into separately compiled Zig builtins (e.g., Dec operations),
// the i128 type may be represented differently depending on the platform:
//
//   - Windows:     <2 x i64> vector type (MSVC doesn't support native i128)
//   - macOS ARM64: [2 x i64] array type (ARM64 ABI convention)
//   - Other:       Native i128 type
//
// These helpers handle the conversion between platform-specific representations
// and native i128/u128 types. They are used when:
//   1. Calling a builtin function that returns i128 (normalize return value)
//   2. Passing i128 arguments to a builtin function (convert to expected ABI)
//
// Note: For JIT-compiled code where we generate and call functions using
// Zig's LLVM builder, the ABI is already consistent. These helpers are only
// needed when interfacing with separately compiled Zig bitcode.

/// Represents an i128 value in the platform-specific ABI format.
/// On most platforms this is just i128, but Windows uses a 2xi64 vector
/// and macOS ARM64 uses a 2xi64 array.
pub const I128Arg = switch (builtin.os.tag) {
    .windows => extern struct { lo: u64, hi: u64 },
    .macos => if (builtin.cpu.arch == .aarch64)
        extern struct { lo: u64, hi: u64 }
    else
        i128,
    else => i128,
};

/// Convert a platform-specific i128 representation to native i128.
/// Used when receiving i128 return values from separately compiled Zig builtins.
pub fn normalizeI128Return(arg: I128Arg) i128 {
    return switch (builtin.os.tag) {
        .windows => @as(i128, arg.hi) << 64 | @as(i128, arg.lo),
        .macos => if (builtin.cpu.arch == .aarch64)
            @as(i128, arg.hi) << 64 | @as(i128, arg.lo)
        else
            arg,
        else => arg,
    };
}

/// Convert a native i128 to the platform-specific representation.
/// Used when passing i128 arguments to separately compiled Zig builtins.
pub fn prepareI128Arg(value: i128) I128Arg {
    return switch (builtin.os.tag) {
        .windows => .{
            .lo = @truncate(@as(u128, @bitCast(value))),
            .hi = @truncate(@as(u128, @bitCast(value)) >> 64),
        },
        .macos => if (builtin.cpu.arch == .aarch64) .{
            .lo = @truncate(@as(u128, @bitCast(value))),
            .hi = @truncate(@as(u128, @bitCast(value)) >> 64),
        } else value,
        else => value,
    };
}

/// Convert a platform-specific u128 representation to native u128.
/// Used when receiving u128 return values from separately compiled Zig builtins.
pub fn normalizeU128Return(arg: I128Arg) u128 {
    return @bitCast(normalizeI128Return(arg));
}

/// Convert a native u128 to the platform-specific representation.
/// Used when passing u128 arguments to separately compiled Zig builtins.
pub fn prepareU128Arg(value: u128) I128Arg {
    return prepareI128Arg(@bitCast(value));
}

/// Errors that can occur during LLVM JIT compilation and execution.
pub const Error = error{
    OutOfMemory,
    JITCreationFailed,
    ModuleAddFailed,
    ModuleLinkFailed,
    SymbolNotFound,
    ExecutionFailed,
    BitcodeParseError,
    /// JIT is not supported on this platform (e.g., statically-linked musl binaries
    /// don't support dynamic loading which LLVM ORC JIT requires).
    JITNotSupported,
    /// The layout is not supported for JIT execution (e.g., non-scalar types).
    UnsupportedLayout,
};

/// Returns true if JIT compilation is supported on this platform.
///
/// LLVM ORC JIT requires dynamic loading (`dlopen`/`dlsym`). On statically-linked
/// musl binaries, musl intentionally stubs out `dlopen` to always fail:
///
/// ```c
/// static void *stub_dlopen(const char *file, int mode) {
///     __dl_seterr("Dynamic loading not supported");
///     return 0;
/// }
/// ```
///
/// This is by design - if a statically-linked binary loaded a dynamic library,
/// that library would need its own libc (you can't share the statically-linked
/// libc with dynamically loaded code), causing issues with global state and
/// symbol tables.
///
/// See: https://www.openwall.com/lists/musl/2017/05/17/2
pub fn isJITSupported() bool {
    return comptime !(builtin.abi == .musl or builtin.abi == .musleabi or builtin.abi == .musleabihf);
}

/// JIT compile LLVM bitcode and execute it, returning the formatted result.
/// Returns `error.JITNotSupported` on platforms where JIT is not available
/// (e.g., statically-linked musl binaries).
pub fn compileAndExecute(
    allocator: Allocator,
    bitcode: []const u32,
    output_layout: LayoutIdx,
    is_list: bool,
) Error![]const u8 {
    // LLVM ORC JIT requires dynamic loading, which is not available on
    // statically-linked musl binaries.
    if (comptime !isJITSupported()) {
        return error.JITNotSupported;
    }

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

    // Load and merge builtin bitcode into the user module.
    // This makes all builtin functions available for the JIT to call.
    // The builtins are compiled with single_threaded=true to avoid TLS symbols.
    {
        const builtin_bitcode = @embedFile("builtins.bc");
        const builtin_mem_buf = bindings.MemoryBuffer.createMemoryBufferWithMemoryRange(
            builtin_bitcode.ptr,
            builtin_bitcode.len,
            "roc_builtins",
            bindings.Bool.False,
        );

        var builtin_module: *bindings.Module = undefined;
        if (context.parseBitcodeInContext2(builtin_mem_buf, &builtin_module).toBool()) {
            builtin_mem_buf.dispose();
            module.dispose();
            return error.BitcodeParseError;
        }
        // Note: builtin_mem_buf is consumed by parseBitcodeInContext2

        // Set the builtin module's target triple and data layout to match the user module.
        // This prevents "Linking two modules of different target triples" warnings and
        // ensures the merged module is compatible with the JIT.
        builtin_module.setTargetTriple(module.getTargetTriple());
        builtin_module.setDataLayout(module.getDataLayout());

        // Link builtins into user module (destroys builtin_module on success)
        if (module.link(builtin_module).toBool()) {
            module.dispose();
            return error.ModuleLinkFailed;
        }
        // Note: builtin_module is now invalid - do NOT dispose it
    }

    // Wrap module in thread-safe module (takes ownership of module)
    const ts_module = bindings.OrcThreadSafeModule.create(module, ts_context);
    // Note: Don't dispose ts_module - ownership transfers to LLJIT

    // Create LLJIT builder
    const builder = bindings.OrcLLJITBuilder.create();
    // Note: builder is consumed by createLLJIT

    // Configure the JIT to use a baseline CPU target to avoid issues with
    // unrecognized CPU features on various platforms.
    // LLVM's default host detection may return CPU names/features that
    // the bundled LLVM version doesn't fully support.
    {
        // Get the host target triple
        const triple = bindings.GetDefaultTargetTriple();
        defer bindings.disposeMessage(triple);

        // Get the target from the triple
        var target: *bindings.Target = undefined;
        var error_message: [*:0]const u8 = undefined;
        if (bindings.Target.getFromTriple(triple, &target, &error_message).toBool()) {
            bindings.disposeMessage(error_message);
            ts_module.dispose();
            return error.JITCreationFailed;
        }

        // Use a baseline CPU that has the required features for each architecture.
        // The CPU name must enable required features for the platform's calling convention.
        //
        // x86_64: "x86-64" enables SSE2 which is required for the Windows x64 ABI
        //         (floats are returned in XMM0). "generic" doesn't guarantee SSE2.
        // x86:    "pentium4" enables SSE2 for consistent float handling.
        //         "i686" only guarantees SSE, not SSE2.
        // aarch64: "generic" is fine - NEON/FP are part of the base AArch64 architecture.
        // arm:    "generic" works, but hard-float targets need VFP which is enabled
        //         by the target triple's ABI suffix (gnueabihf/musleabihf).
        // Other:  "generic" as a safe fallback.
        const cpu: [*:0]const u8 = switch (builtin.cpu.arch) {
            .x86_64 => "x86-64",
            .x86 => "pentium4",
            else => "generic",
        };

        // Create a target machine with the baseline CPU and no specific extra features.
        // This avoids issues where LLVM detects CPU features it doesn't fully support.
        const target_machine = bindings.TargetMachine.create(
            target,
            triple,
            cpu,
            "", // No specific extra features
            .Default, // optimization level
            .Default, // reloc mode
            .Default, // code model
            false, // function_sections (not needed for JIT)
            false, // data_sections (not needed for JIT)
            .Default, // float_abi
            null, // abi_name
            false, // emulated_tls
        );

        // Create JIT target machine builder from the target machine
        // This takes ownership of the target machine
        const jtmb = bindings.OrcJITTargetMachineBuilder.createFromTargetMachine(target_machine);

        // Set the target machine builder on the LLJIT builder
        builder.setJITTargetMachineBuilder(jtmb);
    }

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

    // Add a generator so the JIT can find symbols from the current process.
    // This handles:
    // - Platform symbols (TLS, compiler-rt, etc.)
    // - Builtin functions (until bitcode merging is enabled)
    const global_prefix: u8 = if (builtin.os.tag == .macos) '_' else 0;
    var process_syms_generator: *bindings.OrcDefinitionGenerator = undefined;
    if (bindings.createDynamicLibrarySearchGeneratorForProcess(
        &process_syms_generator,
        global_prefix,
        null, // no filter
        null, // no filter context
    )) |err| {
        bindings.consumeError(err);
        ts_module.dispose();
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

    // Call the function and format the result.
    //
    // Following Roc's host ABI (see src/builtins/host_abi.zig), all functions
    // exposed to the host return void and write their result to a pointer.
    // This makes the ABI simple and platform-independent on all targets.
    //
    // Function signature: void roc_eval(<type>* out_ptr)

    // Handle list output specially
    if (is_list) {
        // RocList is a 24-byte struct on 64-bit platforms
        // { bytes: *u8, length: usize, capacity: usize }
        const RocListBytes = [24]u8;
        const EvalFn = *const fn (*RocListBytes) callconv(.c) void;
        const eval_fn: EvalFn = @ptrFromInt(@as(usize, @intCast(eval_addr)));
        var result: RocListBytes = undefined;
        eval_fn(&result);

        // Read the length (second 8 bytes in the struct)
        const length = std.mem.readInt(u64, result[8..16], .little);
        if (length == 0) {
            return allocator.dupe(u8, "[]") catch return error.OutOfMemory;
        } else {
            // Non-empty list - not yet supported
            return allocator.dupe(u8, "[<list elements>]") catch return error.OutOfMemory;
        }
    }

    switch (output_layout) {
        // Signed integers that fit in i64
        .i8, .i16, .i32, .i64 => {
            const EvalFn = *const fn (*i64) callconv(.c) void;
            const eval_fn: EvalFn = @ptrFromInt(@as(usize, @intCast(eval_addr)));
            var result: i64 = undefined;
            eval_fn(&result);
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return error.OutOfMemory;
        },
        // Unsigned integers that fit in u64
        .u8, .u16, .u32, .u64 => {
            const EvalFn = *const fn (*u64) callconv(.c) void;
            const eval_fn: EvalFn = @ptrFromInt(@as(usize, @intCast(eval_addr)));
            var result: u64 = undefined;
            eval_fn(&result);
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return error.OutOfMemory;
        },
        .i128 => {
            const EvalFn = *const fn (*i128) callconv(.c) void;
            const eval_fn: EvalFn = @ptrFromInt(@as(usize, @intCast(eval_addr)));
            var result: i128 = undefined;
            eval_fn(&result);
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return error.OutOfMemory;
        },
        .u128 => {
            const EvalFn = *const fn (*i128) callconv(.c) void;
            const eval_fn: EvalFn = @ptrFromInt(@as(usize, @intCast(eval_addr)));
            var result: i128 = undefined;
            eval_fn(&result);
            return std.fmt.allocPrint(allocator, "{d}", .{@as(u128, @bitCast(result))}) catch return error.OutOfMemory;
        },
        .f32, .f64 => {
            const EvalFn = *const fn (*f64) callconv(.c) void;
            const eval_fn: EvalFn = @ptrFromInt(@as(usize, @intCast(eval_addr)));
            var result: f64 = undefined;
            eval_fn(&result);
            return std.fmt.allocPrint(allocator, "{d}", .{result}) catch return error.OutOfMemory;
        },
        .dec => {
            const EvalFn = *const fn (*i128) callconv(.c) void;
            const eval_fn: EvalFn = @ptrFromInt(@as(usize, @intCast(eval_addr)));
            var result: i128 = undefined;
            eval_fn(&result);
            return formatDec(allocator, result) catch return error.OutOfMemory;
        },
        .bool => {
            // Bool is stored as i64 (extended from i1/i8) in the eval function
            const EvalFn = *const fn (*i64) callconv(.c) void;
            const eval_fn: EvalFn = @ptrFromInt(@as(usize, @intCast(eval_addr)));
            var result: i64 = undefined;
            eval_fn(&result);
            const str = if (result != 0) "True" else "False";
            return allocator.dupe(u8, str) catch return error.OutOfMemory;
        },
        .str => {
            // RocStr is a 24-byte struct on 64-bit platforms
            // For small strings (< 24 bytes), content is stored inline with length in last byte
            const RocStrBytes = [24]u8;
            const EvalFn = *const fn (*RocStrBytes) callconv(.c) void;
            const eval_fn: EvalFn = @ptrFromInt(@as(usize, @intCast(eval_addr)));
            var result: RocStrBytes = undefined;
            eval_fn(&result);

            // Check if it's a small string (last byte has high bit set)
            const last_byte = result[23];
            if (last_byte & 0x80 != 0) {
                // Small string - length is in the last byte (without high bit)
                const length = last_byte & 0x7F;
                const content = result[0..length];
                // Format as quoted string
                return std.fmt.allocPrint(allocator, "\"{s}\"", .{content}) catch return error.OutOfMemory;
            } else {
                // Big string - we'd need to dereference a pointer which is complex
                // For now, return a placeholder
                return allocator.dupe(u8, "\"<heap string>\"") catch return error.OutOfMemory;
            }
        },
        else => return error.UnsupportedLayout,
    }
}
