//! LLVM Native Compilation Module
//!
//! This module compiles LLVM bitcode to native artifacts. It handles:
//! 1. Parse bitcode into an LLVM module
//! 2. Merge builtin functions into the module
//! 3. Compile to a native object file or shared library
//!
//! The evaluator uses a temporary shared library so the platform loader owns
//! relocations and symbol binding.

const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const bindings = @import("bindings.zig");

const Allocator = std.mem.Allocator;

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
// and native i128/u128 types.

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

/// Errors that can occur during LLVM compilation.
pub const Error = error{
    OutOfMemory,
    BitcodeParseError,
    ModuleLinkFailed,
    CompilationFailed,
    TempFileError,
    LinkFailed,
};

/// Options for controlling LLVM compilation behavior.
pub const CompileOptions = struct {
    /// Whether to place each function in its own section.
    /// Set to false for JIT mode (single .text section is simpler).
    function_sections: bool = true,
    /// Optimization level for code generation.
    opt_level: bindings.CodeGenOptLevel = .Default,
    /// Relocation model to use when emitting the object file.
    reloc_mode: bindings.RelocMode = .Default,
    /// Whether to use the module's native target triple instead of LLVM's default.
    use_module_target_triple: bool = false,
};

fn emitMergedBitcodeToObjectFile(
    bitcode: []const u32,
    options: CompileOptions,
    output_path: [:0]const u8,
) Error!void {
    // Convert u32 slice to u8 slice for the bindings
    const bitcode_bytes: []const u8 = @as([*]const u8, @ptrCast(bitcode.ptr))[0 .. bitcode.len * 4];

    if (std.process.getEnvVarOwned(std.heap.page_allocator, "ROC_LLVM_KEEP_BITCODE")) |keep_path| {
        defer std.heap.page_allocator.free(keep_path);
        std.Io.Dir.cwd().writeFile(.{
            .sub_path = keep_path,
            .data = bitcode_bytes,
        }) catch {};
    } else |_| {}

    // Initialize all targets
    bindings.initializeAllTargets();

    // Create LLVM context
    const context = bindings.Context.create();
    defer context.dispose();

    // Create memory buffer from bitcode
    const mem_buf = bindings.MemoryBuffer.createMemoryBufferWithMemoryRange(
        bitcode_bytes.ptr,
        bitcode_bytes.len,
        "roc_bitcode",
        bindings.Bool.False,
    );

    // Parse bitcode into module
    var module: *bindings.Module = undefined;
    if (context.parseBitcodeInContext2(mem_buf, &module).toBool()) {
        mem_buf.dispose();
        return Error.BitcodeParseError;
    }
    defer module.dispose();
    // Note: mem_buf is consumed by parseBitcodeInContext2

    // Load and merge builtin bitcode into the user module.
    // This makes all builtin functions available.
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
            return Error.BitcodeParseError;
        }
        // Note: builtin_mem_buf is consumed by parseBitcodeInContext2

        // Set the builtin module's target triple and data layout to match the user module.
        builtin_module.setTargetTriple(module.getTargetTriple());
        builtin_module.setDataLayout(module.getDataLayout());

        // Link builtins into user module (destroys builtin_module on success)
        if (module.link(builtin_module).toBool()) {
            return Error.ModuleLinkFailed;
        }
        // Note: builtin_module is now invalid - do NOT dispose it
    }

    const triple, const dispose_triple = blk: {
        if (options.use_module_target_triple) {
            break :blk .{ module.getTargetTriple(), false };
        }

        break :blk .{ bindings.GetDefaultTargetTriple(), true };
    };
    defer if (dispose_triple) bindings.disposeMessage(triple);

    // Get target from triple
    var target: *bindings.Target = undefined;
    var target_error: [*:0]const u8 = undefined;
    if (bindings.Target.getFromTriple(triple, &target, &target_error).toBool()) {
        bindings.disposeMessage(target_error);
        return Error.CompilationFailed;
    }

    // Use a baseline CPU that has the required features for each architecture.
    const cpu: [*:0]const u8 = switch (builtin.cpu.arch) {
        .x86_64 => "x86-64",
        .x86 => "pentium4",
        else => "generic",
    };

    // Create target machine
    const target_machine = bindings.TargetMachine.create(
        target,
        triple,
        cpu,
        "", // No specific features
        options.opt_level, // optimization level
        options.reloc_mode,
        .Default, // code model
        options.function_sections, // function_sections
        options.function_sections, // data_sections: match function_sections
        .Default, // float_abi
        null, // abi_name
        false, // emulated_tls
    );
    defer target_machine.dispose();

    // Set up emit options
    const default_coverage = bindings.TargetMachine.EmitOptions.Coverage{
        .CoverageType = .None,
        .IndirectCalls = false,
        .TraceBB = false,
        .TraceCmp = false,
        .TraceDiv = false,
        .TraceGep = false,
        .Use8bitCounters = false,
        .TracePC = false,
        .TracePCGuard = false,
        .Inline8bitCounters = false,
        .InlineBoolFlag = false,
        .PCTable = false,
        .NoPrune = false,
        .StackDepth = false,
        .TraceLoads = false,
        .TraceStores = false,
        .CollectControlFlow = false,
    };

    const emit_options = bindings.TargetMachine.EmitOptions{
        .is_debug = options.opt_level == .None,
        .is_small = false,
        .time_report_out = null,
        .tsan = false,
        .sancov = false,
        .lto = .None,
        .allow_fast_isel = false,
        .allow_machine_outliner = true,
        .asm_filename = null,
        .bin_filename = output_path.ptr,
        .llvm_ir_filename = null,
        .bitcode_filename = null,
        .coverage = default_coverage,
    };

    // Emit merged module to object file
    var emit_error: [*:0]const u8 = undefined;
    if (target_machine.emitToFile(module, &emit_error, &emit_options)) {
        bindings.disposeMessage(emit_error);
        return Error.CompilationFailed;
    }
}

/// Compile LLVM bitcode to a native object file.
pub fn compileToObject(allocator: Allocator, bitcode: []const u32, options: CompileOptions) Error![]const u8 {
    const temp_path = createTempPath(allocator, ".o") catch return Error.TempFileError;
    defer allocator.free(temp_path);

    try emitMergedBitcodeToObjectFile(bitcode, options, temp_path);

    // Read the object file back into memory
    const object_bytes = std.Io.Dir.cwd().readFileAlloc(
        allocator,
        std.mem.sliceTo(temp_path, 0),
        10 * 1024 * 1024, // 10MB max
    ) catch return Error.TempFileError;

    if (std.process.getEnvVarOwned(allocator, "ROC_LLVM_KEEP_OBJECT")) |keep_path| {
        defer allocator.free(keep_path);
        std.Io.Dir.cwd().writeFile(.{
            .sub_path = keep_path,
            .data = object_bytes,
        }) catch {};
    } else |_| {}

    // Clean up temp file
    std.Io.Dir.cwd().deleteFile(std.mem.sliceTo(temp_path, 0)) catch {};

    return object_bytes;
}

/// Compile LLVM bitcode to a native shared library and return its path.
/// Caller owns the returned path and is responsible for deleting the file.
pub fn compileToSharedLibrary(allocator: Allocator, bitcode: []const u32, options: CompileOptions) Error![:0]const u8 {
    const object_path = createTempPath(allocator, objectExtension()) catch return Error.TempFileError;
    defer {
        std.Io.Dir.cwd().deleteFile(std.mem.sliceTo(object_path, 0)) catch {};
        allocator.free(object_path);
    }

    const shared_lib_path = createTempPath(allocator, sharedLibraryExtension()) catch return Error.TempFileError;
    errdefer {
        std.Io.Dir.cwd().deleteFile(std.mem.sliceTo(shared_lib_path, 0)) catch {};
        allocator.free(shared_lib_path);
    }

    var pic_options = options;
    pic_options.reloc_mode = .PIC;
    pic_options.use_module_target_triple = true;

    try emitMergedBitcodeToObjectFile(bitcode, pic_options, object_path);

    if (std.process.getEnvVarOwned(allocator, "ROC_LLVM_KEEP_OBJECT")) |keep_path| {
        defer allocator.free(keep_path);
        std.Io.Dir.cwd().copyFile(
            std.mem.sliceTo(object_path, 0),
            std.Io.Dir.cwd(),
            keep_path,
            .{},
        ) catch {};
    } else |_| {}

    try linkSharedLibrary(allocator, object_path, shared_lib_path);

    if (std.process.getEnvVarOwned(allocator, "ROC_LLVM_KEEP_DYLIB")) |keep_path| {
        defer allocator.free(keep_path);
        std.Io.Dir.cwd().copyFile(
            std.mem.sliceTo(shared_lib_path, 0),
            std.Io.Dir.cwd(),
            keep_path,
            .{},
        ) catch {};
    } else |_| {}

    return shared_lib_path;
}

fn linkSharedLibrary(
    allocator: Allocator,
    object_path: [:0]const u8,
    shared_lib_path: [:0]const u8,
) Error!void {
    if (builtin.os.tag == .macos) {
        return linkSharedLibraryMacos(allocator, object_path, shared_lib_path);
    }

    var arena_impl = std.heap.ArenaAllocator.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var args: std.ArrayList([]const u8) = .empty;
    defer args.deinit(allocator);

    switch (builtin.os.tag) {
        .macos => unreachable,
        .linux, .freebsd, .openbsd, .netbsd => {
            try args.append(allocator, "ld.lld");
            try args.append(allocator, "-shared");
            try args.append(allocator, "-o");
            try args.append(allocator, std.mem.sliceTo(shared_lib_path, 0));
            try args.append(allocator, std.mem.sliceTo(object_path, 0));
        },
        .windows => {
            try args.append(allocator, "lld-link");
            try args.append(allocator, "/dll");
            try args.append(allocator, try std.fmt.allocPrint(arena, "/out:{s}", .{std.mem.sliceTo(shared_lib_path, 0)}));
            try args.append(allocator, switch (builtin.cpu.arch) {
                .aarch64 => "/machine:arm64",
                .x86_64 => "/machine:x64",
                .x86 => "/machine:x86",
                else => return Error.LinkFailed,
            });
            try args.append(allocator, std.mem.sliceTo(object_path, 0));
            try args.append(allocator, "/defaultlib:kernel32");
            try args.append(allocator, "/defaultlib:ntdll");
            try args.append(allocator, "/defaultlib:msvcrt");
        },
        else => return Error.LinkFailed,
    }

    const c_args = arena.alloc([*:0]const u8, args.items.len) catch return Error.OutOfMemory;

    for (args.items, 0..) |arg, i| {
        c_args[i] = (arena.dupeZ(u8, arg) catch return Error.OutOfMemory).ptr;
    }

    const success = switch (builtin.os.tag) {
        .macos => bindings.LinkMachO(@intCast(c_args.len), c_args.ptr, false, false),
        .linux, .freebsd, .openbsd, .netbsd => bindings.LinkELF(@intCast(c_args.len), c_args.ptr, false, false),
        .windows => bindings.LinkCOFF(@intCast(c_args.len), c_args.ptr, false, false),
        else => false,
    };

    if (!success) return Error.LinkFailed;
}

fn linkSharedLibraryMacos(
    allocator: Allocator,
    object_path: [:0]const u8,
    shared_lib_path: [:0]const u8,
) Error!void {
    const result = std.process.run(.{
        .allocator = allocator,
        .argv = &.{
            "cc",
            "-dynamiclib",
            "-isysroot",
            build_options.darwin_sysroot,
            "-o",
            std.mem.sliceTo(shared_lib_path, 0),
            std.mem.sliceTo(object_path, 0),
            "src/llvm_compile/darwin_compat.o",
        },
    }) catch return Error.LinkFailed;
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    switch (result.term) {
        .Exited => |code| {
            if (code == 0) return;
        },
        else => {},
    }

    if (result.stderr.len > 0) {
        std.debug.print("{s}", .{result.stderr});
    }

    return Error.LinkFailed;
}

/// Create a unique temporary file path for an artifact output.
fn createTempPath(allocator: Allocator, extension: []const u8) ![:0]const u8 {
    // Generate a unique filename using timestamp
    const timestamp = std.time.nanoTimestamp();
    const timestamp_low: u64 = @truncate(@as(u128, @bitCast(timestamp)));
    const random_suffix: u32 = @truncate(timestamp_low);

    // Use appropriate temp directory for each platform
    const tmp_prefix = if (builtin.os.tag == .windows) "C:\\Windows\\Temp\\roc_llvm_" else "/tmp/roc_llvm_";

    const str = try std.fmt.allocPrint(
        allocator,
        "{s}{x}_{x}{s}",
        .{ tmp_prefix, timestamp_low, random_suffix, extension },
    );
    // Convert to null-terminated by reallocating with extra byte
    const result = try allocator.allocSentinel(u8, str.len, 0);
    @memcpy(result, str);
    allocator.free(str);
    return result;
}

fn objectExtension() []const u8 {
    return switch (builtin.os.tag) {
        .windows => ".obj",
        else => ".o",
    };
}

fn sharedLibraryExtension() []const u8 {
    return switch (builtin.os.tag) {
        .windows => ".dll",
        .macos => ".dylib",
        else => ".so",
    };
}
