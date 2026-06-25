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
const bindings = @import("vendor_llvm_compile_bindings");
const embedded_lld = @import("embedded_lld");
const llvm_embedded = @import("llvm_embedded");
const collections = @import("collections");
const roc_target = @import("roc_target");

const Allocator = std.mem.Allocator;

var temp_path_counter = std.atomic.Value(usize).init(0);

const TempPathError = Allocator.Error || std.Io.Dir.CreateDirPathError || std.Io.Dir.RealPathFileAllocError || error{TempDirUnavailable};

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
    WindowsSDKNotFound,
};

/// Options for controlling LLVM compilation behavior.
pub const CompileOptions = struct {
    /// Whether to place each function in its own section.
    /// Set to false for JIT mode (single .text section is simpler).
    function_sections: bool = true,
    /// Optimization level for LLVM IR and target-machine code generation.
    optimization: bindings.IrOptimizationLevel = .O3,
    /// Whether to include debug information in the generated object.
    debug: bool = false,
    /// Relocation model to use when emitting the object file.
    reloc_mode: bindings.RelocMode = .Default,
    /// Whether to use the module's native target triple instead of LLVM's default.
    use_module_target_triple: bool = false,
    /// LLVM CPU name for target-machine code generation. Empty means LLVM's target default.
    cpu: [:0]const u8 = "",
    /// LLVM feature string for target-machine code generation. Empty means LLVM's target default.
    features: [:0]const u8 = "",
    /// Target pointer width in bits. Used to select the matching embedded
    /// builtin bitcode payload before retargeting the merged LLVM module.
    target_ptr_width_bits: u8,
    /// Treat the target as freestanding for LLVM object emission: optimization
    /// cannot assume target library functions, and memory intrinsics are lowered
    /// to explicit loops before codegen.
    no_target_libcalls: bool = false,
};

fn valueName(value: *bindings.Value) []const u8 {
    var name_len: usize = 0;
    const name_ptr = value.getName(&name_len);
    return name_ptr[0..name_len];
}

fn recordValueName(allocator: Allocator, defs: *std.StringHashMap(void), value: *bindings.Value) Error!void {
    const name = valueName(value);
    if (defs.contains(name)) return;

    const owned_name = allocator.dupe(u8, name) catch return Error.OutOfMemory;
    errdefer allocator.free(owned_name);
    defs.put(owned_name, {}) catch return Error.OutOfMemory;
}

fn recordModuleDefinitions(allocator: Allocator, module: *bindings.Module, defs: *std.StringHashMap(void)) Error!void {
    var func = module.getFirstFunction();
    while (func) |value| : (func = value.getNextFunction()) {
        if (value.isDeclaration().toBool()) continue;
        try recordValueName(allocator, defs, value);
    }

    var global = module.getFirstGlobal();
    while (global) |value| : (global = value.getNextGlobal()) {
        if (value.isDeclaration().toBool()) continue;
        try recordValueName(allocator, defs, value);
    }

    var alias = module.getFirstGlobalAlias();
    while (alias) |value| : (alias = value.getNextGlobalAlias()) {
        try recordValueName(allocator, defs, value);
    }
}

fn recordModuleDeclarations(allocator: Allocator, module: *bindings.Module, decls: *std.StringHashMap(void)) Error!void {
    var func = module.getFirstFunction();
    while (func) |value| : (func = value.getNextFunction()) {
        if (!value.isDeclaration().toBool()) continue;
        try recordValueName(allocator, decls, value);
    }

    var global = module.getFirstGlobal();
    while (global) |value| : (global = value.getNextGlobal()) {
        if (!value.isDeclaration().toBool()) continue;
        try recordValueName(allocator, decls, value);
    }
}

const core_builtin_roots = std.StaticStringMap(void).initComptime(.{
    .{ "roc__num_add_with_overflow_i128", {} },
    .{ "roc__num_mul_with_overflow_i16", {} },
    .{ "roc__num_mul_with_overflow_i32", {} },
    .{ "roc__num_mul_with_overflow_i64", {} },
    .{ "roc__num_mul_with_overflow_i8", {} },
    .{ "roc__num_sub_with_overflow_i128", {} },
    .{ "roc_builtins_allocate_with_refcount", {} },
    .{ "roc_builtins_box_decref_with", {} },
    .{ "roc_builtins_box_decref_with_single_thread", {} },
    .{ "roc_builtins_box_free_with", {} },
    .{ "roc_builtins_dbg_str", {} },
    .{ "roc_builtins_expect_err_str", {} },
    .{ "roc_builtins_decref_data_ptr", {} },
    .{ "roc_builtins_decref_data_ptr_single_thread", {} },
    .{ "roc_builtins_erased_callable_decref", {} },
    .{ "roc_builtins_erased_callable_decref_single_thread", {} },
    .{ "roc_builtins_erased_callable_free", {} },
    .{ "roc_builtins_erased_callable_incref", {} },
    .{ "roc_builtins_free_data_ptr", {} },
    .{ "roc_builtins_i16_mod_by", {} },
    .{ "roc_builtins_i32_mod_by", {} },
    .{ "roc_builtins_i64_mod_by", {} },
    .{ "roc_builtins_i8_mod_by", {} },
    .{ "roc_builtins_incref_data_ptr", {} },
    .{ "roc_builtins_incref_data_ptr_single_thread", {} },
    .{ "roc_builtins_int_from_str", {} },
    .{ "roc_builtins_int_to_str", {} },
    .{ "roc_builtins_list_append_unsafe", {} },
    .{ "roc_builtins_list_concat", {} },
    .{ "roc_builtins_list_decref_flat_list", {} },
    .{ "roc_builtins_list_decref_str", {} },
    .{ "roc_builtins_list_decref_with", {} },
    .{ "roc_builtins_list_decref_with_single_thread", {} },
    .{ "roc_builtins_list_drop_at", {} },
    .{ "roc_builtins_list_eq", {} },
    .{ "roc_builtins_list_free_flat_list", {} },
    .{ "roc_builtins_list_free_with", {} },
    .{ "roc_builtins_list_incref", {} },
    .{ "roc_builtins_list_incref_single_thread", {} },
    .{ "roc_builtins_list_list_eq", {} },
    .{ "roc_builtins_list_prepend", {} },
    .{ "roc_builtins_list_release_excess_capacity", {} },
    .{ "roc_builtins_list_replace", {} },
    .{ "roc_builtins_list_reserve", {} },
    .{ "roc_builtins_list_reverse", {} },
    .{ "roc_builtins_list_str_eq", {} },
    .{ "roc_builtins_list_sublist", {} },
    .{ "roc_builtins_list_swap", {} },
    .{ "roc_builtins_list_with_capacity", {} },
    .{ "roc_builtins_roc_crashed", {} },
    .{ "roc_builtins_roc_expect_failed", {} },
    .{ "roc_builtins_str_caseless_ascii_equals", {} },
    .{ "roc_builtins_str_concat", {} },
    .{ "roc_builtins_str_contains", {} },
    .{ "roc_builtins_str_count_utf8_bytes", {} },
    .{ "roc_builtins_str_drop_prefix", {} },
    .{ "roc_builtins_str_drop_prefix_caseless_ascii", {} },
    .{ "roc_builtins_str_drop_suffix", {} },
    .{ "roc_builtins_str_ends_with", {} },
    .{ "roc_builtins_str_equal", {} },
    .{ "roc_builtins_str_escape_and_quote", {} },
    .{ "roc_builtins_str_from_literal", {} },
    .{ "roc_builtins_str_from_utf8", {} },
    .{ "roc_builtins_str_from_utf8_lossy", {} },
    .{ "roc_builtins_str_from_utf8_parts", {} },
    .{ "roc_builtins_str_from_utf8_result", {} },
    .{ "roc_builtins_str_join_with", {} },
    .{ "roc_builtins_str_release_excess_capacity", {} },
    .{ "roc_builtins_str_repeat", {} },
    .{ "roc_builtins_str_reserve", {} },
    .{ "roc_builtins_str_split", {} },
    .{ "roc_builtins_str_starts_with", {} },
    .{ "roc_builtins_str_to_utf8", {} },
    .{ "roc_builtins_str_trim", {} },
    .{ "roc_builtins_str_trim_end", {} },
    .{ "roc_builtins_str_trim_start", {} },
    .{ "roc_builtins_str_with_ascii_lowercased", {} },
    .{ "roc_builtins_str_with_ascii_uppercased", {} },
    .{ "roc_builtins_str_with_capacity", {} },
    .{ "roc_builtins_u16_mod_by", {} },
    .{ "roc_builtins_u32_mod_by", {} },
    .{ "roc_builtins_u64_mod_by", {} },
    .{ "roc_builtins_u8_mod_by", {} },
});

fn isBuiltinRoot(name: []const u8) bool {
    return std.mem.startsWith(u8, name, "roc_builtins_") or std.mem.startsWith(u8, name, "roc__num_");
}

fn canUseCoreBuiltins(app_decls: *const std.StringHashMap(void)) bool {
    var roots = app_decls.keyIterator();
    while (roots.next()) |root| {
        if (isBuiltinRoot(root.*) and !core_builtin_roots.has(root.*)) return false;
    }
    return true;
}

fn selectBuiltinBitcode(ptr_width: u8, app_decls: *const std.StringHashMap(void)) []const u8 {
    const use_core = canUseCoreBuiltins(app_decls);
    return switch (ptr_width) {
        32 => if (use_core) llvm_embedded.builtins32_core_bc else llvm_embedded.builtins32_bc,
        64 => if (use_core) llvm_embedded.builtins64_core_bc else llvm_embedded.builtins64_bc,
        else => "",
    };
}

fn removeFunctionTargetAttrs(func: *bindings.Value) void {
    func.removeStringAttributeAtIndex(
        bindings.attribute_function_index,
        "target-features",
        "target-features".len,
    );
    func.removeStringAttributeAtIndex(
        bindings.attribute_function_index,
        "target-cpu",
        "target-cpu".len,
    );
}

fn cleanMergedBuiltinDefinitions(module: *bindings.Module, app_defs: *const std.StringHashMap(void)) void {
    var alias = module.getFirstGlobalAlias();
    while (alias) |value| : (alias = value.getNextGlobalAlias()) {
        if (app_defs.contains(valueName(value))) continue;
        if (value.aliasGetAliasee()) |aliasee| {
            value.replaceAllUsesWith(aliasee);
        }
        value.setLinkage(bindings.internal_linkage);
    }

    var func = module.getFirstFunction();
    while (func) |value| : (func = value.getNextFunction()) {
        if (value.isDeclaration().toBool()) continue;
        if (app_defs.contains(valueName(value))) continue;
        removeFunctionTargetAttrs(value);
        value.setLinkage(bindings.internal_linkage);
    }

    var global = module.getFirstGlobal();
    while (global) |value| : (global = value.getNextGlobal()) {
        if (value.isDeclaration().toBool()) continue;
        if (app_defs.contains(valueName(value))) continue;
        value.setLinkage(bindings.internal_linkage);
    }
}

/// `LLVMProtectedVisibility`: the symbol stays exported (so the harness can look
/// up entry points and `roc_expect_err_region`) but is non-preemptible, so
/// intra-image references compile to direct PC-relative accesses instead of
/// GOT/PLT slots needing a runtime relocation.
const llvm_protected_visibility: c_int = 2;

/// Make every definition in the merged module non-preemptible so the in-process
/// loader never has to resolve an intra-image GOT/PLT slot. Declarations
/// (external references) are left untouched.
fn markDefinitionsDsoLocal(module: *bindings.Module) void {
    var func = module.getFirstFunction();
    while (func) |value| : (func = value.getNextFunction()) {
        if (value.isDeclaration().toBool()) continue;
        value.setVisibility(llvm_protected_visibility);
    }

    var global = module.getFirstGlobal();
    while (global) |value| : (global = value.getNextGlobal()) {
        if (value.isDeclaration().toBool()) continue;
        value.setVisibility(llvm_protected_visibility);
    }

    var alias = module.getFirstGlobalAlias();
    while (alias) |value| : (alias = value.getNextGlobalAlias()) {
        value.setVisibility(llvm_protected_visibility);
    }
}

fn removeBuiltinUsedRoots(module: *bindings.Module) void {
    if (module.getNamedGlobal("llvm.used")) |used| {
        used.deleteGlobal();
    }
    if (module.getNamedGlobal("llvm.compiler.used")) |compiler_used| {
        compiler_used.deleteGlobal();
    }
}

fn pruneBuiltinModule(module: *bindings.Module, app_decls: *const std.StringHashMap(void)) void {
    removeBuiltinUsedRoots(module);

    var alias = module.getFirstGlobalAlias();
    while (alias) |value| : (alias = value.getNextGlobalAlias()) {
        if (!app_decls.contains(valueName(value))) {
            value.setLinkage(bindings.internal_linkage);
        }
    }

    var func = module.getFirstFunction();
    while (func) |value| : (func = value.getNextFunction()) {
        if (value.isDeclaration().toBool()) continue;
        removeFunctionTargetAttrs(value);
        if (!app_decls.contains(valueName(value))) {
            value.setLinkage(bindings.internal_linkage);
        }
    }

    var global = module.getFirstGlobal();
    while (global) |value| : (global = value.getNextGlobal()) {
        if (value.isDeclaration().toBool()) continue;
        if (!app_decls.contains(valueName(value))) {
            value.setLinkage(bindings.internal_linkage);
        }
    }
}

fn emitMergedBitcodeToObjectFile(
    allocator: Allocator,
    io: std.Io,
    bitcode: []const u32,
    options: CompileOptions,
    output_path: [:0]const u8,
) Error!void {
    // Convert u32 slice to u8 slice for the bindings
    const bitcode_bytes: []const u8 = @as([*]const u8, @ptrCast(bitcode.ptr))[0 .. bitcode.len * 4];

    if (comptime build_options.llvm_keep_bitcode.len != 0) {
        std.Io.Dir.cwd().writeFile(io, .{
            .sub_path = build_options.llvm_keep_bitcode,
            .data = bitcode_bytes,
        }) catch {};
    }

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

    const triple, const dispose_triple = blk: {
        if (options.use_module_target_triple) {
            break :blk .{ module.getTargetTriple(), false };
        }

        break :blk .{ bindings.GetDefaultTargetTriple(), true };
    };
    defer if (dispose_triple) bindings.disposeMessage(triple);
    if (!options.use_module_target_triple) {
        module.setTargetTriple(triple);
    }

    // Get target from triple
    var target: *bindings.Target = undefined;
    var target_error: [*:0]const u8 = undefined;
    if (bindings.Target.getFromTriple(triple, &target, &target_error).toBool()) {
        bindings.disposeMessage(target_error);
        return Error.CompilationFailed;
    }

    // Create target machine
    const target_machine = bindings.TargetMachine.create(
        target,
        triple,
        if (options.cpu.len == 0) null else options.cpu.ptr,
        if (options.features.len == 0) null else options.features.ptr,
        options.optimization.toCodeGenOptLevel(),
        options.reloc_mode,
        .Default, // code model
        options.function_sections, // function_sections
        options.function_sections, // data_sections: match function_sections
        .Default, // float_abi
        null, // abi_name
        false, // emulated_tls
    );
    defer target_machine.dispose();

    var app_defs = std.StringHashMap(void).init(allocator);
    defer {
        var keys = app_defs.keyIterator();
        while (keys.next()) |key| allocator.free(key.*);
        app_defs.deinit();
    }
    try recordModuleDefinitions(allocator, module, &app_defs);

    var app_decls = std.StringHashMap(void).init(allocator);
    defer {
        var keys = app_decls.keyIterator();
        while (keys.next()) |key| allocator.free(key.*);
        app_decls.deinit();
    }
    try recordModuleDeclarations(allocator, module, &app_decls);

    // Load and merge builtin bitcode into the user module. Builtin definitions
    // remain real definitions so LLVM can inline and optimize them with the app.
    {
        const builtin_bitcode = selectBuiltinBitcode(options.target_ptr_width_bits, &app_decls);
        if (builtin_bitcode.len == 0) return Error.CompilationFailed;
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
        builtin_module.setTargetTriple(triple);
        const module_data_layout = module.getDataLayout();
        if (std.mem.len(module_data_layout) == 0) {
            module.setDataLayout(builtin_module.getDataLayout());
        } else {
            builtin_module.setDataLayout(module_data_layout);
        }
        pruneBuiltinModule(builtin_module, &app_decls);
        bindings.runGlobalDCE(builtin_module);

        // Link builtins into user module (destroys builtin_module on success)
        if (module.link(builtin_module).toBool()) {
            return Error.ModuleLinkFailed;
        }
        // Note: builtin_module is now invalid - do NOT dispose it

        cleanMergedBuiltinDefinitions(module, &app_defs);
        bindings.runGlobalDCE(module);
    }

    // This object is loaded in-process by a relocation-free loader (Zig's
    // `ElfDynLib` in a static, no-libc roc binary applies no relocations; the
    // dev backend's object reader handles only a fixed set). Every definition in
    // the merged module lives in this same image, so mark them `dso_local` to
    // make intra-image references direct (PC-relative) instead of routing through
    // a GOT/PLT slot that would need a runtime relocation and otherwise stay null.
    markDefinitionsDsoLocal(module);

    var verify_error: [*:0]const u8 = undefined;
    if (module.verify(.ReturnStatus, &verify_error).toBool()) {
        std.debug.print("{s}\n", .{verify_error});
        bindings.disposeMessage(verify_error);
        return Error.CompilationFailed;
    }

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
        .is_debug = options.debug,
        .ir_opt_level = options.optimization,
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
        .no_target_libcalls = options.no_target_libcalls,
    };

    // Emit merged module to object file
    var emit_error: [*:0]const u8 = undefined;
    if (target_machine.emitToFile(module, &emit_error, &emit_options)) {
        std.debug.print("{s}\n", .{emit_error});
        bindings.disposeMessage(emit_error);
        return Error.CompilationFailed;
    }
}

/// Compile LLVM bitcode to a native object file.
pub fn compileToObject(allocator: Allocator, io: std.Io, bitcode: []const u32, options: CompileOptions) Error![]const u8 {
    const temp_path = createTempPath(allocator, io, ".o") catch return Error.TempFileError;
    defer allocator.free(temp_path);

    try emitMergedBitcodeToObjectFile(allocator, io, bitcode, options, temp_path);

    // Read the object file back into memory
    const object_bytes = std.Io.Dir.cwd().readFileAlloc(
        io,
        std.mem.sliceTo(temp_path, 0),
        allocator,
        .limited(10 * 1024 * 1024), // 10MB max
    ) catch return Error.TempFileError;

    if (comptime build_options.llvm_keep_object.len != 0) {
        std.Io.Dir.cwd().writeFile(io, .{
            .sub_path = build_options.llvm_keep_object,
            .data = object_bytes,
        }) catch {};
    }

    // Clean up temp file
    std.Io.Dir.cwd().deleteFile(io, std.mem.sliceTo(temp_path, 0)) catch {};

    return object_bytes;
}

/// Compile LLVM bitcode to a native shared library and return its path.
/// Caller owns the returned path and is responsible for deleting the file.
pub fn compileToSharedLibrary(allocator: Allocator, io: std.Io, bitcode: []const u32, options: CompileOptions) Error![:0]const u8 {
    const object_path = createTempPath(allocator, io, objectExtension()) catch return Error.TempFileError;
    defer {
        std.Io.Dir.cwd().deleteFile(io, std.mem.sliceTo(object_path, 0)) catch {};
        allocator.free(object_path);
    }

    const shared_lib_path = createTempPath(allocator, io, sharedLibraryExtension()) catch return Error.TempFileError;
    errdefer {
        std.Io.Dir.cwd().deleteFile(io, std.mem.sliceTo(shared_lib_path, 0)) catch {};
        allocator.free(shared_lib_path);
    }

    var pic_options = options;
    pic_options.reloc_mode = .PIC;
    pic_options.use_module_target_triple = true;
    pic_options.no_target_libcalls = switch (builtin.os.tag) {
        .macos, .windows => false,
        else => true,
    };

    try emitMergedBitcodeToObjectFile(allocator, io, bitcode, pic_options, object_path);

    if (comptime build_options.llvm_keep_object.len != 0) {
        std.Io.Dir.cwd().copyFile(
            std.mem.sliceTo(object_path, 0),
            std.Io.Dir.cwd(),
            build_options.llvm_keep_object,
            io,
            .{},
        ) catch {};
    }

    try linkSharedLibrary(allocator, io, object_path, shared_lib_path);

    if (comptime build_options.llvm_keep_dylib.len != 0) {
        std.Io.Dir.cwd().copyFile(
            std.mem.sliceTo(shared_lib_path, 0),
            std.Io.Dir.cwd(),
            build_options.llvm_keep_dylib,
            io,
            .{},
        ) catch {};
    }

    return shared_lib_path;
}

fn linkSharedLibrary(
    allocator: Allocator,
    io: std.Io,
    object_path: [:0]const u8,
    shared_lib_path: [:0]const u8,
) Error!void {
    var arena_impl = collections.SingleThreadArena.init(allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var args: std.ArrayList([]const u8) = .empty;
    defer args.deinit(allocator);

    var stack_probe_path: ?[:0]const u8 = null;
    defer if (stack_probe_path) |path| {
        std.Io.Dir.cwd().deleteFile(io, std.mem.sliceTo(path, 0)) catch {};
        allocator.free(path);
    };

    var compiler_rt_path: ?[:0]const u8 = null;
    defer if (compiler_rt_path) |path| {
        std.Io.Dir.cwd().deleteFile(io, std.mem.sliceTo(path, 0)) catch {};
        allocator.free(path);
    };

    switch (builtin.os.tag) {
        .macos => {
            try args.append(allocator, "ld64.lld");
            try args.append(allocator, "-dylib");
            try args.append(allocator, "-o");
            try args.append(allocator, std.mem.sliceTo(shared_lib_path, 0));
            try args.append(allocator, "-w");
            try args.append(allocator, "-arch");
            try args.append(allocator, switch (builtin.cpu.arch) {
                .aarch64 => "arm64",
                .x86_64 => "x86_64",
                else => return Error.LinkFailed,
            });
            try args.append(allocator, "-platform_version");
            try args.append(allocator, "macos");
            try args.append(allocator, roc_target.macos_deployment.linker_version);
            try args.append(allocator, roc_target.macos_deployment.linker_version);
            try args.append(allocator, "-syslibroot");
            try args.append(allocator, build_options.darwin_sysroot);
            try args.append(allocator, std.mem.sliceTo(object_path, 0));
            try args.append(allocator, "-lSystem");
        },
        .linux, .freebsd, .openbsd, .netbsd => {
            try args.append(allocator, "ld.lld");
            try args.append(allocator, "-shared");
            // The eval-test-runner is a static-musl binary whose dlopen does not
            // resolve a loaded library's own PLT/GOT (no dynamic-loader symbol
            // binding), so default (preemptible, lazily-bound) intra-library
            // calls like roc_builtins_dec_to_str leave GOT slots null and jump to
            // 0x0. -Bsymbolic binds intra-library global references to their local
            // definitions at link time (direct calls / RELATIVE relocs that musl
            // always applies at load); -z now additionally forces eager binding.
            try args.append(allocator, "-Bsymbolic");
            try args.append(allocator, "-z");
            try args.append(allocator, "now");
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
            // LLVM emits ___chkstk_ms stack probes for functions with large
            // frames; no Windows system library defines it, so link the same
            // generated probe object the roc CLI linker uses.
            if (builtin.cpu.arch == .x86_64) {
                const probe_obj = embedded_lld.stack_probe.generateStackProbeObject(arena) catch return Error.OutOfMemory;
                const probe_path = createTempPath(allocator, io, ".obj") catch return Error.TempFileError;
                stack_probe_path = probe_path;
                std.Io.Dir.cwd().writeFile(io, .{
                    .sub_path = std.mem.sliceTo(probe_path, 0),
                    .data = probe_obj,
                }) catch return Error.TempFileError;
                try args.append(allocator, std.mem.sliceTo(probe_path, 0));
            }

            // Native codegen lowers the merged module's 128-bit divide/remainder
            // and 128-bit<->float operations to compiler-rt libcalls (__divti3,
            // __fixdfti, ...) that the image itself does not define. The Unix
            // loaders bind these at load time (the in-process eval_loader via
            // native_runtime_libcalls.resolve, or the OS dynamic loader), but
            // Windows' LoadLibrary needs a fully linked DLL, so link in the
            // object that exports them. See eval_compiler_rt_libcalls.zig.
            const compiler_rt_obj = llvm_embedded.eval_compiler_rt_libcalls_obj;
            const compiler_rt_obj_path = createTempPath(allocator, io, ".obj") catch return Error.TempFileError;
            compiler_rt_path = compiler_rt_obj_path;
            std.Io.Dir.cwd().writeFile(io, .{
                .sub_path = std.mem.sliceTo(compiler_rt_obj_path, 0),
                .data = compiler_rt_obj,
            }) catch return Error.TempFileError;
            try args.append(allocator, std.mem.sliceTo(compiler_rt_obj_path, 0));

            // Tell lld-link where to find the CRT and Windows SDK import
            // libraries. Without these /libpath entries lld-link can only locate
            // kernel32.lib/ntdll.lib/msvcrt.lib via the LIB environment variable,
            // so linking fails in shells where LIB is unset (e.g. outside a
            // Developer Command Prompt). The roc CLI linker resolves these the
            // same way; this is a native compile, so target the host arch.
            const query = std.Target.Query{
                .cpu_arch = builtin.cpu.arch,
                .os_tag = .windows,
                .abi = .msvc,
                .ofmt = .coff,
            };
            const target = std.zig.system.resolveTargetQuery(io, query) catch return Error.WindowsSDKNotFound;

            var environ_map = std.process.Environ.empty.createMap(arena) catch return Error.OutOfMemory;
            defer environ_map.deinit();
            const native_libc = std.zig.LibCInstallation.findNative(arena, io, .{
                .target = &target,
                .environ_map = &environ_map,
            }) catch return Error.WindowsSDKNotFound;

            if (native_libc.crt_dir) |lib_dir| {
                try args.append(allocator, try std.fmt.allocPrint(arena, "/libpath:{s}", .{lib_dir}));
            } else return Error.WindowsSDKNotFound;

            if (native_libc.msvc_lib_dir) |lib_dir| {
                try args.append(allocator, try std.fmt.allocPrint(arena, "/libpath:{s}", .{lib_dir}));
            } else return Error.WindowsSDKNotFound;

            if (native_libc.kernel32_lib_dir) |lib_dir| {
                try args.append(allocator, try std.fmt.allocPrint(arena, "/libpath:{s}", .{lib_dir}));
            } else return Error.WindowsSDKNotFound;

            try args.append(allocator, "/defaultlib:kernel32");
            try args.append(allocator, "/defaultlib:ntdll");
            try args.append(allocator, "/defaultlib:msvcrt");
        },
        else => return Error.LinkFailed,
    }

    const format: embedded_lld.Format = switch (builtin.os.tag) {
        .macos => .macho,
        .linux, .freebsd, .openbsd, .netbsd => .elf,
        .windows => .coff,
        else => return Error.LinkFailed,
    };

    embedded_lld.link(allocator, format, args.items, .{}) catch |err| switch (err) {
        error.OutOfMemory => return Error.OutOfMemory,
        error.LinkFailed => return Error.LinkFailed,
    };
}

fn getTempDir(allocator: Allocator) (Allocator.Error || error{TempDirUnavailable})![]u8 {
    const names: []const [:0]const u8 = if (builtin.os.tag == .windows)
        &.{ "TEMP", "TMP" }
    else
        &.{ "TMPDIR", "TEMP", "TMP" };

    for (names) |name| {
        if (std.c.getenv(name.ptr)) |value_z| {
            const value = std.mem.sliceTo(value_z, 0);
            if (value.len != 0) return allocator.dupe(u8, value);
        }
    }

    return error.TempDirUnavailable;
}

/// Create a unique temporary file path for an artifact output.
fn createTempPath(allocator: Allocator, io: std.Io, extension: []const u8) TempPathError![:0]const u8 {
    const counter = temp_path_counter.fetchAdd(1, .monotonic);
    // zig 0.16 removed std.crypto.random; seed a PRNG from the per-call counter
    // mixed with the pid for cross-process uniqueness of the temp path.
    const pid: u64 = if (builtin.os.tag == .windows)
        std.os.windows.GetCurrentProcessId()
    else
        @intCast(std.c.getpid());
    var prng = std.Random.DefaultPrng.init((@as(u64, counter) << 32) ^ pid);
    const rng = prng.random();
    const random_hi = rng.int(u64);
    const random_lo = rng.int(u64);

    const temp_dir = try getTempDir(allocator);
    defer allocator.free(temp_dir);
    try std.Io.Dir.cwd().createDirPath(io, temp_dir);
    const temp_dir_abs = if (std.fs.path.isAbsolute(temp_dir))
        try allocator.dupe(u8, temp_dir)
    else
        try std.Io.Dir.cwd().realPathFileAlloc(io, temp_dir, allocator);
    defer allocator.free(temp_dir_abs);

    const filename = try std.fmt.allocPrint(
        allocator,
        "roc_llvm_{x}_{x}_{x}{s}",
        .{ random_hi, random_lo, counter, extension },
    );
    defer allocator.free(filename);

    return try std.fs.path.joinZ(allocator, &.{ temp_dir_abs, filename });
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
