//! LLVM-based compilation infrastructure for Roc

const std = @import("std");
const builtin = @import("builtin");
const target = @import("target.zig");
const reporting = @import("reporting");

const Allocator = std.mem.Allocator;

const is_windows = builtin.target.os.tag == .windows;

var stderr_file_writer: std.Io.File.Writer = .{
    .io = std.Io.Threaded.global_single_threaded.io(),
    .interface = std.Io.File.Writer.initInterface(&.{}),
    .file = if (is_windows) undefined else std.Io.File.stderr(),
    .mode = .streaming,
};

fn stderrWriter() *std.Io.Writer {
    if (is_windows) stderr_file_writer.file = std.Io.File.stderr();
    return &stderr_file_writer.interface;
}

/// Optimization levels for compilation
pub const OptimizationLevel = enum {
    size, // --opt size (optimize for binary size)
    speed, // --opt speed (aggressive performance optimizations)

    fn toLLVMCodeGenLevel(self: OptimizationLevel) c_int {
        return switch (self) {
            .size => LLVMCodeGenLevelDefault,
            .speed => LLVMCodeGenLevelAggressive,
        };
    }

    fn toLLVMIRLevel(self: OptimizationLevel) ZigLLVMIROptimizationLevel {
        return switch (self) {
            .size => .oz,
            .speed => .o3,
        };
    }
};

/// Configuration for compiling LLVM bitcode to object files
pub const CompileConfig = struct {
    input_path: []const u8,
    output_path: []const u8,
    optimization: OptimizationLevel,
    target: target.RocTarget,
    cpu: []const u8 = "",
    features: []const u8 = "",
    debug: bool = false, // Enable debug info generation in output
    link_builtins: bool = false,
};

// Check if LLVM is available at compile time
const llvm_available = if (@import("builtin").is_test) false else @import("config").llvm;

// LLVM ABI Types
const ZigLLVMABIType = enum(c_int) {
    ZigLLVMABITypeDefault = 0,
    ZigLLVMABITypeSoft,
    ZigLLVMABITypeHard,
};

const ZigLLVMCoverageType = enum(c_int) {
    ZigLLVMCoverageType_None = 0,
    ZigLLVMCoverageType_Function,
    ZigLLVMCoverageType_BB,
    ZigLLVMCoverageType_Edge,
};

const ZigLLVMCoverageOptions = extern struct {
    CoverageType: ZigLLVMCoverageType,
    IndirectCalls: bool,
    TraceBB: bool,
    TraceCmp: bool,
    TraceDiv: bool,
    TraceGep: bool,
    Use8bitCounters: bool,
    TracePC: bool,
    TracePCGuard: bool,
    Inline8bitCounters: bool,
    InlineBoolFlag: bool,
    PCTable: bool,
    NoPrune: bool,
    StackDepth: bool,
    TraceLoads: bool,
    TraceStores: bool,
    CollectControlFlow: bool,
};

const ZigLLVMThinOrFullLTOPhase = enum(c_int) {
    ZigLLVMThinOrFullLTOPhase_None,
    ZigLLVMThinOrFullLTOPhase_ThinPreLink,
    ZigLLVMThinOrFullLTOPhase_ThinkPostLink,
    ZigLLVMThinOrFullLTOPhase_FullPreLink,
    ZigLLVMThinOrFullLTOPhase_FullPostLink,
};

const ZigLLVMIROptimizationLevel = enum(c_int) {
    oz = 0,
    o3 = 1,
};

const ZigLLVMEmitOptions = extern struct {
    is_debug: bool,
    ir_opt_level: ZigLLVMIROptimizationLevel,
    time_report_out: ?*?[*:0]u8,
    tsan: bool,
    sancov: bool,
    lto: ZigLLVMThinOrFullLTOPhase,
    allow_fast_isel: bool,
    allow_machine_outliner: bool,
    asm_filename: ?[*:0]const u8,
    bin_filename: ?[*:0]const u8,
    llvm_ir_filename: ?[*:0]const u8,
    bitcode_filename: ?[*:0]const u8,
    coverage: ZigLLVMCoverageOptions,
};

// LLVM Code Generation Optimization Levels
const LLVMCodeGenLevelDefault: c_int = 2;
const LLVMCodeGenLevelAggressive: c_int = 3;

// LLVM Relocation Models
const LLVMRelocDefault: c_int = 0;

// LLVM Code Models
const LLVMCodeModelDefault: c_int = 0;

// External C functions from zig_llvm.cpp and LLVM C API - only available when LLVM is enabled
const llvm_externs = if (llvm_available) struct {
    extern fn ZigLLVMTargetMachineEmitToFile(
        targ_machine_ref: ?*anyopaque,
        module_ref: ?*anyopaque,
        error_message: *[*:0]u8,
        options: *const ZigLLVMEmitOptions,
    ) bool;
    extern fn ZigLLVMCreateTargetMachine(
        target_ref: ?*anyopaque,
        triple: [*:0]const u8,
        cpu: [*:0]const u8,
        features: [*:0]const u8,
        level: c_int, // LLVMCodeGenOptLevel
        reloc: c_int, // LLVMRelocMode
        code_model: c_int, // LLVMCodeModel
        function_sections: bool,
        data_sections: bool,
        float_abi: ZigLLVMABIType,
        abi_name: ?[*:0]const u8,
        emulated_tls: bool,
    ) ?*anyopaque;

    // LLVM wrapper functions
    extern fn ZigLLVMInitializeAllTargets() void;

    // LLVM C API functions
    extern fn LLVMGetDefaultTargetTriple() [*:0]u8;
    extern fn LLVMGetTargetFromTriple(triple: [*:0]const u8, target: *?*anyopaque, error_message: *[*:0]u8) c_int;
    extern fn LLVMDisposeMessage(message: [*:0]u8) void;
    extern fn LLVMCreateMemoryBufferWithContentsOfFile(path: [*:0]const u8, out_mem_buf: *?*anyopaque, out_message: *[*:0]u8) c_int;
    extern fn LLVMParseBitcode(mem_buf: ?*anyopaque, out_module: *?*anyopaque, out_message: *[*:0]u8) c_int;
    extern fn LLVMDisposeMemoryBuffer(mem_buf: ?*anyopaque) void;
    extern fn LLVMDisposeModule(module: ?*anyopaque) void;
    extern fn LLVMDisposeTargetMachine(target_machine: ?*anyopaque) void;
    extern fn LLVMSetTarget(module: ?*anyopaque, triple: [*:0]const u8) void;
    // Functions for linking builtin bitcode into the app module so the optimizer
    // can inline builtin calls (e.g. list_append_unsafe) instead of leaving them
    // as opaque external calls.
    extern fn LLVMCreateMemoryBufferWithMemoryRangeCopy(data: [*]const u8, len: usize, name: [*:0]const u8) ?*anyopaque;
    extern fn LLVMParseBitcode2(mem_buf: ?*anyopaque, out_module: *?*anyopaque) c_int;
    extern fn LLVMLinkModules2(dest: ?*anyopaque, src: ?*anyopaque) c_int;
    extern fn LLVMGetFirstFunction(module: ?*anyopaque) ?*anyopaque;
    extern fn LLVMGetNextFunction(fn_val: ?*anyopaque) ?*anyopaque;
    extern fn LLVMGetFirstGlobal(module: ?*anyopaque) ?*anyopaque;
    extern fn LLVMGetNextGlobal(global: ?*anyopaque) ?*anyopaque;
    extern fn LLVMGetNamedGlobal(module: ?*anyopaque, name: [*:0]const u8) ?*anyopaque;
    extern fn LLVMDeleteGlobal(global: ?*anyopaque) void;
    extern fn LLVMGetValueName2(val: ?*anyopaque, len: *usize) [*]const u8;
    extern fn LLVMIsDeclaration(global: ?*anyopaque) c_int;
    extern fn LLVMSetLinkage(global: ?*anyopaque, linkage: c_int) void;
    extern fn LLVMGetDataLayoutStr(module: ?*anyopaque) [*:0]const u8;
    extern fn LLVMSetDataLayout(module: ?*anyopaque, data_layout: [*:0]const u8) void;
    // @export wrappers are GlobalAliases (clean name -> dev_wrappers.* function).
    // The inliner runs before LLVM resolves aliases, so we resolve them ourselves
    // (replace uses with the aliasee) before optimization so calls become direct.
    extern fn LLVMGetFirstGlobalAlias(module: ?*anyopaque) ?*anyopaque;
    extern fn LLVMGetNextGlobalAlias(alias: ?*anyopaque) ?*anyopaque;
    extern fn LLVMAliasGetAliasee(alias: ?*anyopaque) ?*anyopaque;
    extern fn LLVMReplaceAllUsesWith(old_val: ?*anyopaque, new_val: ?*anyopaque) void;
    // The builtins were compiled with explicit target-cpu/target-features; the app
    // functions have none, and the inliner refuses to inline a callee whose features
    // aren't a subset of the caller's. Strip them (the target machine still pins the
    // CPU/features at codegen) so the builtins become inlinable.
    extern fn LLVMRemoveStringAttributeAtIndex(fn_val: ?*anyopaque, idx: c_uint, name: [*]const u8, len: c_uint) void;
    extern fn ZigLLVMRunGlobalDCE(module: ?*anyopaque) void;
} else struct {};

/// Embedded builtin bitcode. Stubbed out when LLVM is unavailable.
const llvm_embedded = if (llvm_available) @import("llvm_embedded") else struct {
    pub const builtins_bc: []const u8 = "";
    pub const builtins32_bc: []const u8 = "";
    pub const builtins64_bc: []const u8 = "";
};

/// LLVM-C linkage value for `internal`: a local definition, never an exported
/// symbol, and discarded by global DCE when unused.
const LLVMInternalLinkage: c_int = 8;

/// LLVM-C attribute index for function-level attributes (`~0U`).
const LLVMAttributeFunctionIndex: c_uint = 0xFFFFFFFF;

/// Initialize LLVM targets (must be called once before using LLVM)
pub fn initializeLLVM() void {
    if (comptime !llvm_available) {
        return;
    }
    const externs = llvm_externs;
    externs.ZigLLVMInitializeAllTargets();
}

/// Compile LLVM bitcode file to object file
pub fn compileBitcodeToObject(gpa: Allocator, std_io: std.Io, config: CompileConfig) Allocator.Error!bool {
    if (comptime !llvm_available) {
        try renderLLVMNotAvailableError(gpa);
        return error.LLVMNotAvailable;
    }

    const externs = llvm_externs;

    std.log.debug("Starting bitcode to object compilation", .{});
    std.log.debug("Input: {s} -> Output: {s}", .{ config.input_path, config.output_path });
    std.log.debug("Target: {} ({s})", .{ config.target, config.target.toTriple() });
    std.log.debug("Optimization: {}", .{config.optimization});
    std.log.debug("CPU: '{s}', Features: '{s}'", .{ config.cpu, config.features });

    // Verify input file exists
    std.Io.Dir.cwd().access(std_io, config.input_path, .{}) catch |err| {
        try renderFileNotAccessibleError(gpa, config.input_path, err);
        return false;
    };

    // 1. Initialize LLVM targets
    std.log.debug("Initializing LLVM targets...", .{});
    initializeLLVM();
    std.log.debug("LLVM targets initialized successfully", .{});

    // 2. Load bitcode file
    std.log.debug("Loading bitcode file: {s}", .{config.input_path});
    var mem_buf: ?*anyopaque = null;
    var error_message: [*:0]u8 = undefined;

    const bitcode_path_z = try gpa.dupeZ(u8, config.input_path);
    defer gpa.free(bitcode_path_z);

    if (externs.LLVMCreateMemoryBufferWithContentsOfFile(bitcode_path_z.ptr, &mem_buf, &error_message) != 0) {
        try renderLLVMError(gpa, "BITCODE LOAD ERROR", "Failed to load bitcode file", std.mem.span(error_message));
        externs.LLVMDisposeMessage(error_message);
        return false;
    }
    defer if (mem_buf) |buf| externs.LLVMDisposeMemoryBuffer(buf);
    std.log.debug("Bitcode file loaded successfully", .{});

    // 3. Parse bitcode into module
    std.log.debug("Parsing bitcode into LLVM module...", .{});
    var module: ?*anyopaque = null;
    if (externs.LLVMParseBitcode(mem_buf, &module, &error_message) != 0) {
        try renderLLVMError(gpa, "BITCODE PARSE ERROR", "Failed to parse bitcode", std.mem.span(error_message));
        externs.LLVMDisposeMessage(error_message);
        return false;
    }
    defer if (module) |mod| externs.LLVMDisposeModule(mod);
    std.log.debug("Bitcode parsed successfully", .{});

    // 4. Get target triple and set it on the module
    const target_triple = config.target.toTriple();
    const target_triple_z = try gpa.dupeZ(u8, target_triple);
    defer gpa.free(target_triple_z);

    std.log.debug("Setting target triple on module: {s}", .{target_triple});
    externs.LLVMSetTarget(module, target_triple_z.ptr);
    std.log.debug("Target triple set successfully", .{});

    // LLVM builds link builtin bitcode before optimization and emit the used
    // builtin definitions from the merged app object. There is no roc_builtins.o
    // in LLVM final links, so builtin definitions remain real definitions (not
    // available_externally). Builtin aliases and definitions are internalized
    // after app references are resolved so LLVM and the final linker can remove
    // unused builtin code.
    if (config.link_builtins) {
        const builtins_bc = switch (config.target.ptrBitWidth()) {
            32 => llvm_embedded.builtins32_bc,
            64 => llvm_embedded.builtins64_bc,
            else => "",
        };
        if (builtins_bc.len == 0) {
            std.log.err("No embedded builtin bitcode for {d}-bit target pointers", .{config.target.ptrBitWidth()});
            return false;
        }

        var app_defs = std.StringHashMap(void).init(gpa);
        defer {
            var keys = app_defs.keyIterator();
            while (keys.next()) |k| gpa.free(k.*);
            app_defs.deinit();
        }

        var app_decls = std.StringHashMap(void).init(gpa);
        defer {
            var keys = app_decls.keyIterator();
            while (keys.next()) |k| gpa.free(k.*);
            app_decls.deinit();
        }

        var pre_func = externs.LLVMGetFirstFunction(module);
        while (pre_func) |fv| : (pre_func = externs.LLVMGetNextFunction(fv)) {
            var name_len: usize = 0;
            const name_ptr = externs.LLVMGetValueName2(fv, &name_len);
            const name_slice = name_ptr[0..name_len];
            const name = try gpa.dupe(u8, name_slice);
            errdefer gpa.free(name);
            if (externs.LLVMIsDeclaration(fv) != 0) {
                try app_decls.put(name, {});
            } else {
                try app_defs.put(name, {});
            }
        }

        var pre_global = externs.LLVMGetFirstGlobal(module);
        while (pre_global) |gv| : (pre_global = externs.LLVMGetNextGlobal(gv)) {
            var name_len: usize = 0;
            const name_ptr = externs.LLVMGetValueName2(gv, &name_len);
            const name_slice = name_ptr[0..name_len];
            const name = try gpa.dupe(u8, name_slice);
            errdefer gpa.free(name);
            if (externs.LLVMIsDeclaration(gv) != 0) {
                try app_decls.put(name, {});
            } else {
                try app_defs.put(name, {});
            }
        }

        var pre_alias = externs.LLVMGetFirstGlobalAlias(module);
        while (pre_alias) |av| : (pre_alias = externs.LLVMGetNextGlobalAlias(av)) {
            var name_len: usize = 0;
            const name_ptr = externs.LLVMGetValueName2(av, &name_len);
            const name = try gpa.dupe(u8, name_ptr[0..name_len]);
            errdefer gpa.free(name);
            try app_defs.put(name, {});
        }

        const bc_buf = externs.LLVMCreateMemoryBufferWithMemoryRangeCopy(builtins_bc.ptr, builtins_bc.len, "roc_builtins_bc");
        var builtins_module: ?*anyopaque = null;
        if (externs.LLVMParseBitcode2(bc_buf, &builtins_module) == 0) {
            externs.LLVMSetTarget(builtins_module, target_triple_z.ptr);
            externs.LLVMSetDataLayout(builtins_module, externs.LLVMGetDataLayoutStr(module));
            if (externs.LLVMGetNamedGlobal(builtins_module, "llvm.used")) |used| {
                externs.LLVMDeleteGlobal(used);
            }
            if (externs.LLVMGetNamedGlobal(builtins_module, "llvm.compiler.used")) |compiler_used| {
                externs.LLVMDeleteGlobal(compiler_used);
            }

            var builtin_alias = externs.LLVMGetFirstGlobalAlias(builtins_module);
            while (builtin_alias) |a| : (builtin_alias = externs.LLVMGetNextGlobalAlias(a)) {
                var name_len: usize = 0;
                const name_ptr = externs.LLVMGetValueName2(a, &name_len);
                if (!app_decls.contains(name_ptr[0..name_len])) {
                    externs.LLVMSetLinkage(a, LLVMInternalLinkage);
                }
            }

            var builtin_func = externs.LLVMGetFirstFunction(builtins_module);
            while (builtin_func) |fv| : (builtin_func = externs.LLVMGetNextFunction(fv)) {
                if (externs.LLVMIsDeclaration(fv) != 0) continue;
                externs.LLVMRemoveStringAttributeAtIndex(fv, LLVMAttributeFunctionIndex, "target-features", "target-features".len);
                externs.LLVMRemoveStringAttributeAtIndex(fv, LLVMAttributeFunctionIndex, "target-cpu", "target-cpu".len);

                var name_len: usize = 0;
                const name_ptr = externs.LLVMGetValueName2(fv, &name_len);
                if (!app_decls.contains(name_ptr[0..name_len])) {
                    externs.LLVMSetLinkage(fv, LLVMInternalLinkage);
                }
            }

            var builtin_global = externs.LLVMGetFirstGlobal(builtins_module);
            while (builtin_global) |gv| : (builtin_global = externs.LLVMGetNextGlobal(gv)) {
                if (externs.LLVMIsDeclaration(gv) != 0) continue;
                var name_len: usize = 0;
                const name_ptr = externs.LLVMGetValueName2(gv, &name_len);
                if (!app_decls.contains(name_ptr[0..name_len])) {
                    externs.LLVMSetLinkage(gv, LLVMInternalLinkage);
                }
            }

            if (externs.LLVMLinkModules2(module, builtins_module) == 0) {
                // Resolve @export aliases (clean builtin name -> dev_wrappers.* fn)
                // so calls target the real function directly and can be inlined.
                var alias = externs.LLVMGetFirstGlobalAlias(module);
                while (alias) |a| : (alias = externs.LLVMGetNextGlobalAlias(a)) {
                    var name_len: usize = 0;
                    const name_ptr = externs.LLVMGetValueName2(a, &name_len);
                    if (app_defs.contains(name_ptr[0..name_len])) continue;
                    if (externs.LLVMAliasGetAliasee(a)) |aliasee| {
                        externs.LLVMReplaceAllUsesWith(a, aliasee);
                    }
                    externs.LLVMSetLinkage(a, LLVMInternalLinkage);
                }

                var post_func = externs.LLVMGetFirstFunction(module);
                while (post_func) |fv| : (post_func = externs.LLVMGetNextFunction(fv)) {
                    if (externs.LLVMIsDeclaration(fv) != 0) continue;
                    var name_len: usize = 0;
                    const name_ptr = externs.LLVMGetValueName2(fv, &name_len);
                    if (!app_defs.contains(name_ptr[0..name_len])) {
                        // Strip target-cpu/target-features so the inliner considers
                        // the builtin compatible with the app functions. The target
                        // machine still controls CPU/features at codegen.
                        externs.LLVMRemoveStringAttributeAtIndex(fv, LLVMAttributeFunctionIndex, "target-features", "target-features".len);
                        externs.LLVMRemoveStringAttributeAtIndex(fv, LLVMAttributeFunctionIndex, "target-cpu", "target-cpu".len);
                        externs.LLVMSetLinkage(fv, LLVMInternalLinkage);
                    }
                }

                var post_global = externs.LLVMGetFirstGlobal(module);
                while (post_global) |gv| : (post_global = externs.LLVMGetNextGlobal(gv)) {
                    if (externs.LLVMIsDeclaration(gv) != 0) continue;
                    var name_len: usize = 0;
                    const name_ptr = externs.LLVMGetValueName2(gv, &name_len);
                    if (!app_defs.contains(name_ptr[0..name_len])) {
                        externs.LLVMSetLinkage(gv, LLVMInternalLinkage);
                    }
                }
                externs.ZigLLVMRunGlobalDCE(module);
            } else {
                std.log.err("Failed to link builtin bitcode into app module", .{});
                return false;
            }
        } else {
            std.log.err("Failed to parse builtin bitcode", .{});
            return false;
        }
    }

    // 5. Create target
    std.log.debug("Getting LLVM target for triple: {s}", .{target_triple});
    var llvm_target: ?*anyopaque = null;
    if (externs.LLVMGetTargetFromTriple(target_triple_z.ptr, &llvm_target, &error_message) != 0) {
        try renderTargetError(gpa, target_triple, std.mem.span(error_message));
        externs.LLVMDisposeMessage(error_message);
        return false;
    }
    std.log.debug("LLVM target obtained successfully", .{});

    // 6. Create target machine
    const cpu_z = try gpa.dupeZ(u8, config.cpu);
    defer gpa.free(cpu_z);
    const features_z = try gpa.dupeZ(u8, config.features);
    defer gpa.free(features_z);

    std.log.debug("Creating target machine with CPU='{s}', Features='{s}'", .{ config.cpu, config.features });
    const target_machine = externs.ZigLLVMCreateTargetMachine(
        llvm_target,
        target_triple_z.ptr,
        cpu_z.ptr,
        features_z.ptr,
        config.optimization.toLLVMCodeGenLevel(),
        LLVMRelocDefault,
        LLVMCodeModelDefault,
        true, // function_sections
        true, // data_sections
        .ZigLLVMABITypeDefault, // float_abi
        null, // abi_name
        false, // emulated_tls
    );
    if (target_machine == null) {
        try renderTargetMachineError(gpa, target_triple, config.cpu, config.features);
        return false;
    }
    defer externs.LLVMDisposeTargetMachine(target_machine);
    std.log.debug("Target machine created successfully", .{});

    // 7. Prepare output path
    const object_path_z = try gpa.dupeZ(u8, config.output_path);
    defer gpa.free(object_path_z);

    // 8. Emit object file
    std.log.debug("Emitting object file to: {s}", .{config.output_path});
    var emit_error_message: [*:0]u8 = undefined;

    var coverage_options = std.mem.zeroes(ZigLLVMCoverageOptions);
    coverage_options.CoverageType = .ZigLLVMCoverageType_None;

    const emit_options = ZigLLVMEmitOptions{
        // Auto-enable debug when roc is built in debug mode, OR when explicitly requested via --debug
        .is_debug = (builtin.mode == .Debug) or config.debug,
        .ir_opt_level = config.optimization.toLLVMIRLevel(),
        .time_report_out = null,
        .tsan = false,
        .sancov = false,
        .lto = .ZigLLVMThinOrFullLTOPhase_None,
        .allow_fast_isel = false,
        .allow_machine_outliner = true,
        .asm_filename = null,
        .bin_filename = object_path_z.ptr,
        .llvm_ir_filename = null,
        .bitcode_filename = null,
        .coverage = coverage_options,
    };

    const emit_result = externs.ZigLLVMTargetMachineEmitToFile(
        target_machine,
        module,
        &emit_error_message,
        &emit_options,
    );

    if (emit_result) {
        try renderEmitError(gpa, config.output_path, std.mem.span(emit_error_message));
        externs.LLVMDisposeMessage(emit_error_message);
        return false;
    }

    std.log.debug("Successfully compiled bitcode to object file: {s}", .{config.output_path});
    return true;
}

test "LLVM optimization option mapping" {
    try std.testing.expectEqual(LLVMCodeGenLevelDefault, OptimizationLevel.size.toLLVMCodeGenLevel());
    try std.testing.expectEqual(LLVMCodeGenLevelAggressive, OptimizationLevel.speed.toLLVMCodeGenLevel());
    try std.testing.expectEqual(ZigLLVMIROptimizationLevel.oz, OptimizationLevel.size.toLLVMIRLevel());
    try std.testing.expectEqual(ZigLLVMIROptimizationLevel.o3, OptimizationLevel.speed.toLLVMIRLevel());
}

/// Check if LLVM is available
pub fn isLLVMAvailable() bool {
    return llvm_available;
}

// --- Error Reporting Helpers ---

fn renderLLVMNotAvailableError(allocator: Allocator) Allocator.Error!void {
    var report = reporting.Report.init(allocator, "LLVM NOT AVAILABLE", .fatal);
    defer report.deinit();

    try report.document.addText("LLVM is not available at compile time.");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("This binary was built without LLVM support.");
    try report.document.addLineBreak();
    try report.document.addText("To use this feature, rebuild roc with LLVM enabled.");
    try report.document.addLineBreak();

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

fn renderFileNotAccessibleError(allocator: Allocator, path: []const u8, err: anyerror) Allocator.Error!void {
    var report = reporting.Report.init(allocator, "FILE NOT ACCESSIBLE", .fatal);
    defer report.deinit();

    try report.document.addText("Input bitcode file does not exist or is not accessible:");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("    ");
    try report.document.addAnnotated(path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("Error: ");
    try report.document.addAnnotated(@errorName(err), .error_highlight);
    try report.document.addLineBreak();

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

fn renderLLVMError(allocator: Allocator, title: []const u8, message: []const u8, llvm_message: []const u8) Allocator.Error!void {
    var report = reporting.Report.init(allocator, title, .fatal);
    defer report.deinit();

    try report.document.addText(message);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("LLVM error: ");
    try report.document.addAnnotated(llvm_message, .error_highlight);
    try report.document.addLineBreak();

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

fn renderTargetError(allocator: Allocator, triple: []const u8, llvm_message: []const u8) Allocator.Error!void {
    var report = reporting.Report.init(allocator, "INVALID TARGET", .fatal);
    defer report.deinit();

    try report.document.addText("Failed to get LLVM target for triple:");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("    ");
    try report.document.addAnnotated(triple, .emphasized);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("LLVM error: ");
    try report.document.addAnnotated(llvm_message, .error_highlight);
    try report.document.addLineBreak();

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

fn renderTargetMachineError(allocator: Allocator, triple: []const u8, cpu: []const u8, features: []const u8) Allocator.Error!void {
    var report = reporting.Report.init(allocator, "TARGET MACHINE ERROR", .fatal);
    defer report.deinit();

    try report.document.addText("Failed to create LLVM target machine with configuration:");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("    Triple:   ");
    try report.document.addAnnotated(triple, .emphasized);
    try report.document.addLineBreak();
    try report.document.addText("    CPU:      ");
    if (cpu.len > 0) {
        try report.document.addAnnotated(cpu, .emphasized);
    } else {
        try report.document.addText("(default)");
    }
    try report.document.addLineBreak();
    try report.document.addText("    Features: ");
    if (features.len > 0) {
        try report.document.addAnnotated(features, .emphasized);
    } else {
        try report.document.addText("(default)");
    }
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("This may indicate an unsupported target configuration.");
    try report.document.addLineBreak();

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

fn renderEmitError(allocator: Allocator, output_path: []const u8, llvm_message: []const u8) Allocator.Error!void {
    var report = reporting.Report.init(allocator, "OBJECT FILE EMIT ERROR", .fatal);
    defer report.deinit();

    try report.document.addText("Failed to emit object file:");
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("    Output: ");
    try report.document.addAnnotated(output_path, .path);
    try report.document.addLineBreak();
    try report.document.addLineBreak();
    try report.document.addText("LLVM error: ");
    try report.document.addAnnotated(llvm_message, .error_highlight);
    try report.document.addLineBreak();

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}
