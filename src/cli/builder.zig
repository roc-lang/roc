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

    /// Check if compiling for the current machine
    pub fn isNative(self: CompileConfig) bool {
        return self.target == target.RocTarget.detectNative();
    }
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
} else struct {};

/// Embedded builtin bitcode, linked into each app module so builtin calls can be
/// inlined by the optimizer. Stubbed out when LLVM is unavailable.
const llvm_embedded = if (llvm_available) @import("llvm_embedded") else struct {
    pub const builtins_bc: []const u8 = "";
};

/// LLVM-C linkage value for `available_externally`: the body is available for
/// inlining but is not emitted (the real definition comes from roc_builtins.o).
const LLVMAvailableExternallyLinkage: c_int = 1;

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

    // Link the builtin bitcode into the app module so the optimizer can inline
    // builtin calls (e.g. list_append_unsafe / list_with_capacity) into the app's
    // hot loops. After linking, every function that came from the builtins module
    // (i.e. wasn't an app definition before the link) is marked
    // `available_externally`: its body is available for inlining but is never
    // emitted, so non-inlined calls are resolved from roc_builtins.o at final link
    // and there are no duplicate symbols. The linkage must be set AFTER linking,
    // because LLVMLinkModules2 promotes available_externally back to external when
    // it resolves an external declaration in the destination module.
    {
        const builtins_bc = llvm_embedded.builtins_bc;
        if (builtins_bc.len > 0) {
            // Record the app's own defined functions before linking.
            var app_defs = std.StringHashMap(void).init(gpa);
            defer {
                var keys = app_defs.keyIterator();
                while (keys.next()) |k| gpa.free(k.*);
                app_defs.deinit();
            }
            var pre = externs.LLVMGetFirstFunction(module);
            while (pre) |fv| : (pre = externs.LLVMGetNextFunction(fv)) {
                if (externs.LLVMIsDeclaration(fv) != 0) continue;
                var name_len: usize = 0;
                const name_ptr = externs.LLVMGetValueName2(fv, &name_len);
                const name = try gpa.dupe(u8, name_ptr[0..name_len]);
                errdefer gpa.free(name);
                try app_defs.put(name, {});
            }

            const bc_buf = externs.LLVMCreateMemoryBufferWithMemoryRangeCopy(builtins_bc.ptr, builtins_bc.len, "roc_builtins_bc");
            var builtins_module: ?*anyopaque = null;
            if (externs.LLVMParseBitcode2(bc_buf, &builtins_module) == 0) {
                externs.LLVMSetTarget(builtins_module, target_triple_z.ptr);
                externs.LLVMSetDataLayout(builtins_module, externs.LLVMGetDataLayoutStr(module));
                if (externs.LLVMLinkModules2(module, builtins_module) == 0) {
                    // Resolve @export aliases (clean builtin name -> dev_wrappers.* fn)
                    // so calls target the real function directly and can be inlined.
                    // Then make each alias internal so the now-unused symbol is not
                    // emitted (avoiding duplicate symbols vs roc_builtins.o).
                    var alias = externs.LLVMGetFirstGlobalAlias(module);
                    while (alias) |a| : (alias = externs.LLVMGetNextGlobalAlias(a)) {
                        if (externs.LLVMAliasGetAliasee(a)) |aliasee| {
                            externs.LLVMReplaceAllUsesWith(a, aliasee);
                            externs.LLVMSetLinkage(a, LLVMInternalLinkage);
                        }
                    }

                    var post = externs.LLVMGetFirstFunction(module);
                    while (post) |fv| : (post = externs.LLVMGetNextFunction(fv)) {
                        if (externs.LLVMIsDeclaration(fv) != 0) continue;
                        var name_len: usize = 0;
                        const name_ptr = externs.LLVMGetValueName2(fv, &name_len);
                        if (!app_defs.contains(name_ptr[0..name_len])) {
                            // Strip target-cpu/target-features so the inliner considers
                            // the builtin compatible with the (feature-less) app functions.
                            externs.LLVMRemoveStringAttributeAtIndex(fv, LLVMAttributeFunctionIndex, "target-features", "target-features".len);
                            externs.LLVMRemoveStringAttributeAtIndex(fv, LLVMAttributeFunctionIndex, "target-cpu", "target-cpu".len);
                            externs.LLVMSetLinkage(fv, LLVMAvailableExternallyLinkage);
                        }
                    }
                } else {
                    std.log.warn("Failed to link builtin bitcode for inlining; builtin calls will not be inlined", .{});
                }
            } else {
                std.log.warn("Failed to parse builtin bitcode for inlining; builtin calls will not be inlined", .{});
            }
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
        false, // function_sections
        false, // data_sections
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
