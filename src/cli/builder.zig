//! LLVM-based compilation infrastructure for Roc

const std = @import("std");
const builtin = @import("builtin");
const target = @import("target.zig");
const reporting = @import("reporting");

const Allocator = std.mem.Allocator;

const is_windows = builtin.target.os.tag == .windows;

var stderr_file_writer: std.fs.File.Writer = .{
    .interface = std.fs.File.Writer.initInterface(&.{}),
    .file = if (is_windows) undefined else std.fs.File.stderr(),
    .mode = .streaming,
};

fn stderrWriter() *std.Io.Writer {
    if (is_windows) stderr_file_writer.file = std.fs.File.stderr();
    return &stderr_file_writer.interface;
}

// Re-export RocTarget from target.zig for backward compatibility
pub const RocTarget = target.RocTarget;

/// Optimization levels for compilation
pub const OptimizationLevel = enum {
    none, // --opt none (no optimizations)
    size, // --opt size (optimize for binary size)
    speed, // --opt speed (aggressive performance optimizations)

    /// Convert to LLVM optimization level
    fn toLLVMLevel(self: OptimizationLevel) c_int {
        return switch (self) {
            .none => LLVMCodeGenLevelNone,
            .size => LLVMCodeGenLevelLess,
            .speed => LLVMCodeGenLevelAggressive,
        };
    }
};

/// Configuration for compiling LLVM bitcode to object files
pub const CompileConfig = struct {
    input_path: []const u8,
    output_path: []const u8,
    optimization: OptimizationLevel,
    target: RocTarget,
    cpu: []const u8 = "",
    features: []const u8 = "",
    debug: bool = false, // Enable debug info generation in output

    /// Check if compiling for the current machine
    pub fn isNative(self: CompileConfig) bool {
        return self.target == RocTarget.detectNative();
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

const ZigLLVMEmitOptions = extern struct {
    is_debug: bool,
    is_small: bool,
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
const LLVMCodeGenLevelNone: c_int = 0;
const LLVMCodeGenLevelLess: c_int = 1;
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
} else struct {};

/// Initialize LLVM targets (must be called once before using LLVM)
pub fn initializeLLVM() void {
    if (comptime !llvm_available) {
        return;
    }
    const externs = llvm_externs;
    externs.ZigLLVMInitializeAllTargets();
}

/// Compile LLVM bitcode file to object file
pub fn compileBitcodeToObject(gpa: Allocator, config: CompileConfig) !bool {
    if (comptime !llvm_available) {
        renderLLVMNotAvailableError(gpa);
        return error.LLVMNotAvailable;
    }

    const externs = llvm_externs;

    std.log.debug("Starting bitcode to object compilation", .{});
    std.log.debug("Input: {s} -> Output: {s}", .{ config.input_path, config.output_path });
    std.log.debug("Target: {} ({s})", .{ config.target, config.target.toTriple() });
    std.log.debug("Optimization: {}", .{config.optimization});
    std.log.debug("CPU: '{s}', Features: '{s}'", .{ config.cpu, config.features });

    // Verify input file exists
    std.fs.cwd().access(config.input_path, .{}) catch |err| {
        renderFileNotAccessibleError(gpa, config.input_path, err);
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
        renderLLVMError(gpa, "BITCODE LOAD ERROR", "Failed to load bitcode file", std.mem.span(error_message));
        externs.LLVMDisposeMessage(error_message);
        return false;
    }
    defer if (mem_buf) |buf| externs.LLVMDisposeMemoryBuffer(buf);
    std.log.debug("Bitcode file loaded successfully", .{});

    // 3. Parse bitcode into module
    std.log.debug("Parsing bitcode into LLVM module...", .{});
    var module: ?*anyopaque = null;
    if (externs.LLVMParseBitcode(mem_buf, &module, &error_message) != 0) {
        renderLLVMError(gpa, "BITCODE PARSE ERROR", "Failed to parse bitcode", std.mem.span(error_message));
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

    // 5. Create target
    std.log.debug("Getting LLVM target for triple: {s}", .{target_triple});
    var llvm_target: ?*anyopaque = null;
    if (externs.LLVMGetTargetFromTriple(target_triple_z.ptr, &llvm_target, &error_message) != 0) {
        renderTargetError(gpa, target_triple, std.mem.span(error_message));
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
        config.optimization.toLLVMLevel(),
        LLVMRelocDefault,
        LLVMCodeModelDefault,
        false, // function_sections
        false, // data_sections
        .ZigLLVMABITypeDefault, // float_abi
        null, // abi_name
        false, // emulated_tls
    );
    if (target_machine == null) {
        renderTargetMachineError(gpa, target_triple, config.cpu, config.features);
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
        .is_small = config.optimization == .size,
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
        renderEmitError(gpa, config.output_path, std.mem.span(emit_error_message));
        externs.LLVMDisposeMessage(emit_error_message);
        return false;
    }

    std.log.debug("Successfully compiled bitcode to object file: {s}", .{config.output_path});
    return true;
}

/// Check if LLVM is available
pub fn isLLVMAvailable() bool {
    return llvm_available;
}

// --- Error Reporting Helpers ---

fn renderLLVMNotAvailableError(allocator: Allocator) void {
    var report = reporting.Report.init(allocator, "LLVM NOT AVAILABLE", .fatal);
    defer report.deinit();

    report.document.addText("LLVM is not available at compile time.") catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("This binary was built without LLVM support.") catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("To use this feature, rebuild roc with LLVM enabled.") catch return;
    report.document.addLineBreak() catch return;

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

fn renderFileNotAccessibleError(allocator: Allocator, path: []const u8, err: anyerror) void {
    var report = reporting.Report.init(allocator, "FILE NOT ACCESSIBLE", .fatal);
    defer report.deinit();

    report.document.addText("Input bitcode file does not exist or is not accessible:") catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("    ") catch return;
    report.document.addAnnotated(path, .path) catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("Error: ") catch return;
    report.document.addAnnotated(@errorName(err), .error_highlight) catch return;
    report.document.addLineBreak() catch return;

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

fn renderLLVMError(allocator: Allocator, title: []const u8, message: []const u8, llvm_message: []const u8) void {
    var report = reporting.Report.init(allocator, title, .fatal);
    defer report.deinit();

    report.document.addText(message) catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("LLVM error: ") catch return;
    report.document.addAnnotated(llvm_message, .error_highlight) catch return;
    report.document.addLineBreak() catch return;

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

fn renderTargetError(allocator: Allocator, triple: []const u8, llvm_message: []const u8) void {
    var report = reporting.Report.init(allocator, "INVALID TARGET", .fatal);
    defer report.deinit();

    report.document.addText("Failed to get LLVM target for triple:") catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("    ") catch return;
    report.document.addAnnotated(triple, .emphasized) catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("LLVM error: ") catch return;
    report.document.addAnnotated(llvm_message, .error_highlight) catch return;
    report.document.addLineBreak() catch return;

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

fn renderTargetMachineError(allocator: Allocator, triple: []const u8, cpu: []const u8, features: []const u8) void {
    var report = reporting.Report.init(allocator, "TARGET MACHINE ERROR", .fatal);
    defer report.deinit();

    report.document.addText("Failed to create LLVM target machine with configuration:") catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("    Triple:   ") catch return;
    report.document.addAnnotated(triple, .emphasized) catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("    CPU:      ") catch return;
    if (cpu.len > 0) {
        report.document.addAnnotated(cpu, .emphasized) catch return;
    } else {
        report.document.addText("(default)") catch return;
    }
    report.document.addLineBreak() catch return;
    report.document.addText("    Features: ") catch return;
    if (features.len > 0) {
        report.document.addAnnotated(features, .emphasized) catch return;
    } else {
        report.document.addText("(default)") catch return;
    }
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("This may indicate an unsupported target configuration.") catch return;
    report.document.addLineBreak() catch return;

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}

fn renderEmitError(allocator: Allocator, output_path: []const u8, llvm_message: []const u8) void {
    var report = reporting.Report.init(allocator, "OBJECT FILE EMIT ERROR", .fatal);
    defer report.deinit();

    report.document.addText("Failed to emit object file:") catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("    Output: ") catch return;
    report.document.addAnnotated(output_path, .path) catch return;
    report.document.addLineBreak() catch return;
    report.document.addLineBreak() catch return;
    report.document.addText("LLVM error: ") catch return;
    report.document.addAnnotated(llvm_message, .error_highlight) catch return;
    report.document.addLineBreak() catch return;

    reporting.renderReportToTerminal(
        &report,
        stderrWriter(),
        .ANSI,
        reporting.ReportingConfig.initColorTerminal(),
    ) catch {};
}
