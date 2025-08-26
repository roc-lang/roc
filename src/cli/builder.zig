//! LLVM-based compilation infrastructure for Roc

const std = @import("std");
const builtin = @import("builtin");
const target = @import("target.zig");

const Allocator = std.mem.Allocator;

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

// LLVM Code Generation Optimization Levels
const LLVMCodeGenLevelNone: c_int = 0;
const LLVMCodeGenLevelLess: c_int = 1;
const LLVMCodeGenLevelDefault: c_int = 2;
const LLVMCodeGenLevelAggressive: c_int = 3;

// LLVM Relocation Models
const LLVMRelocDefault: c_int = 0;
const LLVMRelocStatic: c_int = 1;
const LLVMRelocPIC: c_int = 2;
const LLVMRelocDynamicNoPic: c_int = 3;
const LLVMRelocROPI: c_int = 4;
const LLVMRelocRWPI: c_int = 5;
const LLVMRelocROPI_RWPI: c_int = 6;

// LLVM Code Models
const LLVMCodeModelDefault: c_int = 0;
const LLVMCodeModelJITDefault: c_int = 1;
const LLVMCodeModelTiny: c_int = 2;
const LLVMCodeModelSmall: c_int = 3;
const LLVMCodeModelKernel: c_int = 4;
const LLVMCodeModelMedium: c_int = 5;
const LLVMCodeModelLarge: c_int = 6;

// External C functions from zig_llvm.cpp and LLVM C API - only available when LLVM is enabled
const llvm_externs = if (llvm_available) struct {
    extern fn ZigLLVMTargetMachineEmitToFile(
        targ_machine_ref: ?*anyopaque,
        module_ref: ?*anyopaque,
        error_message: *[*:0]u8,
        is_debug: bool,
        is_small: bool,
        time_report: bool,
        tsan: bool,
        lto: bool,
        asm_filename: ?[*:0]const u8,
        bin_filename: ?[*:0]const u8,
        llvm_ir_filename: ?[*:0]const u8,
        bitcode_filename: ?[*:0]const u8,
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
        return error.LLVMNotAvailable;
    }

    const externs = llvm_externs;

    std.log.debug("Compiling bitcode to object file", .{});
    std.log.debug("Input: {s} -> Output: {s}", .{ config.input_path, config.output_path });
    std.log.debug("Target: {} ({s})", .{ config.target, config.target.toTriple() });
    std.log.debug("Optimization: {}", .{config.optimization});

    // 1. Initialize LLVM targets (safe to call multiple times)
    initializeLLVM();

    // 2. Load bitcode file
    var mem_buf: ?*anyopaque = null;
    var error_message: [*:0]u8 = undefined;

    const bitcode_path_z = try gpa.dupeZ(u8, config.input_path);
    defer gpa.free(bitcode_path_z);

    if (externs.LLVMCreateMemoryBufferWithContentsOfFile(bitcode_path_z.ptr, &mem_buf, &error_message) != 0) {
        std.log.err("Failed to load bitcode file: {s}", .{error_message});
        externs.LLVMDisposeMessage(error_message);
        return false;
    }
    defer if (mem_buf) |buf| externs.LLVMDisposeMemoryBuffer(buf);

    // 3. Parse bitcode into module
    var module: ?*anyopaque = null;
    if (externs.LLVMParseBitcode(mem_buf, &module, &error_message) != 0) {
        std.log.err("Failed to parse bitcode: {s}", .{error_message});
        externs.LLVMDisposeMessage(error_message);
        return false;
    }
    defer if (module) |mod| externs.LLVMDisposeModule(mod);

    // 4. Get target triple and set it on the module
    const target_triple = config.target.toTriple();
    const target_triple_z = try gpa.dupeZ(u8, target_triple);
    defer gpa.free(target_triple_z);

    std.log.debug("Using target triple: {s}", .{target_triple});
    externs.LLVMSetTarget(module, target_triple_z.ptr);

    // 5. Create target
    var llvm_target: ?*anyopaque = null;
    if (externs.LLVMGetTargetFromTriple(target_triple_z.ptr, &llvm_target, &error_message) != 0) {
        std.log.err("Failed to get target from triple: {s}", .{error_message});
        externs.LLVMDisposeMessage(error_message);
        return false;
    }

    // 6. Create target machine
    const cpu_z = try gpa.dupeZ(u8, config.cpu);
    defer gpa.free(cpu_z);
    const features_z = try gpa.dupeZ(u8, config.features);
    defer gpa.free(features_z);

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
    );
    if (target_machine == null) {
        std.log.err("Failed to create target machine", .{});
        return false;
    }
    defer externs.LLVMDisposeTargetMachine(target_machine);

    // 7. Prepare output path
    const object_path_z = try gpa.dupeZ(u8, config.output_path);
    defer gpa.free(object_path_z);

    // 8. Emit object file
    var emit_error_message: [*:0]u8 = undefined;
    if (externs.ZigLLVMTargetMachineEmitToFile(
        target_machine,
        module,
        &emit_error_message,
        false, // is_debug
        config.optimization == .size, // is_small
        false, // time_report
        false, // tsan
        false, // lto
        null, // asm_filename
        object_path_z.ptr, // bin_filename
        null, // llvm_ir_filename
        null, // bitcode_filename
    )) {
        std.log.err("Failed to emit object file: {s}", .{emit_error_message});
        externs.LLVMDisposeMessage(emit_error_message);
        return false;
    }

    std.log.debug("Successfully compiled bitcode to object file", .{});
    return true;
}

/// Check if LLVM is available
pub fn isLLVMAvailable() bool {
    return llvm_available;
}
