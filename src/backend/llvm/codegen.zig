//! High-level Roc Code Generation API
//!
//! This module provides the top-level API for compiling Roc programs to native code.
//! It orchestrates the complete pipeline:
//!
//! 1. Take monomorphized CIR as input
//! 2. Emit LLVM IR using the Builder
//! 3. Serialize to LLVM bitcode
//! 4. Compile bitcode to object code
//! 5. Return the result
//!
//! This module bridges the Roc compiler frontend with the LLVM backend.

const std = @import("../../std.zig");
const Allocator = std.mem.Allocator;
const Builder = @import("Builder.zig");
const emit = @import("emit.zig");
const bindings = @import("bindings.zig");
const bitcode_writer = @import("bitcode_writer.zig");
const target = @import("../../target/mod.zig");

/// Outcome of code generation including path to object file or error details.
pub const CodegenResult = struct {
    /// Path to the generated object file (if successful)
    object_path: ?[]const u8,
    /// Error message (if failed)
    error_message: ?[]const u8,
    /// Whether code generation succeeded
    success: bool,

    /// Creates a successful result with the given object file path.
    pub fn ok(path: []const u8) CodegenResult {
        return .{
            .object_path = path,
            .error_message = null,
            .success = true,
        };
    }

    /// Creates a failed result with the given error message.
    pub fn err(message: []const u8) CodegenResult {
        return .{
            .object_path = null,
            .error_message = message,
            .success = false,
        };
    }
};

/// Options for code generation
pub const CodegenOptions = struct {
    /// Target triple (e.g., "x86_64-linux-gnu", "aarch64-macos-none")
    target_triple: []const u8,
    /// CPU name (e.g., "generic", "skylake") - null for default
    cpu: ?[]const u8 = null,
    /// CPU features - null for default
    features: ?[]const u8 = null,
    /// Output path for the object file
    output_path: []const u8,
    /// Whether to emit debug info and disable optimizations
    is_debug: bool = false,
    /// Whether to optimize for size (-Oz behavior)
    optimize_size: bool = false,
};

/// High-level code generator that manages the compilation pipeline
pub const Codegen = struct {
    allocator: Allocator,
    emitter: emit.LlvmEmitter,

    /// Error types that can occur during code generation.
    pub const Error = error{
        OutOfMemory,
        CompilationFailed,
        BitwideWriteFailed,
    };

    /// Initialize a new code generator
    pub fn init(allocator: Allocator) Error!Codegen {
        const emitter = emit.LlvmEmitter.init(allocator) catch return error.OutOfMemory;
        return Codegen{
            .allocator = allocator,
            .emitter = emitter,
        };
    }

    /// Clean up the code generator
    pub fn deinit(self: *Codegen) void {
        self.emitter.deinit();
    }

    /// Get the underlying emitter for building IR
    pub fn getEmitter(self: *Codegen) *emit.LlvmEmitter {
        return &self.emitter;
    }

    /// Get the underlying LLVM Builder
    pub fn getBuilder(self: *Codegen) *Builder {
        return self.emitter.getBuilder();
    }

    /// Compile the current module to an object file
    pub fn compileToObjectFile(self: *Codegen, options: CodegenOptions) CodegenResult {
        // Get bitcode from the builder
        const builder = self.emitter.getBuilder();

        // Serialize the module to bitcode
        const producer = Builder.Producer{
            .name = "roc",
            .version = .{ .major = 0, .minor = 1, .patch = 0 },
        };

        const bitcode_words = builder.toBitcode(self.allocator, producer) catch {
            return CodegenResult.err("Failed to serialize bitcode");
        };
        defer self.allocator.free(bitcode_words);

        // Convert u32 words to u8 bytes for the bindings
        const bitcode_bytes = std.mem.sliceAsBytes(bitcode_words);

        // Convert strings to null-terminated for C API.
        // Validate lengths upfront to provide clear error messages.
        const triple_max = 255;
        const output_max = 1023;
        const cpu_max = 63;
        const features_max = 255;

        if (options.target_triple.len > triple_max) {
            return CodegenResult.err("Target triple too long (max 255 chars)");
        }
        if (options.output_path.len > output_max) {
            return CodegenResult.err("Output path too long (max 1023 chars)");
        }
        if (options.cpu) |cpu| {
            if (cpu.len > cpu_max) {
                return CodegenResult.err("CPU name too long (max 63 chars)");
            }
        }
        if (options.features) |features| {
            if (features.len > features_max) {
                return CodegenResult.err("Features string too long (max 255 chars)");
            }
        }

        var triple_buf: [triple_max + 1]u8 = undefined;
        const triple_z = std.fmt.bufPrintZ(&triple_buf, "{s}", .{options.target_triple}) catch unreachable;

        var output_buf: [output_max + 1]u8 = undefined;
        const output_z = std.fmt.bufPrintZ(&output_buf, "{s}", .{options.output_path}) catch unreachable;

        // Handle optional CPU string
        var cpu_buf: [cpu_max + 1]u8 = undefined;
        const cpu_z: ?[*:0]const u8 = if (options.cpu) |cpu|
            std.fmt.bufPrintZ(&cpu_buf, "{s}", .{cpu}) catch unreachable
        else
            null;

        // Handle optional features string
        var features_buf: [features_max + 1]u8 = undefined;
        const features_z: ?[*:0]const u8 = if (options.features) |features|
            std.fmt.bufPrintZ(&features_buf, "{s}", .{features}) catch unreachable
        else
            null;

        // Compile bitcode to object file using LLVM bindings
        const error_msg = bindings.compileBitcodeToObject(
            bitcode_bytes,
            triple_z,
            cpu_z,
            features_z,
            output_z,
            options.is_debug,
        );

        if (error_msg) |msg| {
            // Convert C string to slice for error message
            const msg_slice = std.mem.span(msg);
            return CodegenResult.err(msg_slice);
        }

        return CodegenResult.ok(options.output_path);
    }

    /// Emit a simple function that returns a constant i64 value
    /// Useful for testing the pipeline
    pub fn emitConstantFunction(
        self: *Codegen,
        name: []const u8,
        value: i64,
    ) Error!void {
        // Create function type: () -> i64
        const fn_type = self.emitter.createFunctionType(.i64, &.{}) catch return error.OutOfMemory;

        // Add function to module
        const func_idx = self.emitter.addFunction(name, fn_type) catch return error.OutOfMemory;

        // Begin function body
        self.emitter.beginFunction(func_idx) catch return error.OutOfMemory;

        // Emit constant and return it
        const const_val = self.emitter.emitIntConst(.i64, value) catch return error.OutOfMemory;
        self.emitter.emitRet(const_val) catch return error.OutOfMemory;

        // Finish function
        self.emitter.endFunction() catch return error.OutOfMemory;
    }

    /// Emit a simple add function: (a: i64, b: i64) -> i64
    /// Returns a + b
    pub fn emitAddFunction(self: *Codegen, name: []const u8) Error!void {
        // Create function type: (i64, i64) -> i64
        const fn_type = self.emitter.createFunctionType(.i64, &.{ .i64, .i64 }) catch return error.OutOfMemory;

        // Add function to module
        const func_idx = self.emitter.addFunction(name, fn_type) catch return error.OutOfMemory;

        // Begin function body
        self.emitter.beginFunction(func_idx) catch return error.OutOfMemory;

        // Get function parameters
        // In LLVM, function parameters are values in the entry block
        const wip = self.emitter.getWipFunction() orelse return error.OutOfMemory;
        const param0 = wip.arg(0);
        const param1 = wip.arg(1);

        // Emit add and return
        const result = self.emitter.emitAdd(param0, param1) catch return error.OutOfMemory;
        self.emitter.emitRet(result) catch return error.OutOfMemory;

        // Finish function
        self.emitter.endFunction() catch return error.OutOfMemory;
    }
};

/// Get the host target triple
pub fn getHostTriple() []const u8 {
    return target.RocTarget.detectNative().toTriple();
}

// Note: Tests are in a separate file that's part of the main build system
