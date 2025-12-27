//! LLVM-based Evaluator for Roc expressions
//!
//! This module provides an alternative to the interpreter that uses LLVM
//! to compile and execute Roc expressions. It's used when the `--optimize`
//! flag is passed to the REPL.
//!
//! The evaluator works by:
//! 1. Taking a CIR expression
//! 2. Translating it to LLVM IR
//! 3. Compiling to object code
//! 4. Linking with a minimal runtime
//! 5. Executing and capturing the result
//!
//! Note: For the initial implementation, we use ahead-of-time compilation.
//! JIT compilation can be added later for better performance.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const types = @import("types");
const can = @import("can");
const layout = @import("layout");
const builtins = @import("builtins");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;

/// LLVM-based evaluator for Roc expressions
pub const LlvmEvaluator = struct {
    allocator: Allocator,

    /// Temporary directory for object files
    temp_dir: ?[]const u8,

    /// Counter for unique file names
    counter: u64,

    pub const Error = error{
        OutOfMemory,
        CompilationFailed,
        LinkingFailed,
        ExecutionFailed,
        UnsupportedType,
        NotImplemented,
    };

    /// Initialize a new LLVM evaluator
    pub fn init(allocator: Allocator) Error!LlvmEvaluator {
        return LlvmEvaluator{
            .allocator = allocator,
            .temp_dir = null,
            .counter = 0,
        };
    }

    /// Clean up the evaluator
    pub fn deinit(self: *LlvmEvaluator) void {
        if (self.temp_dir) |dir| {
            self.allocator.free(dir);
        }
    }

    /// Evaluate a CIR expression and return its string representation
    /// This is the main entry point for REPL evaluation with LLVM
    pub fn evalToString(
        self: *LlvmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
        type_var: types.Var,
    ) Error![]const u8 {
        _ = type_var;
        _ = expr_idx;
        _ = module_env;

        // TODO: Implement the full pipeline:
        // 1. Run monomorphization on the expression
        // 2. Run closure transformation
        // 3. Translate CIR to LLVM IR using emit.zig
        // 4. Compile to object code using codegen.zig
        // 5. Link with runtime
        // 6. Execute and capture output
        // 7. Parse output and format as Roc value

        // For now, return a placeholder indicating LLVM backend is not yet implemented
        return try self.allocator.dupe(u8, "<LLVM backend: not yet implemented>");
    }

    /// Evaluate a simple numeric expression (for testing)
    /// This provides a simpler path for testing the LLVM pipeline
    pub fn evalNumericExpr(
        self: *LlvmEvaluator,
        module_env: *ModuleEnv,
        expr_idx: CIR.Expr.Idx,
    ) Error!i64 {
        _ = self;
        _ = module_env;
        _ = expr_idx;

        // TODO: Implement numeric expression evaluation
        // This is a stepping stone before full expression support
        return error.NotImplemented;
    }

    /// Get the target triple for the current host
    fn getHostTriple(self: *LlvmEvaluator) []const u8 {
        _ = self;
        return switch (builtin.os.tag) {
            .linux => switch (builtin.cpu.arch) {
                .x86_64 => "x86_64-linux-gnu",
                .aarch64 => "aarch64-linux-gnu",
                else => "unknown-linux-gnu",
            },
            .macos => switch (builtin.cpu.arch) {
                .x86_64 => "x86_64-macos-none",
                .aarch64 => "aarch64-macos-none",
                else => "unknown-macos-none",
            },
            .windows => "x86_64-windows-msvc",
            else => "unknown-unknown-unknown",
        };
    }

    /// Generate a unique temporary file path
    fn getTempPath(self: *LlvmEvaluator, extension: []const u8) Error![]const u8 {
        self.counter += 1;
        return std.fmt.allocPrint(
            self.allocator,
            "/tmp/roc_llvm_{d}{s}",
            .{ self.counter, extension },
        ) catch return error.OutOfMemory;
    }

    /// Execute a compiled program and capture its output
    fn executeAndCapture(self: *LlvmEvaluator, exe_path: []const u8) Error![]const u8 {
        var child = std.process.Child.init(
            &.{exe_path},
            self.allocator,
        );
        child.stdout_behavior = .Pipe;
        child.stderr_behavior = .Pipe;

        child.spawn() catch return error.ExecutionFailed;

        const stdout = child.stdout orelse return error.ExecutionFailed;
        const output = stdout.reader().readAllAlloc(self.allocator, 1024 * 1024) catch return error.ExecutionFailed;

        const term = child.wait() catch return error.ExecutionFailed;

        if (term.Exited != 0) {
            self.allocator.free(output);
            return error.ExecutionFailed;
        }

        return output;
    }
};

/// LLVM emit NumKind enum (duplicated here since we can't import from backend)
/// This must be kept in sync with src/backend/llvm/emit.zig
pub const LlvmNumKind = enum {
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
    numeral,
};

/// Translate a CIR NumKind to the LLVM emit NumKind
/// Returns null for unbound numeric types that need type inference first
pub fn cirNumKindToLlvmNumKind(cir_kind: CIR.NumKind) ?LlvmNumKind {
    return switch (cir_kind) {
        // Unbound types need to be resolved through type inference first
        .num_unbound, .int_unbound => null,
        // Concrete numeric types
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
    };
}

test "llvm evaluator initialization" {
    const allocator = std.testing.allocator;

    var evaluator = try LlvmEvaluator.init(allocator);
    defer evaluator.deinit();

    // Just verify initialization works
    try std.testing.expect(evaluator.counter == 0);
}
