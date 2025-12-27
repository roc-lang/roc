//! LLVM IR Emitter for Roc
//!
//! This module provides utilities for emitting LLVM IR for Roc programs.
//! It works with the LLVM Builder to construct IR that can be serialized
//! to bitcode and compiled to native object code.
//!
//! The emitter provides:
//! - Type mapping from Roc types to LLVM types
//! - Function construction utilities
//! - Expression emission helpers
//!
//! Note: This module uses the LLVM Builder directly and does not import
//! Roc-specific modules (CIR, types, etc.). Higher-level integration
//! with Roc's IR is done in src/eval/llvm_evaluator.zig.

const std = @import("../../std.zig");
const Allocator = std.mem.Allocator;
const Builder = @import("Builder.zig");

/// Roc numeric type kinds (mirrors CIR.NumKind for type mapping)
pub const NumKind = enum {
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

/// LLVM IR Emitter for Roc programs
pub const LlvmEmitter = struct {
    /// The LLVM IR builder
    builder: *Builder,

    /// Allocator for temporary allocations
    allocator: Allocator,

    /// Current work-in-progress function being built
    wip_function: ?*Builder.WipFunction,

    /// Error type for emission failures
    pub const Error = error{
        OutOfMemory,
        UnsupportedExpression,
        UnsupportedType,
        TypeMismatch,
        InvalidNumKind,
        NoActiveFunction,
    };

    /// Initialize a new LLVM emitter
    pub fn init(allocator: Allocator) Error!LlvmEmitter {
        const builder = allocator.create(Builder) catch return error.OutOfMemory;
        errdefer allocator.destroy(builder);

        builder.* = Builder.init(.{
            .allocator = allocator,
            .strip = false,
            .name = "roc_module",
        }) catch return error.OutOfMemory;
        errdefer builder.deinit();

        return LlvmEmitter{
            .builder = builder,
            .allocator = allocator,
            .wip_function = null,
        };
    }

    /// Clean up the emitter
    pub fn deinit(self: *LlvmEmitter) void {
        if (self.wip_function) |wip| {
            wip.deinit();
            self.allocator.destroy(wip);
        }
        self.builder.deinit();
        self.allocator.destroy(self.builder);
    }

    /// Convert a Roc NumKind to LLVM type
    pub fn numKindToLlvmType(_: *LlvmEmitter, kind: NumKind) Builder.Type {
        return switch (kind) {
            .u8 => .i8,
            .i8 => .i8,
            .u16 => .i16,
            .i16 => .i16,
            .u32 => .i32,
            .i32 => .i32,
            .u64 => .i64,
            .i64 => .i64,
            .u128 => .i128,
            .i128 => .i128,
            .f32 => .float,
            .f64 => .double,
            .dec => .i128, // Dec is stored as i128
            .numeral => .i64, // Default unspecialized numeric type
        };
    }

    /// Emit an integer constant
    pub fn emitIntConst(self: *LlvmEmitter, llvm_type: Builder.Type, value: i64) Error!Builder.Value {
        const constant = self.builder.intConst(llvm_type, value) catch return error.OutOfMemory;
        return constant.toValue();
    }

    /// Emit a floating point constant (f32)
    pub fn emitF32Const(self: *LlvmEmitter, value: f32) Error!Builder.Value {
        const constant = self.builder.floatConst(.float, value) catch return error.OutOfMemory;
        return constant.toValue();
    }

    /// Emit a floating point constant (f64)
    pub fn emitF64Const(self: *LlvmEmitter, value: f64) Error!Builder.Value {
        const constant = self.builder.floatConst(.double, value) catch return error.OutOfMemory;
        return constant.toValue();
    }

    /// Emit an add instruction
    pub fn emitAdd(self: *LlvmEmitter, lhs: Builder.Value, rhs: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.bin(.add, lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Emit a sub instruction
    pub fn emitSub(self: *LlvmEmitter, lhs: Builder.Value, rhs: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.bin(.sub, lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Emit a mul instruction
    pub fn emitMul(self: *LlvmEmitter, lhs: Builder.Value, rhs: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.bin(.mul, lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Emit a signed div instruction
    pub fn emitSDiv(self: *LlvmEmitter, lhs: Builder.Value, rhs: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.bin(.sdiv, lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Emit an unsigned div instruction
    pub fn emitUDiv(self: *LlvmEmitter, lhs: Builder.Value, rhs: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.bin(.udiv, lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Emit a floating point add instruction
    pub fn emitFAdd(self: *LlvmEmitter, lhs: Builder.Value, rhs: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.bin(.fadd, lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Emit a floating point sub instruction
    pub fn emitFSub(self: *LlvmEmitter, lhs: Builder.Value, rhs: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.bin(.fsub, lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Emit a floating point mul instruction
    pub fn emitFMul(self: *LlvmEmitter, lhs: Builder.Value, rhs: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.bin(.fmul, lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Emit a floating point div instruction
    pub fn emitFDiv(self: *LlvmEmitter, lhs: Builder.Value, rhs: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.bin(.fdiv, lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Create an LLVM function type
    pub fn createFunctionType(
        self: *LlvmEmitter,
        return_type: Builder.Type,
        param_types: []const Builder.Type,
    ) Error!Builder.Type {
        return self.builder.fnType(return_type, param_types, .normal) catch return error.OutOfMemory;
    }

    /// Add a function to the module
    pub fn addFunction(
        self: *LlvmEmitter,
        name: []const u8,
        fn_type: Builder.Type,
    ) Error!Builder.Function.Index {
        return self.builder.addFunction(fn_type, name, .default) catch return error.OutOfMemory;
    }

    /// Start building a function body
    pub fn beginFunction(
        self: *LlvmEmitter,
        func_idx: Builder.Function.Index,
    ) Error!void {
        const wip = self.allocator.create(Builder.WipFunction) catch return error.OutOfMemory;
        errdefer self.allocator.destroy(wip);

        wip.* = Builder.WipFunction.init(.{
            .builder = self.builder,
            .function = func_idx,
            .strip = false,
        }) catch return error.OutOfMemory;
        self.wip_function = wip;

        // Create entry block
        _ = wip.block(0, "entry") catch return error.OutOfMemory;
    }

    /// Emit a return instruction
    pub fn emitRet(self: *LlvmEmitter, return_val: Builder.Value) Error!void {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        _ = wip.ret(return_val) catch return error.OutOfMemory;
    }

    /// Emit a void return instruction
    pub fn emitRetVoid(self: *LlvmEmitter) Error!void {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        _ = wip.retVoid() catch return error.OutOfMemory;
    }

    /// Finish building a function body
    pub fn endFunction(self: *LlvmEmitter) Error!void {
        if (self.wip_function) |wip| {
            wip.finish() catch return error.OutOfMemory;
            wip.deinit();
            self.allocator.destroy(wip);
            self.wip_function = null;
        }
    }

    /// Create a basic block
    pub fn createBlock(self: *LlvmEmitter, name: []const u8) Error!Builder.Function.Block.Index {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.block(0, name) catch return error.OutOfMemory;
    }

    /// Emit an unconditional branch
    pub fn emitBr(self: *LlvmEmitter, dest: Builder.Function.Block.Index) Error!void {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        _ = wip.br(dest) catch return error.OutOfMemory;
    }

    /// Emit a conditional branch
    pub fn emitCondBr(
        self: *LlvmEmitter,
        cond: Builder.Value,
        then_block: Builder.Function.Block.Index,
        else_block: Builder.Function.Block.Index,
    ) Error!void {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        _ = wip.brCond(cond, then_block, else_block, .none) catch return error.OutOfMemory;
    }

    /// Set the current insertion point to a block
    pub fn positionAtBlock(self: *LlvmEmitter, block: Builder.Function.Block.Index) Error!void {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        wip.cursor = .{ .block = block };
    }

    /// Get the underlying LLVM Builder for direct access
    pub fn getBuilder(self: *LlvmEmitter) *Builder {
        return self.builder;
    }

    /// Get the current WipFunction for advanced operations
    pub fn getWipFunction(self: *LlvmEmitter) ?*Builder.WipFunction {
        return self.wip_function;
    }
};

// Note: Tests are commented out since this file uses the vendored std import
// Tests should be added in a separate test file that's part of the main build
