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

const std = @import("std");
const Allocator = std.mem.Allocator;
const Builder = @import("Builder.zig");
const layout_mod = @import("../../layout/mod.zig");

const Layout = layout_mod.Layout;

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

    /// Compile-time validation that this enum matches the expected structure.
    pub fn validate() void {
        comptime {
            const fields = @typeInfo(NumKind).@"enum".fields;
            if (fields.len != 14) @compileError("NumKind must have exactly 14 variants");
            if (!std.mem.eql(u8, fields[0].name, "u8")) @compileError("NumKind[0] must be u8");
            if (!std.mem.eql(u8, fields[1].name, "i8")) @compileError("NumKind[1] must be i8");
            if (!std.mem.eql(u8, fields[13].name, "numeral")) @compileError("NumKind[13] must be numeral");
        }
    }
};

comptime {
    NumKind.validate();
}

/// Represents Roc's Str type in LLVM
/// Str is a pointer-and-length pair (no capacity, since strings are immutable)
pub const StrLayout = struct {
    ptr: Builder.Type = .ptr,
    len: Builder.Type = .i64,

    /// Returns the LLVM types for pointer and length fields.
    pub fn toFieldTypes(self: StrLayout) [2]Builder.Type {
        return .{ self.ptr, self.len };
    }
};

/// Represents Roc's List type in LLVM
/// List is a pointer, length, and capacity triple
pub const ListLayout = struct {
    ptr: Builder.Type = .ptr,
    len: Builder.Type = .i64,
    capacity: Builder.Type = .i64,

    /// Returns the LLVM types for pointer, length, and capacity fields.
    pub fn toFieldTypes(self: ListLayout) [3]Builder.Type {
        return .{ self.ptr, self.len, self.capacity };
    }
};

/// LLVM IR Emitter for Roc programs
pub const LlvmEmitter = struct {
    /// The LLVM IR builder
    builder: *Builder,

    /// Allocator for temporary allocations
    allocator: Allocator,

    /// Current work-in-progress function being built
    wip_function: ?*Builder.WipFunction,

    /// Current scope for variable bindings (forms a parent chain)
    current_scope: ?*Scope,

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
            .current_scope = null,
        };
    }

    /// Clean up the emitter
    pub fn deinit(self: *LlvmEmitter) void {
        // Clean up any remaining scopes
        while (self.current_scope) |scope| {
            const parent = scope.parent;
            scope.deinit();
            self.allocator.destroy(scope);
            self.current_scope = parent;
        }

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

    // Type Creation Methods

    /// Get the LLVM type for Roc's Bool
    pub fn boolType(_: *LlvmEmitter) Builder.Type {
        return .i1;
    }

    /// Get the LLVM pointer type (opaque pointer in LLVM 15+)
    pub fn ptrType(_: *LlvmEmitter) Builder.Type {
        return .ptr;
    }

    /// Create an LLVM struct type from field types
    /// This is used for Roc Records
    pub fn createRecordType(
        self: *LlvmEmitter,
        field_types: []const Builder.Type,
    ) Error!Builder.Type {
        return self.builder.structType(.normal, field_types) catch return error.OutOfMemory;
    }

    /// Create an LLVM struct type for Roc's Str
    /// Layout: { ptr: *u8, len: u64 }
    pub fn createStrType(self: *LlvmEmitter) Error!Builder.Type {
        const layout = StrLayout{};
        const fields = layout.toFieldTypes();
        return self.builder.structType(.normal, &fields) catch return error.OutOfMemory;
    }

    /// Create an LLVM struct type for Roc's List
    /// Layout: { ptr: *T, len: u64, capacity: u64 }
    pub fn createListType(self: *LlvmEmitter) Error!Builder.Type {
        const layout = ListLayout{};
        const fields = layout.toFieldTypes();
        return self.builder.structType(.normal, &fields) catch return error.OutOfMemory;
    }

    /// Create a tagged union type for Roc tag unions
    /// Layout: { tag: iN, payload: largest_payload_type }
    /// The tag size depends on the number of variants
    pub fn createTagUnionType(
        self: *LlvmEmitter,
        tag_type: Builder.Type,
        payload_type: Builder.Type,
    ) Error!Builder.Type {
        const fields = [_]Builder.Type{ tag_type, payload_type };
        return self.builder.structType(.normal, &fields) catch return error.OutOfMemory;
    }

    /// Get the appropriate integer type for a tag discriminant
    /// Based on the number of variants
    pub fn tagDiscriminantType(_: *LlvmEmitter, num_variants: u64) Builder.Type {
        if (num_variants <= 256) {
            return .i8;
        } else if (num_variants <= 65536) {
            return .i16;
        } else if (num_variants <= 4294967296) {
            return .i32;
        } else {
            return .i64;
        }
    }

    /// Emit an integer constant
    pub fn emitIntConst(self: *LlvmEmitter, llvm_type: Builder.Type, value: i128) Error!Builder.Value {
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
            // In debug builds, verify the function IR before finishing
            if (builtin.mode == .Debug) {
                verifyFunction(wip);
            }

            wip.finish() catch return error.OutOfMemory;
            wip.deinit();
            self.allocator.destroy(wip);
            self.wip_function = null;
        }
    }

    /// Verify that a function's IR is valid.
    /// This is called automatically in debug builds before finishing a function.
    /// Checks:
    /// - All blocks are properly terminated (end with ret, br, switch, or unreachable)
    fn verifyFunction(wip: *Builder.WipFunction) void {
        for (wip.blocks.items, 0..) |block, block_idx| {
            // Each block must have at least one instruction (the terminator)
            if (block.instructions.items.len == 0) {
                std.debug.panic(
                    "LLVM IR verification failed: Block {d} '{s}' has no instructions (missing terminator)",
                    .{ block_idx, wip.builder.getString(block.name).slice(wip.builder) },
                );
            }

            // Get the last instruction in the block
            const last_inst_idx = block.instructions.items[block.instructions.items.len - 1];
            const last_inst = wip.instructions.items[@intFromEnum(last_inst_idx)];

            // Check if it's a terminator instruction
            const is_terminator = switch (last_inst.tag) {
                .ret,
                .@"ret void",
                .br,
                .br_cond,
                .@"switch",
                .indirectbr,
                .@"unreachable",
                => true,
                else => false,
            };

            if (!is_terminator) {
                std.debug.panic(
                    "LLVM IR verification failed: Block {d} '{s}' does not end with a terminator instruction (found {s})",
                    .{ block_idx, wip.builder.getString(block.name).slice(wip.builder), @tagName(last_inst.tag) },
                );
            }
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

    // Struct Operations

    /// Extract a value from a struct/record by field index
    pub fn emitExtractValue(
        self: *LlvmEmitter,
        aggregate: Builder.Value,
        index: u32,
    ) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.extractValue(aggregate, &.{index}, "") catch return error.OutOfMemory;
    }

    /// Insert a value into a struct/record at field index
    pub fn emitInsertValue(
        self: *LlvmEmitter,
        aggregate: Builder.Value,
        element: Builder.Value,
        index: u32,
    ) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.insertValue(aggregate, element, &.{index}, "") catch return error.OutOfMemory;
    }

    /// Get element pointer for struct field access
    pub fn emitStructGep(
        self: *LlvmEmitter,
        struct_type: Builder.Type,
        ptr: Builder.Value,
        field_index: u32,
    ) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.gepStruct(struct_type, ptr, field_index, "") catch return error.OutOfMemory;
    }

    // Comparison Operations

    /// Integer comparison conditions
    pub const IntCond = enum {
        eq, // equal
        ne, // not equal
        ugt, // unsigned greater than
        uge, // unsigned greater or equal
        ult, // unsigned less than
        ule, // unsigned less or equal
        sgt, // signed greater than
        sge, // signed greater or equal
        slt, // signed less than
        sle, // signed less or equal
    };

    /// Emit an integer comparison
    pub fn emitICmp(
        self: *LlvmEmitter,
        cond: IntCond,
        lhs: Builder.Value,
        rhs: Builder.Value,
    ) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        const llvm_cond: Builder.IntegerCondition = switch (cond) {
            .eq => .eq,
            .ne => .ne,
            .ugt => .ugt,
            .uge => .uge,
            .ult => .ult,
            .ule => .ule,
            .sgt => .sgt,
            .sge => .sge,
            .slt => .slt,
            .sle => .sle,
        };
        return wip.icmp(llvm_cond, lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Float comparison conditions
    pub const FloatCond = enum {
        oeq, // ordered equal
        one, // ordered not equal
        ogt, // ordered greater than
        oge, // ordered greater or equal
        olt, // ordered less than
        ole, // ordered less or equal
    };

    /// Emit a floating point comparison
    pub fn emitFCmp(
        self: *LlvmEmitter,
        cond: FloatCond,
        lhs: Builder.Value,
        rhs: Builder.Value,
    ) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        const llvm_cond: Builder.FloatCondition = switch (cond) {
            .oeq => .oeq,
            .one => .one,
            .ogt => .ogt,
            .oge => .oge,
            .olt => .olt,
            .ole => .ole,
        };
        return wip.fcmp(.normal, llvm_cond, lhs, rhs, "") catch return error.OutOfMemory;
    }

    // Memory Operations

    /// Emit an alloca instruction (allocate on stack).
    /// Allocas are always placed in the entry block to ensure proper LLVM semantics
    /// and prevent stack growth when called in loops.
    pub fn emitAlloca(self: *LlvmEmitter, ty: Builder.Type) Error!Builder.Value {
        return self.emitAllocaAligned(ty, .default);
    }

    /// Emit an alloca instruction with explicit alignment.
    /// For i128 on ARM platforms, use 16-byte alignment.
    /// Allocas are always placed in the entry block to ensure proper LLVM semantics.
    pub fn emitAllocaAligned(self: *LlvmEmitter, ty: Builder.Type, alignment: Builder.Alignment) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        // Save current cursor position
        const saved_cursor = wip.cursor;
        // Move to start of entry block (allocas should be at function entry, not in loops)
        wip.cursor = .{ .block = .entry, .instruction = 0 };
        const result = wip.alloca(.normal, ty, .none, alignment, .default, "") catch return error.OutOfMemory;
        // Restore cursor
        wip.cursor = saved_cursor;
        return result;
    }

    /// Get the required alignment for a given LLVM type.
    /// Returns 16-byte alignment for i128 (important for ARM), default otherwise.
    pub fn getTypeAlignment(ty: Builder.Type) Builder.Alignment {
        return switch (ty) {
            .i128 => Builder.Alignment.fromByteUnits(16),
            else => .default,
        };
    }

    /// Emit a load instruction
    pub fn emitLoad(self: *LlvmEmitter, ty: Builder.Type, ptr: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.load(.normal, ty, ptr, "") catch return error.OutOfMemory;
    }

    /// Emit a store instruction
    pub fn emitStore(self: *LlvmEmitter, val: Builder.Value, ptr: Builder.Value) Error!void {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        _ = wip.store(.normal, val, ptr, "") catch return error.OutOfMemory;
    }

    // Call Operations

    /// Emit a function call with default C calling convention and no attributes
    pub fn emitCall(
        self: *LlvmEmitter,
        fn_type: Builder.Type,
        callee: Builder.Value,
        args: []const Builder.Value,
    ) Error!Builder.Value {
        return self.emitCallWithConv(fn_type, callee, args, .ccc, .none);
    }

    /// Emit a function call with explicit calling convention and attributes
    pub fn emitCallWithConv(
        self: *LlvmEmitter,
        fn_type: Builder.Type,
        callee: Builder.Value,
        args: []const Builder.Value,
        call_conv: Builder.CallConv,
        function_attributes: Builder.FunctionAttributes,
    ) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.call(.normal, call_conv, function_attributes, fn_type, callee, args, "") catch return error.OutOfMemory;
    }

    // PHI Node Operations

    /// Emit a phi node for merging values from different branches
    pub fn emitPhi(
        self: *LlvmEmitter,
        ty: Builder.Type,
    ) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.phi(ty, "") catch return error.OutOfMemory;
    }

    // Boolean Operations

    /// Emit a boolean constant
    pub fn emitBoolConst(self: *LlvmEmitter, value: bool) Error!Builder.Value {
        const constant = self.builder.intConst(.i1, if (value) 1 else 0) catch return error.OutOfMemory;
        return constant.toValue();
    }

    /// Emit boolean AND
    pub fn emitAnd(self: *LlvmEmitter, lhs: Builder.Value, rhs: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.bin(.@"and", lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Emit boolean OR
    pub fn emitOr(self: *LlvmEmitter, lhs: Builder.Value, rhs: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.bin(.@"or", lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Emit boolean XOR (also used for NOT when xor with true)
    pub fn emitXor(self: *LlvmEmitter, lhs: Builder.Value, rhs: Builder.Value) Error!Builder.Value {
        const wip = self.wip_function orelse return error.NoActiveFunction;
        return wip.bin(.xor, lhs, rhs, "") catch return error.OutOfMemory;
    }

    /// Emit boolean NOT (xor with 1)
    pub fn emitNot(self: *LlvmEmitter, val: Builder.Value) Error!Builder.Value {
        const true_val = try self.emitBoolConst(true);
        return self.emitXor(val, true_val);
    }

    // Builder Access

    /// Get the underlying LLVM Builder for direct access
    pub fn getBuilder(self: *LlvmEmitter) *Builder {
        return self.builder;
    }

    /// Get the current WipFunction for advanced operations
    pub fn getWipFunction(self: *LlvmEmitter) ?*Builder.WipFunction {
        return self.wip_function;
    }

    // Scope Management

    /// Push a new scope onto the scope stack
    pub fn pushScope(self: *LlvmEmitter) Error!void {
        const new_scope = self.allocator.create(Scope) catch return error.OutOfMemory;
        new_scope.* = Scope.init(self.allocator, self.current_scope);
        self.current_scope = new_scope;
    }

    /// Pop the current scope, returning to parent scope
    /// Note: Does NOT emit decrefs - caller must handle cleanup if needed
    pub fn popScope(self: *LlvmEmitter) void {
        if (self.current_scope) |scope| {
            const parent = scope.parent;
            scope.deinit();
            self.allocator.destroy(scope);
            self.current_scope = parent;
        }
    }

    /// Define a variable in the current scope
    pub fn defineVar(self: *LlvmEmitter, name: []const u8, value: ScopedValue) Error!void {
        if (self.current_scope) |scope| {
            scope.define(name, value) catch return error.OutOfMemory;
        }
    }

    /// Look up a variable by name, searching from current scope up to parents
    pub fn lookupVar(self: *LlvmEmitter, name: []const u8) ?ScopedValue {
        var scope_opt = self.current_scope;
        while (scope_opt) |scope| {
            if (scope.lookup(name)) |value| {
                return value;
            }
            scope_opt = scope.parent;
        }
        return null;
    }

    /// Get all values in current scope that need decref on exit
    pub fn getDecrefValues(self: *LlvmEmitter) []const ScopedValue {
        if (self.current_scope) |scope| {
            return scope.getDecrefValues();
        }
        return &.{};
    }
};

/// A value bound in a scope, with metadata for cleanup
pub const ScopedValue = struct {
    /// The LLVM value (could be a direct value or a pointer to stack slot)
    value: Builder.Value,

    /// The LLVM type of the value
    llvm_type: Builder.Type,

    /// The Roc layout of the value (needed for proper refcount cleanup)
    layout: ?Layout,

    /// Whether this value is stored on the stack (and value is a pointer to it)
    is_ptr: bool,

    /// Whether this value needs decref when the scope exits
    needs_decref: bool,

    /// Create a simple value that doesn't need decref (scalars, etc.)
    pub fn simple(value: Builder.Value, llvm_type: Builder.Type) ScopedValue {
        return .{
            .value = value,
            .llvm_type = llvm_type,
            .layout = null,
            .is_ptr = false,
            .needs_decref = false,
        };
    }

    /// Create a refcounted value that needs decref on scope exit
    pub fn refcounted(value: Builder.Value, llvm_type: Builder.Type, layout: Layout, is_ptr: bool) ScopedValue {
        return .{
            .value = value,
            .llvm_type = llvm_type,
            .layout = layout,
            .is_ptr = is_ptr,
            .needs_decref = true,
        };
    }
};

/// Scope for tracking variable bindings and cleanup
/// Scopes form a parent chain for lexical scoping
pub const Scope = struct {
    /// Variable bindings in this scope (name → value)
    bindings: std.StringHashMapUnmanaged(ScopedValue),

    /// Values that need decref when this scope exits
    decref_on_exit: std.ArrayListUnmanaged(ScopedValue),

    /// Parent scope (for lexical lookup)
    parent: ?*Scope,

    /// Allocator for this scope's data
    allocator: Allocator,

    /// Initialize a new scope
    pub fn init(allocator: Allocator, parent: ?*Scope) Scope {
        return .{
            .bindings = .{},
            .decref_on_exit = .{},
            .parent = parent,
            .allocator = allocator,
        };
    }

    /// Clean up scope resources
    pub fn deinit(self: *Scope) void {
        // Free the string keys (they were duped when inserted)
        var key_iter = self.bindings.keyIterator();
        while (key_iter.next()) |key_ptr| {
            self.allocator.free(key_ptr.*);
        }
        self.bindings.deinit(self.allocator);
        self.decref_on_exit.deinit(self.allocator);
    }

    /// Define a variable in this scope
    pub fn define(self: *Scope, name: []const u8, value: ScopedValue) Allocator.Error!void {
        // Dupe the name so we own it
        const owned_name = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(owned_name);

        try self.bindings.put(self.allocator, owned_name, value);

        // Track for decref if needed
        if (value.needs_decref) {
            try self.decref_on_exit.append(self.allocator, value);
        }
    }

    /// Look up a variable in this scope only (not parents)
    pub fn lookup(self: *const Scope, name: []const u8) ?ScopedValue {
        return self.bindings.get(name);
    }

    /// Get values that need decref when this scope exits
    pub fn getDecrefValues(self: *const Scope) []const ScopedValue {
        return self.decref_on_exit.items;
    }
};

// Note: Tests are commented out since this file uses the vendored std import
// Tests should be added in a separate test file that's part of the main build
