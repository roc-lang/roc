//! LLVM Builtin Function Call Integration
//!
//! This module provides infrastructure for calling Roc builtin functions from
//! generated LLVM code. Builtins handle operations like:
//! - Reference counting (incref, decref)
//! - List operations (get, set, append, etc.)
//! - String operations (concat, substring, etc.)
//! - Dec (decimal) arithmetic
//!
//! ## Implementation Strategy
//!
//! The Zig builtins in src/builtins/ are compiled into the same binary as the
//! compiler. For LLVM code generation, we have two paths:
//!
//! 1. **JIT execution (REPL)**: The builtins are already in the process's
//!    address space. We can look them up via `LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess`.
//!
//! 2. **Ahead-of-time compilation**: We need to either:
//!    a. Link against the builtins as a static library
//!    b. Embed builtins bitcode and merge into the module (TODO)
//!
//! For now, this module focuses on the JIT path since that's what the REPL uses.

const std = @import("../../std.zig");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const Builder = @import("Builder.zig");
const emit = @import("emit.zig");

/// Builtin function categories
pub const BuiltinCategory = enum {
    list,
    str,
    num,
    dec,
    utils,
};

/// Common builtin function names used in code generation.
/// These match the exported function names from src/builtins/*.zig
pub const BuiltinNames = struct {
    // Reference counting (from utils.zig)
    pub const incref_rc_ptr = "roc_builtins_utils_incref_rc_ptr_c";
    pub const decref_rc_ptr = "roc_builtins_utils_decref_rc_ptr_c";

    // List operations (from list.zig)
    pub const list_len = "roc_builtins_list_len";
    pub const list_get_unsafe = "roc_builtins_list_get_unsafe";
    pub const list_replace_unsafe = "roc_builtins_list_replace_unsafe";
    pub const list_append = "roc_builtins_list_append";
    pub const list_concat = "roc_builtins_list_concat";
    pub const list_drop_first = "roc_builtins_list_drop_first";
    pub const list_drop_last = "roc_builtins_list_drop_last";
    pub const list_sublist = "roc_builtins_list_sublist";

    // String operations (from str.zig)
    pub const str_concat = "roc_builtins_str_concat";
    pub const str_count_graphemes = "roc_builtins_str_count_graphemes";
    pub const str_starts_with = "roc_builtins_str_starts_with";
    pub const str_ends_with = "roc_builtins_str_ends_with";
    pub const str_split = "roc_builtins_str_split";
    pub const str_to_num = "roc_builtins_str_to_num";

    // Dec operations (from dec.zig)
    pub const dec_add = "roc_builtins_dec_add_with_overflow";
    pub const dec_sub = "roc_builtins_dec_sub_with_overflow";
    pub const dec_mul = "roc_builtins_dec_mul_with_overflow";
    pub const dec_div = "roc_builtins_dec_div";
    pub const dec_from_str = "roc_builtins_dec_from_str";
    pub const dec_to_str = "roc_builtins_dec_to_str";

    // Numeric operations (from num.zig)
    pub const num_pow_int = "roc_builtins_num_pow_int";
    pub const num_log = "roc_builtins_num_log";
    pub const num_sin = "roc_builtins_num_sin";
    pub const num_cos = "roc_builtins_num_cos";
    pub const num_tan = "roc_builtins_num_tan";
};

/// Error types for builtin operations
pub const Error = error{
    OutOfMemory,
    BuiltinNotFound,
    TypeMismatch,
};

/// Context for builtin function calls during code generation.
/// This tracks function declarations so we don't re-declare the same function.
pub const BuiltinContext = struct {
    allocator: Allocator,
    /// Cache of declared builtin function types
    declared_functions: std.StringHashMap(DeclaredBuiltin),

    const DeclaredBuiltin = struct {
        fn_type: Builder.Type,
        fn_index: Builder.Function.Index,
    };

    pub fn init(allocator: Allocator) BuiltinContext {
        return .{
            .allocator = allocator,
            .declared_functions = std.StringHashMap(DeclaredBuiltin).init(allocator),
        };
    }

    pub fn deinit(self: *BuiltinContext) void {
        self.declared_functions.deinit();
    }

    /// Declare a builtin function if not already declared.
    /// Returns the function index for calling.
    pub fn declareBuiltin(
        self: *BuiltinContext,
        emitter: *emit.LlvmEmitter,
        name: []const u8,
        return_type: Builder.Type,
        param_types: []const Builder.Type,
    ) Error!Builder.Function.Index {
        // Check cache first
        if (self.declared_functions.get(name)) |declared| {
            return declared.fn_index;
        }

        // Create function type and declare it
        const fn_type = emitter.createFunctionType(return_type, param_types) catch return error.OutOfMemory;
        const fn_index = emitter.addFunction(name, fn_type) catch return error.OutOfMemory;

        // Cache it
        const owned_name = self.allocator.dupe(u8, name) catch return error.OutOfMemory;
        self.declared_functions.put(owned_name, .{
            .fn_type = fn_type,
            .fn_index = fn_index,
        }) catch {
            self.allocator.free(owned_name);
            return error.OutOfMemory;
        };

        return fn_index;
    }
};

/// Platform-specific i128 handling for builtin calls.
/// Different platforms use different representations for i128 in the C ABI.
pub const I128Abi = struct {
    /// On Windows, i128 is passed as a <2 x i64> vector
    /// On macOS ARM64, i128 is passed as a [2 x i64] array
    /// On other platforms, i128 is passed natively
    pub const needs_conversion = switch (builtin.os.tag) {
        .windows => true,
        .macos => builtin.cpu.arch == .aarch64,
        else => false,
    };

    /// Get the LLVM type to use for i128 in function signatures
    pub fn paramType(builder: *Builder) Error!Builder.Type {
        if (needs_conversion) {
            // Use [2 x i64] representation
            return builder.arrayType(2, .i64) catch return error.OutOfMemory;
        } else {
            return .i128;
        }
    }

    /// Get the LLVM type for i128 return values
    pub fn returnType(builder: *Builder) Error!Builder.Type {
        return paramType(builder);
    }
};

/// Helper to emit a call to a builtin function.
/// Handles the common pattern of:
/// 1. Ensure the function is declared
/// 2. Emit the call instruction
/// 3. Return the result value
pub fn emitBuiltinCall(
    ctx: *BuiltinContext,
    emitter: *emit.LlvmEmitter,
    name: []const u8,
    return_type: Builder.Type,
    param_types: []const Builder.Type,
    args: []const Builder.Value,
) Error!Builder.Value {
    // Ensure function is declared
    const fn_index = try ctx.declareBuiltin(emitter, name, return_type, param_types);

    // Get the function value for calling (Function.Index -> Value)
    const fn_value = fn_index.toValue(emitter.builder);

    // Get the function type
    const declared = ctx.declared_functions.get(name).?;

    // Emit the call
    return emitter.emitCall(declared.fn_type, fn_value, args) catch return error.OutOfMemory;
}

// ============================================================
// High-level helpers for common builtin patterns
// ============================================================

/// Emit an incref call for a refcounted value.
/// ptr: Pointer to the data (refcount is at ptr - sizeof(isize))
/// amount: Number to increment by (usually 1)
pub fn emitIncref(
    ctx: *BuiltinContext,
    emitter: *emit.LlvmEmitter,
    ptr: Builder.Value,
    amount: Builder.Value,
) Error!void {
    // incref is void-returning
    _ = try emitBuiltinCall(
        ctx,
        emitter,
        BuiltinNames.incref_rc_ptr,
        .void,
        &.{ .ptr, .i64 }, // ptr, amount
        &.{ ptr, amount },
    );
}

/// Emit a decref call for a refcounted value.
/// ptr: Pointer to the data (refcount is at ptr - sizeof(isize))
/// alignment: Alignment of the data
/// elements_refcounted: Whether elements need recursive decref
pub fn emitDecref(
    ctx: *BuiltinContext,
    emitter: *emit.LlvmEmitter,
    ptr: Builder.Value,
    alignment: Builder.Value,
    elements_refcounted: Builder.Value,
) Error!void {
    _ = try emitBuiltinCall(
        ctx,
        emitter,
        BuiltinNames.decref_rc_ptr,
        .void,
        &.{ .ptr, .i64, .i1 }, // ptr, alignment, elements_refcounted
        &.{ ptr, alignment, elements_refcounted },
    );
}
