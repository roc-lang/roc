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
    // Memory allocation (from utils.zig)
    pub const allocate_with_refcount = "roc_builtins_utils_allocate_with_refcount";

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
    /// ABI representation type for i128 values
    pub const AbiType = enum {
        /// i128 is passed natively
        native,
        /// Windows: i128 is passed as <2 x i64> vector
        windows_vector,
        /// macOS ARM64: i128 is passed as [2 x i64] array
        macos_arm64_array,
    };

    /// Determine the ABI type for the current target
    pub const abi_type: AbiType = switch (builtin.os.tag) {
        .windows => .windows_vector,
        .macos => if (builtin.cpu.arch == .aarch64) .macos_arm64_array else .native,
        else => .native,
    };

    /// On Windows, i128 is passed as a <2 x i64> vector
    /// On macOS ARM64, i128 is passed as a [2 x i64] array
    /// On other platforms, i128 is passed natively
    pub const needs_conversion = abi_type != .native;

    /// Get the LLVM type to use for i128 in function signatures
    pub fn paramType(builder: *Builder) Error!Builder.Type {
        return switch (abi_type) {
            .native => .i128,
            .windows_vector => builder.vectorType(.normal, 2, .i64) catch return error.OutOfMemory,
            .macos_arm64_array => builder.arrayType(2, .i64) catch return error.OutOfMemory,
        };
    }

    /// Get the LLVM type for i128 return values
    pub fn returnType(builder: *Builder) Error!Builder.Type {
        return paramType(builder);
    }

    /// Convert an i128 value to the platform-specific ABI representation.
    /// On Windows: bitcast to <2 x i64>
    /// On macOS ARM64: bitcast to [2 x i64]
    /// On other platforms: return as-is
    pub fn toAbiType(emitter: *emit.LlvmEmitter, value: Builder.Value) Error!Builder.Value {
        if (!needs_conversion) {
            return value;
        }

        // Get the platform-specific type
        const target_type = try paramType(emitter.builder);

        // Bitcast i128 to the target type
        const wip = emitter.wip_function orelse return error.OutOfMemory;
        return wip.cast(.bitcast, value, target_type, "") catch return error.OutOfMemory;
    }

    /// Convert a platform-specific ABI value back to i128.
    /// On Windows: bitcast from <2 x i64> to i128
    /// On macOS ARM64: bitcast from [2 x i64] to i128
    /// On other platforms: return as-is
    pub fn fromAbiType(emitter: *emit.LlvmEmitter, value: Builder.Value) Error!Builder.Value {
        if (!needs_conversion) {
            return value;
        }

        // Bitcast back to i128
        const wip = emitter.wip_function orelse return error.OutOfMemory;
        return wip.cast(.bitcast, value, .i128, "") catch return error.OutOfMemory;
    }

    /// Call a builtin function that returns i128, handling ABI conversion.
    /// This declares the function with the platform-specific return type and
    /// converts the result back to i128.
    pub fn callI128Builtin(
        ctx: *BuiltinContext,
        emitter: *emit.LlvmEmitter,
        name: []const u8,
        param_types: []const Builder.Type,
        args: []const Builder.Value,
    ) Error!Builder.Value {
        // Get the ABI return type
        const ret_type = try returnType(emitter.builder);

        // Call the builtin with the ABI return type
        const result = try emitBuiltinCall(ctx, emitter, name, ret_type, param_types, args);

        // Convert back to i128 if necessary
        return fromAbiType(emitter, result);
    }
};

/// Helper to emit a call to a builtin function.
/// Handles the common pattern of:
/// 1. Ensure the function is declared
/// 2. Emit the call instruction with C calling convention
/// 3. Return the result value
///
/// Builtins are implemented in Zig/C, so they use the C calling convention (ccc).
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

    // Emit the call with C calling convention (builtins are C-compatible)
    // Note: We use FunctionAttributes.none for now. If specific builtins need
    // attributes like sret or byval, those should be added here or via a
    // more sophisticated attribute propagation system.
    return emitter.emitCallWithConv(
        declared.fn_type,
        fn_value,
        args,
        .ccc, // C calling convention for builtins
        .none, // No special attributes
    ) catch return error.OutOfMemory;
}

// High-level helpers for common builtin patterns

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

/// Emit a heap allocation with refcount.
/// This allocates memory with space for a reference count before the data.
/// Returns a pointer to the data portion (not the refcount).
///
/// data_bytes: Size of the data to allocate (not including refcount)
/// alignment: Alignment requirement for the data
/// elements_refcounted: Whether elements are refcounted (for nested data)
/// roc_ops: Pointer to RocOps struct for allocation
pub fn emitAllocateWithRefcount(
    ctx: *BuiltinContext,
    emitter: *emit.LlvmEmitter,
    data_bytes: Builder.Value,
    alignment: Builder.Value,
    elements_refcounted: Builder.Value,
    roc_ops: Builder.Value,
) Error!Builder.Value {
    // allocate_with_refcount(data_bytes: usize, alignment: u32, elements_refcounted: bool, roc_ops: *RocOps) -> *u8
    return try emitBuiltinCall(
        ctx,
        emitter,
        BuiltinNames.allocate_with_refcount,
        .ptr, // Returns pointer to allocated data
        &.{ .i64, .i32, .i1, .ptr }, // data_bytes, alignment, elements_refcounted, roc_ops
        &.{ data_bytes, alignment, elements_refcounted, roc_ops },
    );
}
