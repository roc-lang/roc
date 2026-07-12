//! Single source of truth for the compiler's Zig-backed builtin functions.
//!
//! Every builtin that generated code reaches through a `roc_builtins_*`
//! linker symbol is registered here exactly once, as a `BuiltinFn` member
//! whose name is the symbol name minus the shared prefix. The wrapper
//! function in `dev_wrappers.zig` with that exact symbol name IS the
//! builtin's ABI: its Zig parameter and return types define what crosses
//! the call boundary.
//!
//! Everything downstream is derived from this enum at comptime:
//! - the `@export` lists in `static_lib.zig` / `static_lib_core.zig`,
//! - the dev-backend JIT and shim symbol resolution tables,
//! - the wasm backend's call signatures (`backend/wasm/builtin_signatures.zig`),
//! - the LLVM backend's call-site symbol names,
//! - the linker allowlists that pick the core vs. full builtins payload.
//!
//! Adding a builtin therefore touches the wrapper in `dev_wrappers.zig`, one
//! enum member here, and (if user binaries must link it) the membership
//! predicates below. Forgetting any of those is a compile error, enforced by
//! the comptime block at the bottom of this file: members and wrappers must
//! match one-to-one.

const std = @import("std");
const dev_wrappers = @import("dev_wrappers.zig");
const num = @import("num.zig");

/// Linker-symbol prefix shared by every registered builtin.
pub const symbol_prefix = "roc_builtins_";

/// One member per Zig-backed builtin, named after its linker symbol
/// (member `str_concat` is symbol `roc_builtins_str_concat`).
/// Ordered to match the wrapper definitions in `dev_wrappers.zig`.
pub const BuiltinFn = enum {
    // Hashing
    hasher_write_u64,
    hasher_write_u128,
    hasher_write_f32_bits,
    hasher_write_f64_bits,
    hasher_write_bytes,
    hasher_write_str,
    hasher_finish,
    dict_pseudo_seed,

    // Crypto
    crypto_sha256_hash_bytes,
    crypto_sha256_hasher_empty,
    crypto_sha256_hasher_write,
    crypto_sha256_hasher_finish,
    crypto_blake3_hash_bytes,
    crypto_blake3_hasher_empty,
    crypto_blake3_hasher_write,
    crypto_blake3_hasher_finish,

    // Strings
    str_to_utf8,
    str_concat,
    str_contains,
    str_starts_with,
    str_ends_with,
    str_equal,
    str_equal_static_small,
    str_static_small_word_eq,
    str_static_small_word_caseless_eq,
    str_count_utf8_bytes,
    str_find_first,
    str_drop_prefix_caseless_ascii,
    str_caseless_ascii_equals,
    str_repeat,
    str_trim,
    str_trim_start,
    str_trim_end,
    str_split,
    str_join_with,
    str_reserve,
    str_release_excess_capacity,
    str_with_capacity,
    str_drop_prefix,
    str_drop_suffix,
    str_with_ascii_lowercased,
    str_with_ascii_uppercased,
    str_from_utf8_lossy,
    str_from_utf8,
    str_from_utf8_result,
    str_from_utf8_parts,
    str_escape_and_quote,

    // Debug / expect / crash
    roc_dbg,
    dbg_str,
    expect_err_str,
    roc_expect_failed,
    roc_crashed,

    // Lists
    list_with_capacity,
    list_append_unsafe,
    list_map_can_reuse,
    list_concat,
    list_prepend,
    list_sublist,
    list_drop_at,
    list_replace,
    list_swap,
    list_reserve,
    list_release_excess_capacity,
    list_incref,
    list_incref_single_thread,
    list_decref_str,
    list_decref_flat_list,
    list_decref_with,
    list_decref_with_single_thread,
    list_free_flat_list,
    list_free_with,

    // Boxes and erased callables
    box_decref_with,
    box_decref_with_single_thread,
    box_free_with,
    erased_callable_incref,
    erased_callable_decref,
    erased_callable_decref_single_thread,
    erased_callable_free,

    // Hot reload (in-process dev evaluator only)
    hot_reload_enter,
    hot_reload_leave,
    hot_reload_retain_current,
    hot_reload_erased_callable_drop,

    // Memory / refcounting
    allocate_with_refcount,
    incref_data_ptr,
    incref_data_ptr_single_thread,
    decref_data_ptr,
    decref_data_ptr_single_thread,
    free_data_ptr,

    // Literals
    str_from_literal,

    // Numeric conversions
    dec_to_str,
    dec_to_i64_trunc,
    i64_to_dec,
    u64_to_dec,
    dec_to_f64,
    i128_to_f64,
    u128_to_f64,
    f64_to_i128_trunc,
    f64_to_u128_trunc,
    i128_try_convert,
    u128_try_convert,
    int_try_signed,
    int_try_unsigned,
    dec_to_int_try_unsafe,
    f64_to_int_try_unsafe,
    dec_to_f32_try_unsafe,
    f64_to_f32_try_unsafe,
    i128_to_dec_try_unsafe,
    u128_to_dec_try_unsafe,

    // Dec arithmetic
    dec_mul,
    dec_mul_saturated,
    dec_div,
    dec_div_trunc,
    dec_pow,
    dec_sqrt,
    dec_sin,
    dec_cos,
    dec_tan,
    dec_asin,
    dec_acos,
    dec_atan,

    // 128-bit integer helpers
    num_mul_with_overflow_u128,
    num_mul_with_overflow_i128,
    num_div_trunc_u128,
    num_div_trunc_i128,
    num_rem_trunc_u128,
    num_rem_trunc_i128,
    num_mod_i128,
    num_shl_u128,
    num_shr_i128,
    num_shr_u128,

    // Number formatting / parsing and float math
    int_to_str,
    float_to_str,
    float_floor,
    float_ceiling,
    float_pow,
    float_sin,
    float_cos,
    float_tan,
    float_asin,
    float_acos,
    float_atan,
    int_from_str,
    dec_from_str,
    float_from_str,

    // List equality / reverse
    list_eq,
    list_str_eq,
    list_list_eq,
    list_reverse,

    // Integer modulo
    i32_mod_by,
    i8_mod_by,
    u8_mod_by,
    i16_mod_by,
    u16_mod_by,
    u32_mod_by,
    i64_mod_by,
    u64_mod_by,

    /// The linker symbol this builtin is reached through.
    pub fn symbolName(self: BuiltinFn) [:0]const u8 {
        switch (self) {
            inline else => |tag| return comptime symbol_prefix ++ @tagName(tag),
        }
    }

    /// The type of this builtin's wrapper function; its parameter and
    /// return types are the builtin's call ABI.
    pub fn WrapperType(comptime self: BuiltinFn) type {
        return @TypeOf(@field(dev_wrappers, self.symbolName()));
    }

    /// The wrapper function implementing this builtin.
    pub fn wrapper(comptime self: BuiltinFn) *const WrapperType(self) {
        return &@field(dev_wrappers, self.symbolName());
    }

    /// In-process address of this builtin's wrapper, for JIT/shim symbol
    /// resolution. Runtime-callable.
    pub fn wrapperAddr(self: BuiltinFn) usize {
        switch (self) {
            inline else => |tag| return @intFromPtr(tag.wrapper()),
        }
    }

    /// Find a registered builtin by its full linker-symbol name.
    pub fn byName(name: []const u8) ?BuiltinFn {
        return name_map.get(name);
    }

    /// Whether the full static-lib payload (`static_lib.zig`) exports this
    /// builtin for user binaries to link against.
    ///
    /// The excluded members are reachable only through the in-process
    /// JIT/native paths today (hot reload and dbg hooks are compiler
    /// internals; the float floor/ceil and 128-bit shift wrappers are only
    /// resolved in-process). If a backend starts emitting object-file
    /// relocations to one of them, move it into the exported set.
    pub fn inFullStaticLib(self: BuiltinFn) bool {
        return switch (self) {
            .float_ceiling,
            .float_floor,
            .hot_reload_enter,
            .hot_reload_erased_callable_drop,
            .hot_reload_leave,
            .hot_reload_retain_current,
            .num_shl_u128,
            .num_shr_i128,
            .num_shr_u128,
            .roc_dbg,
            => false,
            else => true,
        };
    }

    /// Whether the core static-lib payload (`static_lib_core.zig`) exports
    /// this builtin. The core payload is the smaller bitcode linked when an
    /// app only needs common string/list/refcount/debug operations; keep it
    /// minimal.
    pub fn inCoreStaticLib(self: BuiltinFn) bool {
        return switch (self) {
            .str_to_utf8,
            .str_concat,
            .str_contains,
            .str_starts_with,
            .str_ends_with,
            .str_equal,
            .str_equal_static_small,
            .str_static_small_word_eq,
            .str_static_small_word_caseless_eq,
            .str_count_utf8_bytes,
            .str_find_first,
            .str_drop_prefix_caseless_ascii,
            .str_caseless_ascii_equals,
            .str_repeat,
            .str_trim,
            .str_trim_start,
            .str_trim_end,
            .str_split,
            .str_join_with,
            .str_reserve,
            .str_release_excess_capacity,
            .str_with_capacity,
            .str_drop_prefix,
            .str_drop_suffix,
            .str_with_ascii_lowercased,
            .str_with_ascii_uppercased,
            .str_from_utf8_lossy,
            .str_from_utf8,
            .str_from_utf8_result,
            .str_from_utf8_parts,
            .str_escape_and_quote,
            .dbg_str,
            .expect_err_str,
            .roc_expect_failed,
            .roc_crashed,
            .list_with_capacity,
            .list_append_unsafe,
            .list_concat,
            .list_prepend,
            .list_sublist,
            .list_drop_at,
            .list_replace,
            .list_swap,
            .list_reserve,
            .list_release_excess_capacity,
            .list_incref,
            .list_incref_single_thread,
            .list_decref_str,
            .list_decref_flat_list,
            .list_decref_with,
            .list_decref_with_single_thread,
            .list_free_flat_list,
            .list_free_with,
            .box_decref_with,
            .box_decref_with_single_thread,
            .box_free_with,
            .erased_callable_incref,
            .erased_callable_decref,
            .erased_callable_decref_single_thread,
            .erased_callable_free,
            .allocate_with_refcount,
            .incref_data_ptr,
            .incref_data_ptr_single_thread,
            .decref_data_ptr,
            .decref_data_ptr_single_thread,
            .free_data_ptr,
            .str_from_literal,
            .num_mul_with_overflow_u128,
            .num_mul_with_overflow_i128,
            .int_to_str,
            .int_from_str,
            .list_eq,
            .list_str_eq,
            .list_list_eq,
            .list_reverse,
            .i32_mod_by,
            .i8_mod_by,
            .u8_mod_by,
            .i16_mod_by,
            .u16_mod_by,
            .u32_mod_by,
            .i64_mod_by,
            .u64_mod_by,
            => true,
            else => false,
        };
    }
};

const name_map = blk: {
    @setEvalBranchQuota(200_000);
    const members = std.enums.values(BuiltinFn);
    var kvs: [members.len]struct { []const u8, BuiltinFn } = undefined;
    for (members, 0..) |b, i| kvs[i] = .{ b.symbolName(), b };
    break :blk std.StaticStringMap(BuiltinFn).initComptime(kvs);
};

/// Integer types whose mul-with-overflow helper is exported under the
/// `roc__num_mul_with_overflow_` scheme by both static-lib payloads.
pub const overflow_mul_types = [_]type{ i64, i32, i16, i8 };
/// Types whose add-with-overflow helper is exported (`roc__num_add_with_overflow_`).
pub const overflow_add_types = [_]type{i128};
/// Types whose sub-with-overflow helper is exported (`roc__num_sub_with_overflow_`).
pub const overflow_sub_types = [_]type{i128};

/// Export the wide-integer overflow helpers that accompany the builtin
/// wrappers in every static-lib payload. Call from a comptime block.
pub fn exportNumOverflowHelpers() void {
    inline for (overflow_mul_types) |T| num.exportMulWithOverflow(T, "roc__num_mul_with_overflow_");
    inline for (overflow_add_types) |T| num.exportAddWithOverflow(T, "roc__num_add_with_overflow_");
    inline for (overflow_sub_types) |T| num.exportSubWithOverflow(T, "roc__num_sub_with_overflow_");
}

/// Every symbol the core static-lib payload exports: the core builtin
/// wrappers plus the overflow helpers. Linkers consult this to decide
/// whether the core payload can satisfy all builtin roots of an app.
const core_link_roots = blk: {
    @setEvalBranchQuota(200_000);
    var count = overflow_mul_types.len + overflow_add_types.len + overflow_sub_types.len;
    for (std.enums.values(BuiltinFn)) |b| {
        if (b.inCoreStaticLib()) count += 1;
    }
    var kvs: [count]struct { []const u8, void } = undefined;
    var i: usize = 0;
    for (std.enums.values(BuiltinFn)) |b| {
        if (b.inCoreStaticLib()) {
            kvs[i] = .{ b.symbolName(), {} };
            i += 1;
        }
    }
    for (overflow_mul_types) |T| {
        kvs[i] = .{ "roc__num_mul_with_overflow_" ++ @typeName(T), {} };
        i += 1;
    }
    for (overflow_add_types) |T| {
        kvs[i] = .{ "roc__num_add_with_overflow_" ++ @typeName(T), {} };
        i += 1;
    }
    for (overflow_sub_types) |T| {
        kvs[i] = .{ "roc__num_sub_with_overflow_" ++ @typeName(T), {} };
        i += 1;
    }
    break :blk std.StaticStringMap(void).initComptime(kvs);
};

/// Whether a linker root symbol is satisfied by the core static-lib payload.
pub fn isCoreLinkRoot(name: []const u8) bool {
    return core_link_roots.has(name);
}

comptime {
    @setEvalBranchQuota(100_000);

    // Every registry member must have a wrapper whose name is exactly its
    // symbol name. The wrapper defines the builtin's ABI, so a missing or
    // misnamed wrapper must fail here rather than at link or run time.
    for (@typeInfo(BuiltinFn).@"enum".fields) |field| {
        const symbol = symbol_prefix ++ field.name;
        if (!@hasDecl(dev_wrappers, symbol)) {
            @compileError("BuiltinFn." ++ field.name ++ " has no wrapper: dev_wrappers.zig must define `pub fn " ++ symbol ++ "`");
        }
        if (@typeInfo(@TypeOf(@field(dev_wrappers, symbol))) != .@"fn") {
            @compileError("dev_wrappers." ++ symbol ++ " must be a function to serve as a builtin wrapper");
        }
    }

    // And the reverse: dev_wrappers cannot grow a roc_builtins_* function
    // the registry does not know about.
    for (@typeInfo(dev_wrappers).@"struct".decls) |decl| {
        if (std.mem.startsWith(u8, decl.name, symbol_prefix)) {
            if (!@hasField(BuiltinFn, decl.name[symbol_prefix.len..])) {
                @compileError("dev_wrappers." ++ decl.name ++ " is not registered in builtin_registry.BuiltinFn");
            }
        }
    }
}

test "every builtin resolves by name to its wrapper address" {
    inline for (comptime std.enums.values(BuiltinFn)) |b| {
        const found = BuiltinFn.byName(b.symbolName()) orelse return error.TestUnexpectedResult;
        try std.testing.expectEqual(b, found);
        try std.testing.expect(b.wrapperAddr() != 0);
    }
    try std.testing.expectEqual(@as(?BuiltinFn, null), BuiltinFn.byName("roc_builtins_nonexistent"));
}
