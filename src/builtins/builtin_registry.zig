//! Single source of truth for the compiler's Zig-backed builtin wrappers.
//!
//! Every `roc_builtins_*` linker symbol the compiler can emit or resolve is a
//! member of `BuiltinFn`, and the member name IS the symbol suffix: the
//! linker symbol, the wrapper function reference, and payload membership are
//! all derived from the member at comptime. Registering a builtin means
//! adding its wrapper to `dev_wrappers.zig` and its member here; the export
//! lists, the dev JIT symbol table, the wasm signature table, and the LLVM
//! payload allowlists are generated from this enum, so forgetting any of
//! those formerly-manual steps is a compile error.
//!
//! Bidirectional comptime checks pin the enum to the `pub fn roc_builtins_*`
//! definitions in `dev_wrappers.zig`: a member without a wrapper or a
//! wrapper without a member fails the build.

const std = @import("std");
const dev_wrappers = @import("dev_wrappers.zig");
const num = @import("num.zig");

/// Prefix shared by every builtin wrapper linker symbol.
pub const symbol_prefix = "roc_builtins_";

/// One member per builtin wrapper, in `dev_wrappers.zig` definition order.
pub const BuiltinFn = enum {
    hasher_write_u64,
    hasher_write_u128,
    hasher_write_f32_bits,
    hasher_write_f64_bits,
    hasher_write_bytes,
    hasher_write_str,
    hasher_finish,
    dict_pseudo_seed,
    crypto_sha256_hash_bytes,
    crypto_sha256_hasher_empty,
    crypto_sha256_hasher_write,
    crypto_sha256_hasher_finish,
    crypto_blake3_hash_bytes,
    crypto_blake3_hasher_empty,
    crypto_blake3_hasher_write,
    crypto_blake3_hasher_finish,
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
    str_get_utf8_byte_unsafe,
    str_substring_unsafe,
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
    dbg_str,
    expect_err_str,
    roc_expect_failed,
    roc_crashed,
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
    box_prepare_update,
    box_decref_with,
    box_decref_with_single_thread,
    box_free_with,
    erased_callable_incref,
    erased_callable_decref,
    erased_callable_decref_single_thread,
    erased_callable_repack,
    erased_callable_free,
    hot_reload_enter,
    hot_reload_leave,
    hot_reload_retain_current,
    hot_reload_erased_callable_drop,
    allocate_with_refcount,
    incref_data_ptr,
    incref_data_ptr_single_thread,
    decref_data_ptr,
    decref_data_ptr_single_thread,
    free_data_ptr,
    str_from_literal,
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
    f32_to_int_wrap,
    f64_to_int_wrap,
    dec_to_f32_try_unsafe,
    f64_to_f32_try_unsafe,
    i128_to_dec_try_unsafe,
    u128_to_dec_try_unsafe,
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
    list_eq,
    list_str_eq,
    list_list_eq,
    list_reverse,
    i32_mod_by,
    i8_mod_by,
    u8_mod_by,
    i16_mod_by,
    u16_mod_by,
    u32_mod_by,
    i64_mod_by,
    u64_mod_by,

    /// The linker symbol this builtin is exported and resolved under.
    pub fn symbolName(self: BuiltinFn) [:0]const u8 {
        switch (self) {
            inline else => |f| return symbol_prefix ++ @tagName(f),
        }
    }

    /// The wrapper function backing this builtin, typed per member.
    pub fn wrapper(comptime self: BuiltinFn) *const @TypeOf(@field(dev_wrappers, symbol_prefix ++ @tagName(self))) {
        return &@field(dev_wrappers, symbol_prefix ++ @tagName(self));
    }

    /// Address of the wrapper function (dev-JIT native calls and symbol resolution).
    pub fn wrapperAddress(self: BuiltinFn) usize {
        switch (self) {
            inline else => |f| return @intFromPtr(wrapper(f)),
        }
    }

    /// Which linkable builtins payloads carry a builtin's wrapper.
    pub const Payload = enum {
        /// Resolved only in-process by the dev JIT; exported by no payload.
        jit_only,
        /// Exported by the full payload only.
        full,
        /// Exported by both the full payload and the minimal core payload.
        core,
    };

    /// Payload membership for this builtin. Members not listed here are
    /// `.full`: exported by the full payload and absent from the core one,
    /// the right default for a newly added builtin.
    pub fn payload(self: BuiltinFn) Payload {
        return switch (self) {
            .allocate_with_refcount,
            .box_decref_with,
            .box_decref_with_single_thread,
            .box_free_with,
            .box_prepare_update,
            .dbg_str,
            .decref_data_ptr,
            .decref_data_ptr_single_thread,
            .erased_callable_decref,
            .erased_callable_decref_single_thread,
            .erased_callable_free,
            .erased_callable_incref,
            .erased_callable_repack,
            .expect_err_str,
            .free_data_ptr,
            .i16_mod_by,
            .i32_mod_by,
            .i64_mod_by,
            .i8_mod_by,
            .incref_data_ptr,
            .incref_data_ptr_single_thread,
            .int_from_str,
            .int_to_str,
            .list_append_unsafe,
            .list_concat,
            .list_decref_flat_list,
            .list_decref_str,
            .list_decref_with,
            .list_decref_with_single_thread,
            .list_drop_at,
            .list_eq,
            .list_free_flat_list,
            .list_free_with,
            .list_incref,
            .list_incref_single_thread,
            .list_list_eq,
            .list_prepend,
            .list_release_excess_capacity,
            .list_replace,
            .list_reserve,
            .list_reverse,
            .list_str_eq,
            .list_sublist,
            .list_swap,
            .list_with_capacity,
            .num_mul_with_overflow_i128,
            .num_mul_with_overflow_u128,
            .roc_crashed,
            .roc_expect_failed,
            .str_caseless_ascii_equals,
            .str_concat,
            .str_contains,
            .str_count_utf8_bytes,
            .str_drop_prefix,
            .str_drop_prefix_caseless_ascii,
            .str_drop_suffix,
            .str_ends_with,
            .str_equal,
            .str_equal_static_small,
            .str_escape_and_quote,
            .str_find_first,
            .str_get_utf8_byte_unsafe,
            .str_from_literal,
            .str_from_utf8,
            .str_from_utf8_lossy,
            .str_from_utf8_parts,
            .str_from_utf8_result,
            .str_join_with,
            .str_release_excess_capacity,
            .str_repeat,
            .str_reserve,
            .str_split,
            .str_starts_with,
            .str_static_small_word_caseless_eq,
            .str_static_small_word_eq,
            .str_substring_unsafe,
            .str_to_utf8,
            .str_trim,
            .str_trim_end,
            .str_trim_start,
            .str_with_ascii_lowercased,
            .str_with_ascii_uppercased,
            .str_with_capacity,
            .u16_mod_by,
            .u32_mod_by,
            .u64_mod_by,
            .u8_mod_by,
            => .core,

            .float_ceiling,
            .float_floor,
            .hot_reload_enter,
            .hot_reload_erased_callable_drop,
            .hot_reload_leave,
            .hot_reload_retain_current,
            .num_shl_u128,
            .num_shr_i128,
            .num_shr_u128,
            => .jit_only,

            else => .full,
        };
    }
};

/// Which exported subset of the registry a payload root links.
pub const ExportSet = enum {
    /// Everything linkable: `.core` and `.full` members (`static_lib.zig`).
    all_linked,
    /// Only `.core` members (`static_lib_core.zig`).
    core,
};

/// `@export` every wrapper in `set` under its `symbolName`. Called from the
/// `comptime` block of a payload root.
pub fn exportWrappers(comptime set: ExportSet) void {
    @setEvalBranchQuota(100_000);
    inline for (comptime std.enums.values(BuiltinFn)) |f| {
        const included = switch (comptime f.payload()) {
            .jit_only => false,
            .full => set == .all_linked,
            .core => true,
        };
        if (included) @export(BuiltinFn.wrapper(f), .{ .name = f.symbolName() });
    }
}

// Overflow-arithmetic exports (`roc__num_*` scheme) present in every payload.
// One table drives both the `@export`s and the allowlist names, so the two
// cannot disagree.
const overflow_exports = .{
    .{ num.exportMulWithOverflow, "roc__num_mul_with_overflow_", i64 },
    .{ num.exportMulWithOverflow, "roc__num_mul_with_overflow_", i32 },
    .{ num.exportMulWithOverflow, "roc__num_mul_with_overflow_", i16 },
    .{ num.exportMulWithOverflow, "roc__num_mul_with_overflow_", i8 },
    .{ num.exportAddWithOverflow, "roc__num_add_with_overflow_", i128 },
    .{ num.exportSubWithOverflow, "roc__num_sub_with_overflow_", i128 },
};

/// `@export` the overflow helpers shipped alongside the wrapper exports in
/// every payload. Called from the `comptime` block of a payload root.
pub fn exportOverflowWrappers() void {
    inline for (overflow_exports) |e| e[0](e[2], e[1]);
}

/// Linker names of the overflow exports (each helper exports as
/// `base ++ @typeName(T)`).
pub const overflow_root_symbols = blk: {
    var names: [overflow_exports.len][:0]const u8 = undefined;
    for (overflow_exports, 0..) |e, i| names[i] = e[1] ++ @typeName(e[2]);
    const frozen = names;
    break :blk frozen;
};

/// Symbols an app may declare as roots while still linking the core payload:
/// every `.core` registry export plus the overflow helpers. Deriving this from
/// the enum keeps the root allowlist in step with what the core payload
/// actually exports.
pub const core_root_symbols: std.StaticStringMap(void) = blk: {
    @setEvalBranchQuota(100_000);
    var core_count: usize = 0;
    for (std.enums.values(BuiltinFn)) |f| {
        if (f.payload() == .core) core_count += 1;
    }
    const total = core_count + overflow_root_symbols.len;
    var kvs: [total]struct { []const u8 } = undefined;
    var i: usize = 0;
    for (std.enums.values(BuiltinFn)) |f| {
        if (f.payload() == .core) {
            kvs[i] = .{f.symbolName()};
            i += 1;
        }
    }
    for (overflow_root_symbols) |name| {
        kvs[i] = .{name};
        i += 1;
    }
    const frozen = kvs;
    break :blk std.StaticStringMap(void).initComptime(frozen);
};

/// Fully qualified names of annotation-only Builtin.roc declarations that are
/// compiler intrinsics rather than low-level-op wrappers: checking and
/// post-check lowering handle them from checked data, so canonicalization
/// exempts them from the rule that every annotation-only builtin def must map
/// to a low-level op.
pub const intrinsic_annotation_names = [_][]const u8{
    "Builtin.Str.inspect",
    "Builtin.Str.Utf8Problem.is_eq",
    "Builtin.Encoding.ParseTagUnionSpec.parse",
    "Builtin.Encoding.FieldName.FieldNames.rename_fields",
    "Builtin.Encoding.FieldName.FieldNames.shortest_name",
    "Builtin.Encoding.FieldName.FieldNames.longest_name",
    "Builtin.Encoding.FieldName.FieldNames.iter",
    "Builtin.Encoding.FieldName.FieldNames.for_size",
    "Builtin.Encoding.FieldName.name",
};

comptime {
    @setEvalBranchQuota(200_000);
    // Every member is backed by a wrapper with the exact symbol name.
    for (std.enums.values(BuiltinFn)) |f| {
        const name = symbol_prefix ++ @tagName(f);
        if (!@hasDecl(dev_wrappers, name)) {
            @compileError("BuiltinFn." ++ @tagName(f) ++ " has no wrapper named " ++ name ++ " in dev_wrappers.zig");
        }
    }
    // Every wrapper is registered as a member.
    for (@typeInfo(dev_wrappers).@"struct".decls) |decl| {
        if (!std.mem.startsWith(u8, decl.name, symbol_prefix)) continue;
        if (@typeInfo(@TypeOf(@field(dev_wrappers, decl.name))) != .@"fn") continue;
        if (!@hasField(BuiltinFn, decl.name[symbol_prefix.len..])) {
            @compileError("dev_wrappers." ++ decl.name ++ " is not registered; add BuiltinFn." ++ decl.name[symbol_prefix.len..] ++ " to builtin_registry.zig");
        }
    }
}

test "symbol names round-trip through the registry" {
    inline for (comptime std.enums.values(BuiltinFn)) |f| {
        const name = f.symbolName();
        try std.testing.expect(std.mem.startsWith(u8, name, symbol_prefix));
        try std.testing.expectEqual(f, std.meta.stringToEnum(BuiltinFn, name[symbol_prefix.len..]).?);
        try std.testing.expect(f.wrapperAddress() != 0);
    }
}
