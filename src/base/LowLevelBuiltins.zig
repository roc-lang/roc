//! The single shared mapping from `LowLevel` ops to the builtin registry
//! members they lower to.
//!
//! Whether a backend calls a builtin or emits inline code for an op is a
//! per-backend decision; WHICH builtin an op lowers to is not. Every backend
//! that calls a builtin for an op selects the member through this module, so
//! two backends cannot lower the same op (under the same operand conditions)
//! to different symbols or to wrappers with different semantics.
//!
//! Families whose member depends only on the op are keyed by `LowLevel`;
//! families whose member depends on operand facts (width, signedness,
//! atomicity, element kind) are keyed by those facts directly.

const LowLevel = @import("LowLevel.zig").LowLevel;
const builtins = @import("builtins");

pub const BuiltinFn = builtins.builtin_registry.BuiltinFn;

fn lookup(op: LowLevel, mappings: anytype) BuiltinFn {
    inline for (mappings) |mapping| {
        if (op == mapping[0]) return mapping[1];
    }
    unreachable;
}

/// Float variant of a unary transcendental math op.
pub fn unaryMathFloat(op: LowLevel, is_f32: bool) BuiltinFn {
    return lookup(op, .{
        .{ LowLevel.num_sin, if (is_f32) BuiltinFn.float_sin_f32 else BuiltinFn.float_sin },
        .{ LowLevel.num_cos, if (is_f32) BuiltinFn.float_cos_f32 else BuiltinFn.float_cos },
        .{ LowLevel.num_tan, if (is_f32) BuiltinFn.float_tan_f32 else BuiltinFn.float_tan },
        .{ LowLevel.num_asin, if (is_f32) BuiltinFn.float_asin_f32 else BuiltinFn.float_asin },
        .{ LowLevel.num_acos, if (is_f32) BuiltinFn.float_acos_f32 else BuiltinFn.float_acos },
        .{ LowLevel.num_atan, if (is_f32) BuiltinFn.float_atan_f32 else BuiltinFn.float_atan },
    });
}

/// Dec variant of a unary math op.
pub fn unaryMathDec(op: LowLevel) BuiltinFn {
    return lookup(op, .{
        .{ LowLevel.num_sin, BuiltinFn.dec_sin },
        .{ LowLevel.num_cos, BuiltinFn.dec_cos },
        .{ LowLevel.num_tan, BuiltinFn.dec_tan },
        .{ LowLevel.num_asin, BuiltinFn.dec_asin },
        .{ LowLevel.num_acos, BuiltinFn.dec_acos },
        .{ LowLevel.num_atan, BuiltinFn.dec_atan },
        .{ LowLevel.num_sqrt, BuiltinFn.dec_sqrt },
    });
}

/// Float rounding ops, for backends that call rather than inline them.
pub fn floatRounding(op: LowLevel, is_f32: bool) BuiltinFn {
    return lookup(op, .{
        .{ LowLevel.num_floor, if (is_f32) BuiltinFn.float_floor_f32 else BuiltinFn.float_floor },
        .{ LowLevel.num_ceiling, if (is_f32) BuiltinFn.float_ceiling_f32 else BuiltinFn.float_ceiling },
    });
}

/// Binary Dec arithmetic. `dec_mul` crashes on
/// overflow like the interpreter's Dec multiply; the saturating
/// `dec_mul_saturated` wrapper is not the lowering of any current op.
pub fn decBinaryArith(op: LowLevel) BuiltinFn {
    return lookup(op, .{
        .{ LowLevel.dec_mul, BuiltinFn.dec_mul },
        .{ LowLevel.num_div_by, BuiltinFn.dec_div },
        .{ LowLevel.num_div_trunc_by, BuiltinFn.dec_div_trunc },
        .{ LowLevel.num_pow, BuiltinFn.dec_pow },
    });
}

/// Float `num_pow`, for backends that call rather than inline it.
pub fn floatPow(is_f32: bool) BuiltinFn {
    return if (is_f32) .float_pow_f32 else .float_pow;
}

/// Float binary operations which lower through shared wrappers.
pub fn floatBinaryArith(op: LowLevel, is_f32: bool) BuiltinFn {
    return lookup(op, .{
        .{ LowLevel.num_div_trunc_by, if (is_f32) BuiltinFn.float_div_trunc_f32 else BuiltinFn.float_div_trunc },
        .{ LowLevel.num_rem_by, if (is_f32) BuiltinFn.float_rem_f32 else BuiltinFn.float_rem },
        .{ LowLevel.num_mod_by, if (is_f32) BuiltinFn.float_rem_f32 else BuiltinFn.float_rem },
    });
}

/// 128-bit truncating division / remainder.
pub fn i128DivRem(is_rem: bool, is_unsigned: bool) BuiltinFn {
    return if (is_unsigned)
        (if (is_rem) BuiltinFn.num_rem_trunc_u128 else BuiltinFn.num_div_trunc_u128)
    else
        (if (is_rem) BuiltinFn.num_rem_trunc_i128 else BuiltinFn.num_div_trunc_i128);
}

/// 128-bit modulo. Unsigned modulo equals the truncating remainder; signed
/// modulo has its own wrapper.
pub fn i128Mod(is_unsigned: bool) BuiltinFn {
    return if (is_unsigned) .num_rem_trunc_u128 else .num_mod_i128;
}

/// 128-bit multiply with overflow detection.
pub fn checkedMul128(is_unsigned: bool) BuiltinFn {
    return if (is_unsigned) .num_mul_with_overflow_u128 else .num_mul_with_overflow_i128;
}

/// Exact 128-bit integer-to-float conversion. The f32 wrappers round directly
/// from the integer; they must not first round the integer to f64.
pub fn int128ToFloat(is_signed: bool, is_f32: bool) BuiltinFn {
    return if (is_signed)
        (if (is_f32) .i128_to_f32 else .i128_to_f64)
    else
        (if (is_f32) .u128_to_f32 else .u128_to_f64);
}

/// Decimal-to-float conversion selected by destination width.
pub fn decToFloat(is_f32: bool) BuiltinFn {
    return if (is_f32) .dec_to_f32 else .dec_to_f64;
}

/// Checked 128-bit integer-to-Dec conversion. The wrappers report whether the
/// value fits; `unsafe` in their names is about the raw output pointer.
pub fn int128ToDec(is_signed: bool) BuiltinFn {
    return if (is_signed) .i128_to_dec_try_unsafe else .u128_to_dec_try_unsafe;
}

/// Scalar integer modulo by width and signedness, for backends that call
/// rather than inline it.
pub fn scalarModBy(bits: u16, is_signed: bool) BuiltinFn {
    return switch (bits) {
        8 => if (is_signed) BuiltinFn.i8_mod_by else BuiltinFn.u8_mod_by,
        16 => if (is_signed) BuiltinFn.i16_mod_by else BuiltinFn.u16_mod_by,
        32 => if (is_signed) BuiltinFn.i32_mod_by else BuiltinFn.u32_mod_by,
        64 => if (is_signed) BuiltinFn.i64_mod_by else BuiltinFn.u64_mod_by,
        else => unreachable,
    };
}

/// The numeric class a to_str / from_str op operates on.
pub const NumericClass = enum { int, float, dec };

/// Numeric-to-string formatting.
pub fn numToStr(class: NumericClass) BuiltinFn {
    return switch (class) {
        .int => .int_to_str,
        .float => .float_to_str,
        .dec => .dec_to_str,
    };
}

/// Numeric parsing from a string.
pub fn numFromStr(class: NumericClass) BuiltinFn {
    return switch (class) {
        .int => .int_from_str,
        .float => .float_from_str,
        .dec => .dec_from_str,
    };
}

/// Checked integer narrowing. Sources up to 64 bits use the bounds-checked
/// scalar wrappers; 128-bit sources use the range-checked i128 wrappers.
pub fn intTryConvert(src_is_128: bool, src_is_signed: bool) BuiltinFn {
    return if (src_is_128)
        (if (src_is_signed) BuiltinFn.i128_try_convert else BuiltinFn.u128_try_convert)
    else
        (if (src_is_signed) BuiltinFn.int_try_signed else BuiltinFn.int_try_unsigned);
}

/// String ops that lower to a single builtin, for backends that call rather
/// than inline them.
pub fn strOp(op: LowLevel) BuiltinFn {
    return lookup(op, .{
        .{ LowLevel.str_is_eq, BuiltinFn.str_equal },
        .{ LowLevel.str_is_eq_static_small, BuiltinFn.str_equal_static_small },
        .{ LowLevel.str_static_small_word_eq, BuiltinFn.str_static_small_word_eq },
        .{ LowLevel.str_static_small_word_caseless_eq, BuiltinFn.str_static_small_word_caseless_eq },
        .{ LowLevel.str_concat, BuiltinFn.str_concat },
        .{ LowLevel.str_contains, BuiltinFn.str_contains },
        .{ LowLevel.str_trim, BuiltinFn.str_trim },
        .{ LowLevel.str_trim_start, BuiltinFn.str_trim_start },
        .{ LowLevel.str_trim_end, BuiltinFn.str_trim_end },
        .{ LowLevel.str_caseless_ascii_equals, BuiltinFn.str_caseless_ascii_equals },
        .{ LowLevel.str_with_ascii_lowercased, BuiltinFn.str_with_ascii_lowercased },
        .{ LowLevel.str_with_ascii_uppercased, BuiltinFn.str_with_ascii_uppercased },
        .{ LowLevel.str_starts_with, BuiltinFn.str_starts_with },
        .{ LowLevel.str_ends_with, BuiltinFn.str_ends_with },
        .{ LowLevel.str_repeat, BuiltinFn.str_repeat },
        .{ LowLevel.str_drop_prefix, BuiltinFn.str_drop_prefix },
        .{ LowLevel.str_drop_prefix_caseless_ascii, BuiltinFn.str_drop_prefix_caseless_ascii },
        .{ LowLevel.str_drop_suffix, BuiltinFn.str_drop_suffix },
        .{ LowLevel.str_split_first, BuiltinFn.str_split_first },
        .{ LowLevel.str_split_last, BuiltinFn.str_split_last },
        .{ LowLevel.str_count_utf8_bytes, BuiltinFn.str_count_utf8_bytes },
        .{ LowLevel.str_get_utf8_byte_unsafe, BuiltinFn.str_get_utf8_byte_unsafe },
        .{ LowLevel.str_substring_unsafe, BuiltinFn.str_substring_unsafe },
        .{ LowLevel.str_with_capacity, BuiltinFn.str_with_capacity },
        .{ LowLevel.str_reserve, BuiltinFn.str_reserve },
        .{ LowLevel.str_release_excess_capacity, BuiltinFn.str_release_excess_capacity },
        .{ LowLevel.str_to_utf8, BuiltinFn.str_to_utf8 },
        .{ LowLevel.str_from_utf8_lossy, BuiltinFn.str_from_utf8_lossy },
        .{ LowLevel.str_from_utf8, BuiltinFn.str_from_utf8_result },
        .{ LowLevel.str_split_on, BuiltinFn.str_split },
        .{ LowLevel.str_join_with, BuiltinFn.str_join_with },
        .{ LowLevel.str_inspect, BuiltinFn.str_escape_and_quote },
    });
}

/// List ops that lower to a single builtin, for backends that call rather
/// than inline them. The owned sublist-shaped ops share one wrapper; ARC's
/// borrowed sublist variant uses its non-consuming wrapper.
pub fn listOp(op: LowLevel) BuiltinFn {
    return lookup(op, .{
        .{ LowLevel.list_with_capacity, BuiltinFn.list_with_capacity },
        .{ LowLevel.list_append_unsafe, BuiltinFn.list_append_unsafe },
        .{ LowLevel.list_concat, BuiltinFn.list_concat },
        .{ LowLevel.list_append_range_within, BuiltinFn.list_append_range_within },
        .{ LowLevel.list_copy_range_within, BuiltinFn.list_copy_range_within },
        .{ LowLevel.list_append_range_within_unsafe, BuiltinFn.list_append_range_within_unsafe },
        .{ LowLevel.list_append_sublist, BuiltinFn.list_append_sublist },
        .{ LowLevel.list_append_le_bytes, BuiltinFn.list_append_le_bytes },
        .{ LowLevel.list_slack_unique, BuiltinFn.list_slack_unique },
        .{ LowLevel.list_owned_unique, BuiltinFn.list_owned_unique },
        .{ LowLevel.list_prepend, BuiltinFn.list_prepend },
        .{ LowLevel.list_sublist, BuiltinFn.list_sublist },
        .{ LowLevel.list_drop_first, BuiltinFn.list_sublist },
        .{ LowLevel.list_drop_last, BuiltinFn.list_sublist },
        .{ LowLevel.list_take_first, BuiltinFn.list_sublist },
        .{ LowLevel.list_take_last, BuiltinFn.list_sublist },
        .{ LowLevel.list_sublist_borrowed, BuiltinFn.list_sublist_borrowed },
        .{ LowLevel.list_drop_at, BuiltinFn.list_drop_at },
        .{ LowLevel.list_swap, BuiltinFn.list_swap },
        .{ LowLevel.list_set, BuiltinFn.list_set },
        .{ LowLevel.list_replace_unsafe, BuiltinFn.list_replace },
        .{ LowLevel.list_set_in_place_unsafe, BuiltinFn.list_replace },
        .{ LowLevel.list_reserve, BuiltinFn.list_reserve },
        .{ LowLevel.list_release_excess_capacity, BuiltinFn.list_release_excess_capacity },
        .{ LowLevel.list_reverse, BuiltinFn.list_reverse },
        .{ LowLevel.list_map_can_reuse, BuiltinFn.list_map_can_reuse },
    });
}

/// Element kind selecting a structural list-equality wrapper.
pub const ListEqElem = enum { str, list, flat };

/// Structural list equality by element kind, for backends that call rather
/// than inline it.
pub fn listEq(elem: ListEqElem) BuiltinFn {
    return switch (elem) {
        .str => .list_str_eq,
        .list => .list_list_eq,
        .flat => .list_eq,
    };
}

/// Hasher primitives. Scalar writes funnel into the width-normalized wrappers:
/// everything up to 64 bits hashes through `hasher_write_u64`, and `Dec` --
/// which is a 128-bit fixed-point value, not a float -- hashes through
/// `hasher_write_u128` alongside u128/i128.
///
/// `Dec` used to be mapped onto `hasher_write_f64_bits` here. That wrapper takes
/// (seed, bits), while the 128-bit wrapper takes (seed, domain, low, high), so
/// the one caller that passes a runtime `op` rather than a comptime literal --
/// the LLVM backend -- emitted a four-argument call to a two-parameter function.
/// The Dec's actual value was never read, and Dict lookups on Dec-keyed records
/// and tuples returned KeyNotFound on x86_64-windows.
pub fn hasherOp(op: LowLevel) BuiltinFn {
    return lookup(op, .{
        .{ LowLevel.dict_pseudo_seed, BuiltinFn.dict_pseudo_seed },
        .{ LowLevel.hasher_finish, BuiltinFn.hasher_finish },
        .{ LowLevel.hasher_write_bool, BuiltinFn.hasher_write_u64 },
        .{ LowLevel.hasher_write_u8, BuiltinFn.hasher_write_u64 },
        .{ LowLevel.hasher_write_u16, BuiltinFn.hasher_write_u64 },
        .{ LowLevel.hasher_write_u32, BuiltinFn.hasher_write_u64 },
        .{ LowLevel.hasher_write_u64, BuiltinFn.hasher_write_u64 },
        .{ LowLevel.hasher_write_i8, BuiltinFn.hasher_write_u64 },
        .{ LowLevel.hasher_write_i16, BuiltinFn.hasher_write_u64 },
        .{ LowLevel.hasher_write_i32, BuiltinFn.hasher_write_u64 },
        .{ LowLevel.hasher_write_i64, BuiltinFn.hasher_write_u64 },
        .{ LowLevel.hasher_write_u128, BuiltinFn.hasher_write_u128 },
        .{ LowLevel.hasher_write_i128, BuiltinFn.hasher_write_u128 },
        .{ LowLevel.hasher_write_dec, BuiltinFn.hasher_write_u128 },
        .{ LowLevel.hasher_write_f32, BuiltinFn.hasher_write_f32_bits },
        .{ LowLevel.hasher_write_f64, BuiltinFn.hasher_write_f64_bits },
        .{ LowLevel.hasher_write_bytes, BuiltinFn.hasher_write_bytes },
        .{ LowLevel.hasher_write_str, BuiltinFn.hasher_write_str },
    });
}

/// Crypto primitives, 1:1 with their wrappers.
pub fn cryptoOp(op: LowLevel) BuiltinFn {
    return lookup(op, .{
        .{ LowLevel.crypto_sha256_hash_bytes, BuiltinFn.crypto_sha256_hash_bytes },
        .{ LowLevel.crypto_sha256_hasher_empty, BuiltinFn.crypto_sha256_hasher_empty },
        .{ LowLevel.crypto_sha256_hasher_write, BuiltinFn.crypto_sha256_hasher_write },
        .{ LowLevel.crypto_sha256_hasher_finish, BuiltinFn.crypto_sha256_hasher_finish },
        .{ LowLevel.crypto_blake3_hash_bytes, BuiltinFn.crypto_blake3_hash_bytes },
        .{ LowLevel.crypto_blake3_hasher_empty, BuiltinFn.crypto_blake3_hasher_empty },
        .{ LowLevel.crypto_blake3_hasher_write, BuiltinFn.crypto_blake3_hasher_write },
        .{ LowLevel.crypto_blake3_hasher_finish, BuiltinFn.crypto_blake3_hasher_finish },
    });
}

/// Refcount-helper shapes that lower to builtin calls.
pub const RcHelper = enum {
    data_ptr_incref,
    data_ptr_decref,
    data_ptr_free,
    list_incref,
    list_decref,
    list_free,
    box_decref,
    box_free,
    erased_callable_incref,
    erased_callable_decref,
    erased_callable_free,
};

/// Whether a refcount update may race with other threads.
pub const RcAtomicity = enum { atomic, single_thread };

/// Refcount helper wrappers by shape and atomicity. Shapes without a
/// single-thread variant (the frees, which only run on unique values)
/// ignore the atomicity.
pub fn rcHelper(helper: RcHelper, atomicity: RcAtomicity) BuiltinFn {
    const single = atomicity == .single_thread;
    return switch (helper) {
        .data_ptr_incref => if (single) BuiltinFn.incref_data_ptr_single_thread else BuiltinFn.incref_data_ptr,
        .data_ptr_decref => if (single) BuiltinFn.decref_data_ptr_single_thread else BuiltinFn.decref_data_ptr,
        .data_ptr_free => .free_data_ptr,
        .list_incref => if (single) BuiltinFn.list_incref_single_thread else BuiltinFn.list_incref,
        .list_decref => if (single) BuiltinFn.list_decref_with_single_thread else BuiltinFn.list_decref_with,
        .list_free => .list_free_with,
        .box_decref => if (single) BuiltinFn.box_decref_with_single_thread else BuiltinFn.box_decref_with,
        .box_free => .box_free_with,
        .erased_callable_incref => if (single) BuiltinFn.incref_data_ptr_single_thread else BuiltinFn.erased_callable_incref,
        .erased_callable_decref => if (single) BuiltinFn.erased_callable_decref_single_thread else BuiltinFn.erased_callable_decref,
        .erased_callable_free => .erased_callable_free,
    };
}
