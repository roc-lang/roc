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

/// Float variant of a unary transcendental math op.
pub fn unaryMathFloat(op: LowLevel) BuiltinFn {
    return switch (op) {
        .num_sin => .float_sin,
        .num_cos => .float_cos,
        .num_tan => .float_tan,
        .num_asin => .float_asin,
        .num_acos => .float_acos,
        .num_atan => .float_atan,
        else => unreachable,
    };
}

/// Dec variant of a unary math op.
pub fn unaryMathDec(op: LowLevel) BuiltinFn {
    return switch (op) {
        .num_sin => .dec_sin,
        .num_cos => .dec_cos,
        .num_tan => .dec_tan,
        .num_asin => .dec_asin,
        .num_acos => .dec_acos,
        .num_atan => .dec_atan,
        .num_sqrt => .dec_sqrt,
        else => unreachable,
    };
}

/// Float rounding ops, for backends that call rather than inline them.
pub fn floatRounding(op: LowLevel) BuiltinFn {
    return switch (op) {
        .num_floor => .float_floor,
        .num_ceiling => .float_ceiling,
        else => unreachable,
    };
}

/// Binary Dec arithmetic. `num_times` maps to `dec_mul`, which crashes on
/// overflow like the interpreter's Dec multiply; the saturating
/// `dec_mul_saturated` wrapper is not the lowering of any current op.
pub fn decBinaryArith(op: LowLevel) BuiltinFn {
    return switch (op) {
        .num_times => .dec_mul,
        .num_div_by => .dec_div,
        .num_div_trunc_by => .dec_div_trunc,
        .num_pow => .dec_pow,
        else => unreachable,
    };
}

/// Float `num_pow`, for backends that call rather than inline it.
pub fn floatPow() BuiltinFn {
    return .float_pow;
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
    return switch (op) {
        .str_is_eq => .str_equal,
        .str_is_eq_static_small => .str_equal_static_small,
        .str_static_small_word_eq => .str_static_small_word_eq,
        .str_static_small_word_caseless_eq => .str_static_small_word_caseless_eq,
        .str_concat => .str_concat,
        .str_contains => .str_contains,
        .str_trim => .str_trim,
        .str_trim_start => .str_trim_start,
        .str_trim_end => .str_trim_end,
        .str_caseless_ascii_equals => .str_caseless_ascii_equals,
        .str_with_ascii_lowercased => .str_with_ascii_lowercased,
        .str_with_ascii_uppercased => .str_with_ascii_uppercased,
        .str_starts_with => .str_starts_with,
        .str_ends_with => .str_ends_with,
        .str_repeat => .str_repeat,
        .str_drop_prefix => .str_drop_prefix,
        .str_drop_prefix_caseless_ascii => .str_drop_prefix_caseless_ascii,
        .str_drop_suffix => .str_drop_suffix,
        .str_find_first => .str_find_first,
        .str_count_utf8_bytes => .str_count_utf8_bytes,
        .str_with_capacity => .str_with_capacity,
        .str_reserve => .str_reserve,
        .str_release_excess_capacity => .str_release_excess_capacity,
        .str_to_utf8 => .str_to_utf8,
        .str_from_utf8_lossy => .str_from_utf8_lossy,
        .str_from_utf8 => .str_from_utf8_result,
        .str_split_on => .str_split,
        .str_join_with => .str_join_with,
        .str_inspect => .str_escape_and_quote,
        else => unreachable,
    };
}

/// List ops that lower to a single builtin, for backends that call rather
/// than inline them. The sublist-shaped ops all share one wrapper.
pub fn listOp(op: LowLevel) BuiltinFn {
    return switch (op) {
        .list_with_capacity => .list_with_capacity,
        .list_append_unsafe => .list_append_unsafe,
        .list_concat => .list_concat,
        .list_prepend => .list_prepend,
        .list_sublist,
        .list_drop_first,
        .list_drop_last,
        .list_take_first,
        .list_take_last,
        => .list_sublist,
        .list_drop_at => .list_drop_at,
        .list_swap => .list_swap,
        .list_set, .list_replace_unsafe => .list_replace,
        .list_reserve => .list_reserve,
        .list_release_excess_capacity => .list_release_excess_capacity,
        .list_reverse => .list_reverse,
        .list_map_can_reuse => .list_map_can_reuse,
        else => unreachable,
    };
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
    return switch (op) {
        .dict_pseudo_seed => .dict_pseudo_seed,
        .hasher_finish => .hasher_finish,
        .hasher_write_bool,
        .hasher_write_u8,
        .hasher_write_u16,
        .hasher_write_u32,
        .hasher_write_u64,
        .hasher_write_i8,
        .hasher_write_i16,
        .hasher_write_i32,
        .hasher_write_i64,
        => .hasher_write_u64,
        .hasher_write_u128, .hasher_write_i128, .hasher_write_dec => .hasher_write_u128,
        .hasher_write_f32 => .hasher_write_f32_bits,
        .hasher_write_f64 => .hasher_write_f64_bits,
        .hasher_write_bytes => .hasher_write_bytes,
        .hasher_write_str => .hasher_write_str,
        else => unreachable,
    };
}

/// Crypto primitives, 1:1 with their wrappers.
pub fn cryptoOp(op: LowLevel) BuiltinFn {
    return switch (op) {
        .crypto_sha256_hash_bytes => .crypto_sha256_hash_bytes,
        .crypto_sha256_hasher_empty => .crypto_sha256_hasher_empty,
        .crypto_sha256_hasher_write => .crypto_sha256_hasher_write,
        .crypto_sha256_hasher_finish => .crypto_sha256_hasher_finish,
        .crypto_blake3_hash_bytes => .crypto_blake3_hash_bytes,
        .crypto_blake3_hasher_empty => .crypto_blake3_hasher_empty,
        .crypto_blake3_hasher_write => .crypto_blake3_hasher_write,
        .crypto_blake3_hasher_finish => .crypto_blake3_hasher_finish,
        else => unreachable,
    };
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
