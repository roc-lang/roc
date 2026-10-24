//! The layouts every `LowLevel` operation takes and returns in LIR.
//!
//! A backend reads an operation's operands and result through their layouts:
//! a list index is a `U64`, a string operation's text is a `Str`, the element
//! appended to a list has the list's element layout. Any other layout there
//! can only come from a compiler bug in an earlier stage, so each operation
//! declares those layouts here, and the debug-only LIR verifier
//! (`src/lir/low_level_layouts.zig`) checks every `assign_low_level` a
//! procedure reaches against them (design.md "Debug Invariants").
//!
//! `base` cannot see the layout store, so a signature names layouts through
//! `Operand` constraints that the verifier interprets.

const std = @import("std");
const LowLevel = @import("LowLevel.zig").LowLevel;
const numeric_conversion = @import("numeric_conversion.zig");

/// A builtin layout with a fixed index. Each name is a member of `layout.Idx`.
pub const Prim = enum {
    bool,
    str,
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
    zst,
    u8x16,
    i8x16,
    u16x8,
    i16x8,
    u32x4,
    i32x4,
    u64x2,
    i64x2,
};

/// The argument or result whose layout a constraint refers to.
pub const Slot = union(enum) {
    arg: u8,
    result,
};

/// What one argument's or the result's layout must be.
pub const Operand = union(enum) {
    /// Any layout.
    any,
    /// Exactly this builtin layout.
    exact: Prim,
    /// A fixed-width integer.
    int,
    /// A fixed-width integer or `Dec`.
    int_or_dec,
    /// `F32` or `F64`.
    float,
    /// `F32`, `F64`, or `Dec`.
    frac,
    /// A fixed-width integer, `F32`, `F64`, or `Dec`.
    num,
    /// A 128-bit integer SIMD vector.
    vector,
    /// Any tag union.
    tag_union,
    /// A layout satisfying at least one of these.
    any_of: []const Operand,
    /// The same layout as another slot.
    same: Slot,
    /// The element layout of a list, the payload layout of a box, or the
    /// pointee layout of a pointer in another slot. A list or box of a
    /// zero-sized type stores no element layout, so its inner layout must be
    /// zero-sized.
    inner_of: Slot,
    /// The integer layout of one lane of a vector in another slot.
    lane_of: Slot,
    /// The vector whose lanes are twice as wide as another slot's vector's,
    /// with the same signedness.
    widened_of: Slot,
    /// A vector whose lanes are half as wide as another slot's vector's.
    narrowed_of: Slot,
    /// The integer a vector in another slot sums its lanes into: the lane's
    /// signedness at twice its width, and never narrower than 32 bits.
    lane_sum_of: Slot,
    /// The integer holding one bit per lane of a vector in another slot:
    /// `U16` for sixteen lanes, `U8` for fewer.
    bitmask_of: Slot,
    /// The unsigned integer of the same width as an integer in another slot,
    /// or that slot's own layout when it is `F32`, `F64`, or `Dec`.
    unsigned_of: Slot,
    /// A list whose element satisfies this.
    list: *const Operand,
    /// A box whose payload satisfies this.
    box: *const Operand,
    /// A compiler-internal pointer whose pointee satisfies this.
    ptr: *const Operand,
    /// A struct whose semantic fields, in semantic order, satisfy these.
    struct_fields: []const Operand,
};

/// The layouts one operation takes and returns.
pub const Signature = struct {
    args: []const Operand,
    /// Arguments that may follow `args`, each optional only after all
    /// earlier ones are present.
    optional_args: []const Operand = &.{},
    result: Operand,
};

/// Whether an operation may appear in LIR, and its signature there.
pub const LirSignature = union(enum) {
    /// An earlier stage always rewrites the operation into something else.
    absent,
    present: Signature,
};

/// The signature of `op` in LIR.
pub fn of(op: LowLevel) LirSignature {
    return signatures[@backingInt(op)];
}

const signatures = blk: {
    @setEvalBranchQuota(1 << 16);
    const ops = std.enums.values(LowLevel);
    var table: [ops.len]LirSignature = undefined;
    for (ops) |op| {
        table[@backingInt(op)] = if (numeric_conversion.getConversionSpec(op)) |conversion|
            conversionSignature(conversion)
        else if (numeric_conversion.getStringConversionSpec(op)) |conversion|
            stringConversionSignature(conversion)
        else
            @field(explicit, @tagName(op));
    }
    break :blk table;
};

fn prim(num: numeric_conversion.NumType) Prim {
    return @field(Prim, @tagName(num));
}

fn conversionSignature(comptime conversion: numeric_conversion.Conversion) LirSignature {
    const dst = exact(prim(conversion.dst));
    return sig(&.{exact(prim(conversion.src))}, switch (conversion.mode) {
        .exact, .wrap, .trunc => dst,
        .@"try" => .tag_union,
        // `{ success : U8, val_or_memory_garbage : T }`
        .try_unsafe => fields(&.{ exact(.u8), dst }),
    });
}

fn stringConversionSignature(comptime conversion: numeric_conversion.StringConversion) LirSignature {
    const num = exact(prim(conversion.num));
    return switch (conversion.direction) {
        .to_str => sig(&.{num}, exact(.str)),
        .from_str => sig(&.{exact(.str)}, .tag_union),
        // `{ err : U8, rest : Str, value : T }`
        .from_str_prefix => sig(&.{exact(.str)}, fields(&.{ exact(.u8), exact(.str), num })),
        // `{ err : U8, rest : List(U8), value : T }`
        .from_utf8_prefix => sig(&.{bytes}, fields(&.{ exact(.u8), same(0), num })),
    };
}

fn sig(comptime args: []const Operand, comptime result: Operand) LirSignature {
    return .{ .present = .{ .args = args, .result = result } };
}

fn exact(comptime p: Prim) Operand {
    return .{ .exact = p };
}

fn same(comptime arg: u8) Operand {
    return .{ .same = .{ .arg = arg } };
}

fn inner(comptime arg: u8) Operand {
    return .{ .inner_of = .{ .arg = arg } };
}

fn listOf(comptime elem: Operand) Operand {
    return .{ .list = &elem };
}

fn fields(comptime operands: []const Operand) Operand {
    return .{ .struct_fields = operands };
}

const any_list = listOf(.any);
const bytes = listOf(exact(.u8));
const str = exact(.str);
const u64_ = exact(.u64);
const boolean = exact(.bool);
const hasher = fields(&.{u64_});

fn unary(comptime operand: Operand) LirSignature {
    return sig(&.{operand}, same(0));
}

fn binary(comptime operand: Operand) LirSignature {
    return sig(&.{ operand, same(0) }, same(0));
}

fn compare(comptime operand: Operand) LirSignature {
    return sig(&.{ operand, same(0) }, boolean);
}

fn hasherWrite(comptime value: Operand) LirSignature {
    return sig(&.{ hasher, value }, same(0));
}

/// A struct with one field for each operation whose signature is not derived
/// from its numeric conversion, so `explicit` must give every such operation
/// its own row and cannot give one to a derived operation.
const Explicit = blk: {
    @setEvalBranchQuota(1 << 16);
    var names: []const []const u8 = &.{};
    for (std.enums.values(LowLevel)) |op| {
        if (numeric_conversion.getConversionSpec(op) != null) continue;
        if (numeric_conversion.getStringConversionSpec(op) != null) continue;
        names = names ++ .{@tagName(op)};
    }
    break :blk @Struct(.auto, null, names, &@splat(LirSignature), &@splat(.{}));
};

const explicit: Explicit = .{
    .str_is_eq = compare(str),
    // The text, then its length and its bytes as three little-endian words.
    .str_is_eq_static_small = sig(&.{ str, u64_, u64_, u64_, u64_ }, boolean),
    // The text, then a byte offset, the compared length, and one word.
    .str_static_small_word_eq = sig(&.{ str, u64_, u64_, u64_ }, boolean),
    .str_static_small_word_caseless_eq = sig(&.{ str, u64_, u64_, u64_ }, boolean),
    .str_concat = binary(str),
    .str_contains = compare(str),
    .str_trim = unary(str),
    .str_trim_start = unary(str),
    .str_trim_end = unary(str),
    .str_caseless_ascii_equals = compare(str),
    .str_with_ascii_lowercased = unary(str),
    .str_with_ascii_uppercased = unary(str),
    .str_starts_with = compare(str),
    .str_ends_with = compare(str),
    .str_repeat = sig(&.{ str, u64_ }, str),
    .str_drop_prefix = binary(str),
    // `{ after : Str, found : Bool }`
    .str_drop_prefix_caseless_ascii = sig(&.{ str, str }, fields(&.{ str, boolean })),
    .str_drop_suffix = binary(str),
    // `{ after : Str, before : Str, found : Bool }`
    .str_split_first = sig(&.{ str, str }, fields(&.{ str, str, boolean })),
    .str_split_last = sig(&.{ str, str }, fields(&.{ str, str, boolean })),
    .str_count_utf8_bytes = sig(&.{str}, u64_),
    .str_get_utf8_byte_unsafe = sig(&.{ str, u64_ }, exact(.u8)),
    .str_substring_unsafe = sig(&.{ str, u64_, u64_ }, str),
    .str_with_capacity = sig(&.{u64_}, str),
    .str_reserve = sig(&.{ str, u64_ }, str),
    .str_release_excess_capacity = unary(str),
    .str_to_utf8 = sig(&.{str}, bytes),
    .str_from_utf8_lossy = sig(&.{bytes}, str),
    .str_from_utf8_validated = sig(&.{bytes}, str),
    .str_from_utf16_le_short = sig(&.{bytes}, str),
    .str_from_utf16_be_short = sig(&.{bytes}, str),
    .str_from_utf32_le_short = sig(&.{bytes}, str),
    .str_from_utf32_be_short = sig(&.{bytes}, str),
    // `Try(Str, { index : U64, problem : Utf8Problem })`
    .str_from_utf8 = sig(&.{bytes}, .tag_union),
    .str_split_on = sig(&.{ str, str }, listOf(str)),
    .str_join_with = sig(&.{ listOf(str), str }, str),
    .str_inspect = unary(str),

    .list_len = sig(&.{any_list}, u64_),
    .list_capacity = sig(&.{any_list}, u64_),
    .list_get_unsafe = sig(&.{ any_list, u64_ }, inner(0)),
    .list_prefetched = .absent,
    .list_prefetch = sig(&.{ any_list, u64_ }, exact(.zst)),
    .list_append_unsafe = sig(&.{ any_list, inner(0) }, same(0)),
    .list_concat = binary(any_list),
    .list_append_range_within = sig(&.{ any_list, u64_, u64_ }, same(0)),
    .list_copy_range_within = sig(&.{ any_list, u64_, u64_, u64_ }, same(0)),
    .list_append_range_within_unsafe = sig(&.{ any_list, u64_, u64_ }, same(0)),
    .list_append_sublist = sig(&.{ any_list, same(0), u64_, u64_ }, same(0)),
    .list_append_le_bytes = sig(&.{ bytes, u64_, u64_ }, same(0)),
    .list_append_le_bytes_unsafe = sig(&.{ bytes, u64_, u64_ }, same(0)),
    .list_slack_unique = sig(&.{any_list}, u64_),
    .list_owned_unique = sig(&.{any_list}, u64_),
    .list_set_in_place_unsafe = sig(&.{ any_list, u64_, inner(0) }, same(0)),
    .list_with_capacity = sig(&.{u64_}, any_list),
    .list_drop_at = sig(&.{ any_list, u64_ }, same(0)),
    // `{ len : U64, start : U64 }`
    .list_sublist = sig(&.{ any_list, fields(&.{ u64_, u64_ }) }, same(0)),
    .list_sublist_borrowed = sig(&.{ any_list, fields(&.{ u64_, u64_ }) }, same(0)),
    .list_set = sig(&.{ any_list, u64_, inner(0) }, same(0)),
    // `{ list : List(item), prev : item }`
    .list_replace_unsafe = sig(&.{ any_list, u64_, inner(0) }, fields(&.{ same(0), inner(0) })),
    .list_swap = sig(&.{ any_list, u64_, u64_ }, same(0)),
    .list_prepend = sig(&.{ any_list, inner(0) }, same(0)),
    .list_take_first = sig(&.{ any_list, u64_ }, same(0)),
    .list_take_last = sig(&.{ any_list, u64_ }, same(0)),
    .list_reverse = unary(any_list),
    .list_reserve = sig(&.{ any_list, u64_ }, same(0)),
    .list_reserve_for_append = sig(&.{ any_list, u64_ }, same(0)),
    .list_release_excess_capacity = unary(any_list),
    .list_clear = unary(any_list),
    .list_map_prepare_reuse = unary(any_list),
    // The list, then the transform whose output element layout the
    // statement's `map_output_elem` commits.
    .list_map_can_reuse = sig(&.{ any_list, .any }, exact(.u8)),
    // The same allocation retyped to the output element layout.
    .list_map_cast_unsafe = sig(&.{any_list}, any_list),
    // An input element read out of a list already retyped to the output.
    .list_map_extract_unsafe = sig(&.{ any_list, u64_ }, .any),
    .list_map_write_unsafe = sig(&.{ any_list, u64_, inner(0) }, same(0)),
    .list_item_size_at_most = sig(&.{ any_list, u64_ }, exact(.u8)),

    .bool_not = unary(boolean),
    .bool_likely = unary(boolean),

    .dict_pseudo_seed = sig(&.{}, u64_),
    .hasher_finish = sig(&.{hasher}, u64_),
    .hasher_write_bool = hasherWrite(boolean),
    .hasher_write_u8 = hasherWrite(exact(.u8)),
    .hasher_write_u16 = hasherWrite(exact(.u16)),
    .hasher_write_u32 = hasherWrite(exact(.u32)),
    .hasher_write_u64 = hasherWrite(u64_),
    .hasher_write_u128 = hasherWrite(exact(.u128)),
    .hasher_write_i8 = hasherWrite(exact(.i8)),
    .hasher_write_i16 = hasherWrite(exact(.i16)),
    .hasher_write_i32 = hasherWrite(exact(.i32)),
    .hasher_write_i64 = hasherWrite(exact(.i64)),
    .hasher_write_i128 = hasherWrite(exact(.i128)),
    .hasher_write_f32 = hasherWrite(exact(.f32)),
    .hasher_write_f64 = hasherWrite(exact(.f64)),
    .hasher_write_dec = hasherWrite(exact(.dec)),
    .hasher_write_bytes = hasherWrite(bytes),
    .hasher_write_str = hasherWrite(str),

    .crypto_sha256_hash_bytes = sig(&.{bytes}, bytes),
    .crypto_sha256_hasher_empty = sig(&.{}, bytes),
    .crypto_sha256_hasher_write = sig(&.{ bytes, bytes }, bytes),
    .crypto_sha256_hasher_finish = sig(&.{bytes}, bytes),
    .crypto_blake3_hash_bytes = sig(&.{bytes}, bytes),
    .crypto_blake3_hasher_empty = sig(&.{}, bytes),
    .crypto_blake3_hasher_write = sig(&.{ bytes, bytes }, bytes),
    .crypto_blake3_hasher_finish = sig(&.{bytes}, bytes),

    .num_is_eq = compare(.num),
    .num_is_gt = compare(.num),
    .num_is_gte = compare(.num),
    .num_is_lt = compare(.num),
    .num_is_lte = compare(.num),

    .num_negate = unary(.num),
    .num_abs = unary(.num),
    .num_abs_diff = sig(&.{ .num, same(0) }, .{ .unsigned_of = .{ .arg = 0 } }),
    // Lowering commits source arithmetic to a concrete family member.
    .num_plus = .absent,
    .num_minus = .absent,
    .num_times = .absent,
    .num_int_add_wrap = binary(.int),
    .num_int_add_crash_on_overflow = binary(.int_or_dec),
    .num_int_add_overflows = compare(.int),
    .num_int_add_proven_cannot_overflow = binary(.int_or_dec),
    .num_int_sub_wrap = binary(.int),
    .num_int_sub_crash_on_overflow = binary(.int_or_dec),
    .num_int_sub_overflows = compare(.int),
    .num_int_sub_proven_cannot_overflow = binary(.int_or_dec),
    .num_int_mul_wrap = binary(.int),
    .num_int_mul_crash_on_overflow = binary(.int),
    .num_int_mul_overflows = compare(.int),
    .num_int_mul_proven_cannot_overflow = binary(.int),
    .num_float_add = binary(.float),
    .num_float_sub = binary(.float),
    .num_float_mul = binary(.float),
    .dec_mul = binary(exact(.dec)),
    .num_div_by = binary(.num),
    .num_div_by_checked = binary(.int),
    .num_div_trunc_by = binary(.num),
    .num_div_trunc_by_checked = binary(.int),
    .num_rem_by = binary(.num),
    .num_rem_by_checked = binary(.int),
    .num_mod_by = binary(.num),
    .num_mod_by_checked = binary(.int),
    .num_negate_checked = unary(.int),
    .num_abs_checked = unary(.int_or_dec),
    .num_pow = binary(.frac),
    .num_atan2 = binary(.frac),
    .num_sqrt = unary(.frac),
    .num_sin = unary(.frac),
    .num_cos = unary(.frac),
    .num_tan = unary(.frac),
    .num_asin = unary(.frac),
    .num_acos = unary(.frac),
    .num_atan = unary(.frac),
    .num_floor = unary(.float),
    .num_ceiling = unary(.float),
    .f32_to_bits = sig(&.{exact(.f32)}, exact(.u32)),
    .f32_from_bits = sig(&.{exact(.u32)}, exact(.f32)),
    .f64_to_bits = sig(&.{exact(.f64)}, u64_),
    .f64_from_bits = sig(&.{u64_}, exact(.f64)),

    .num_shift_left_by = sig(&.{ .int, exact(.u8) }, same(0)),
    .num_shift_right_by = sig(&.{ .int, exact(.u8) }, same(0)),
    .num_shift_right_zf_by = sig(&.{ .int, exact(.u8) }, same(0)),
    .num_bitwise_and = binary(.int),
    .num_bitwise_or = binary(.int),
    .num_bitwise_xor = binary(.int),
    .num_bitwise_not = unary(.int),
    .num_count_one_bits = sig(&.{.int}, exact(.u8)),
    .num_count_leading_zero_bits = sig(&.{.int}, exact(.u8)),
    .num_count_trailing_zero_bits = sig(&.{.int}, exact(.u8)),
    .num_from_le_bytes_unchecked = sig(&.{ bytes, u64_ }, .int),

    // Bytes, or the vector's own lane integers, indexed in element units.
    .simd_load_16_unchecked = sig(&.{ .{ .any_of = &.{ bytes, listOf(.{ .lane_of = .result }) } }, u64_ }, .vector),
    .simd_store_16_unchecked = sig(&.{ .vector, bytes, u64_ }, same(1)),
    .simd_append_16 = sig(&.{ .vector, bytes }, same(1)),
    .simd_splat = sig(&.{.{ .lane_of = .result }}, .vector),
    .simd_get_lane_unchecked = sig(&.{ .vector, u64_ }, .{ .lane_of = .{ .arg = 0 } }),
    .simd_with_lane_unchecked = sig(&.{ .vector, u64_, .{ .lane_of = .{ .arg = 0 } } }, same(0)),
    .simd_to_u128_bits = sig(&.{.vector}, exact(.u128)),
    .simd_from_u128_bits = sig(&.{exact(.u128)}, .vector),
    .simd_add_wrap = binary(.vector),
    .simd_sub_wrap = binary(.vector),
    .simd_add_sat = binary(.vector),
    .simd_sub_sat = binary(.vector),
    .simd_neg_wrap = unary(.vector),
    .simd_abs_wrap = unary(.vector),
    .simd_min = binary(.vector),
    .simd_max = binary(.vector),
    .simd_abs_diff = binary(.vector),
    .simd_avg_rounded = binary(.vector),
    .simd_mul_wrap = binary(.vector),
    .simd_mul_high = binary(.vector),
    .simd_mul_q15_sat = binary(exact(.i16x8)),
    .simd_mul_wide_lo = sig(&.{ .vector, same(0) }, .{ .widened_of = .{ .arg = 0 } }),
    .simd_mul_wide_hi = sig(&.{ .vector, same(0) }, .{ .widened_of = .{ .arg = 0 } }),
    .simd_dot_pairs = sig(&.{ exact(.i16x8), same(0) }, exact(.i32x4)),
    .simd_dot_pairs_sat = sig(&.{ exact(.u8x16), exact(.i8x16) }, exact(.i16x8)),
    .simd_sad = sig(&.{ exact(.u8x16), same(0) }, exact(.u64x2)),
    .simd_and = binary(.vector),
    .simd_or = binary(.vector),
    .simd_xor = binary(.vector),
    .simd_not = unary(.vector),
    .simd_bit_select = sig(&.{ .vector, same(0), same(0) }, same(0)),
    .simd_eq_lanes = binary(.vector),
    .simd_gt_lanes = binary(.vector),
    .simd_gte_lanes = binary(.vector),
    .simd_bitmask = sig(&.{.vector}, .{ .bitmask_of = .{ .arg = 0 } }),
    .simd_shl_wrap = sig(&.{ .vector, exact(.u8) }, same(0)),
    .simd_shr_wrap = sig(&.{ .vector, exact(.u8) }, same(0)),
    .simd_shr_zf_wrap = sig(&.{ .vector, exact(.u8) }, same(0)),
    .simd_shr_rounded = sig(&.{ .vector, exact(.u8) }, same(0)),
    .simd_interleave_lo = binary(.vector),
    .simd_interleave_hi = binary(.vector),
    .simd_even_lanes = binary(.vector),
    .simd_odd_lanes = binary(.vector),
    .simd_reverse_lanes = unary(.vector),
    .simd_table_lookup = binary(exact(.u8x16)),
    .simd_concat_shift_bytes = sig(&.{ exact(.u8x16), same(0), exact(.u8) }, same(0)),
    .simd_widen_lo = sig(&.{.vector}, .{ .widened_of = .{ .arg = 0 } }),
    .simd_widen_hi = sig(&.{.vector}, .{ .widened_of = .{ .arg = 0 } }),
    .simd_pairwise_add_widen = sig(&.{.vector}, .{ .widened_of = .{ .arg = 0 } }),
    .simd_narrow_wrap = sig(&.{ .vector, same(0) }, .{ .narrowed_of = .{ .arg = 0 } }),
    .simd_narrow_sat = sig(&.{ .vector, same(0) }, .{ .narrowed_of = .{ .arg = 0 } }),
    .simd_sum_lanes = sig(&.{.vector}, .{ .lane_sum_of = .{ .arg = 0 } }),
    .simd_sum_lanes_wrap = sig(&.{.vector}, .{ .lane_of = .{ .arg = 0 } }),
    .simd_clmul_lo = binary(exact(.u64x2)),
    .simd_clmul_hi = binary(exact(.u64x2)),

    .dec_to_attos = sig(&.{exact(.dec)}, exact(.i128)),
    .dec_from_attos = sig(&.{exact(.i128)}, exact(.dec)),

    .box_box = sig(&.{.any}, .{ .box = &same(0) }),
    .box_unbox = sig(&.{.{ .box = &.any }}, inner(0)),
    .box_unbox_borrowed = sig(&.{.{ .box = &.any }}, inner(0)),
    .box_prepare_update = unary(.{ .box = &.any }),
    // The capture pointer, then optionally the callable whose allocation
    // holds the captures, which keeps it alive through the copy.
    .erased_capture_load = .{ .present = .{ .args = &.{.any}, .optional_args = &.{.any}, .result = .any } },
    .ptr_alloca = sig(&.{}, .{ .ptr = &.any }),
    .box_alloc_zeroed = sig(&.{}, .{ .box = &.any }),
    .ptr_store = sig(&.{ .{ .any_of = &.{ .{ .box = &.any }, .{ .ptr = &.any } } }, inner(0) }, exact(.zst)),
    .ptr_load = sig(&.{.{ .ptr = &.any }}, inner(0)),
    .ptr_cast = sig(&.{.{ .any_of = &.{ .{ .box = &.any }, .{ .ptr = &.any } } }}, .{ .ptr = &inner(0) }),

    // `[After, Before, Same]`
    .compare = sig(&.{ .int_or_dec, same(0) }, .tag_union),
    // Lowering turns a crash into a `crash` statement.
    .crash = .absent,
};

test "every operation has a signature" {
    for (std.enums.values(LowLevel)) |op| _ = of(op);
}
