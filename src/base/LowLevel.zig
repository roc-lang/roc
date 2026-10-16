//! Shared canonical primitive-op vocabulary for canonicalization and LIR.
//!
//! This is the single source of truth for primitive names. Backends may still
//! reject specific ops that should have been lowered away earlier, but there is
//! no separate semantic/backend enum pair.

const std = @import("std");
const numeric_conversion = @import("numeric_conversion.zig");

/// Canonical primitive operations shared across canonicalization and LIR/codegen.
pub const LowLevel = enum(u16) {
    // String operations
    str_is_eq,
    str_is_eq_static_small,
    str_static_small_word_eq,
    str_static_small_word_caseless_eq,
    str_concat,
    str_contains,
    str_trim,
    str_trim_start,
    str_trim_end,
    str_caseless_ascii_equals,
    str_with_ascii_lowercased,
    str_with_ascii_uppercased,
    str_starts_with,
    str_ends_with,
    str_repeat,
    str_drop_prefix,
    str_drop_prefix_caseless_ascii,
    str_drop_suffix,
    str_split_first,
    str_split_last,
    str_count_utf8_bytes,
    str_get_utf8_byte_unsafe,
    str_substring_unsafe,
    str_with_capacity,
    str_reserve,
    str_release_excess_capacity,
    str_to_utf8,
    str_from_utf8_lossy,
    str_from_utf8_validated,
    str_from_utf16_le_short,
    str_from_utf16_be_short,
    str_from_utf32_le_short,
    str_from_utf32_be_short,
    str_from_utf8,
    str_split_on,
    str_join_with,
    str_inspect,

    // Numeric to_str operations
    u8_to_str,
    i8_to_str,
    u16_to_str,
    i16_to_str,
    u32_to_str,
    i32_to_str,
    u64_to_str,
    i64_to_str,
    u128_to_str,
    i128_to_str,
    dec_to_str,
    f32_to_str,
    f64_to_str,

    // List operations
    list_len,
    list_capacity,
    list_get_unsafe,
    /// `List.prefetched`: the same list, with a hint that the item at an
    /// index is about to be read or written. LIR lowering splits it into
    /// `list_prefetch` on the list and an alias of the list, so no later
    /// stage sees this operation.
    list_prefetched,
    /// The hint half of `list_prefetched`, created only by LIR lowering. It
    /// reads nothing and changes nothing; a backend with no prefetch
    /// instruction emits no code for it, and an index outside the list is
    /// harmless.
    list_prefetch,
    list_append_unsafe,
    list_concat,
    list_append_range_within,
    list_copy_range_within,
    list_append_range_within_unsafe,
    list_append_sublist,
    list_append_le_bytes,
    list_slack_unique,
    list_owned_unique,
    list_set_in_place_unsafe,
    list_with_capacity,
    list_drop_at,
    list_sublist,
    list_sublist_borrowed,
    list_set,
    list_replace_unsafe,
    list_swap,
    list_prepend,
    list_take_first,
    list_take_last,
    list_reverse,
    list_reserve,
    list_reserve_for_append,
    list_release_excess_capacity,
    /// `List.clear`: the list with every item removed, keeping its
    /// allocation and capacity when it is uniquely owned. Every backend
    /// runs it as a zero-length sublist from index zero, whose result is
    /// never a slice, so unlike `list_sublist` the result is born unique.
    list_clear,
    list_map_prepare_reuse,
    list_map_can_reuse,
    list_map_cast_unsafe,
    list_map_extract_unsafe,
    list_map_write_unsafe,
    /// Whether the list's item layout occupies at most the given number of
    /// bytes: 1 or 0. A `match` on it with a literal bound folds at lowering
    /// (see `layoutResolved`); anywhere else a backend answers it from the
    /// item layout it builds for.
    list_item_size_at_most,

    // Bool operations
    bool_not,
    /// The same Bool, marking the branch it decides as the one control is
    /// expected to take. `if` lowering erases it into the switch's cold
    /// default arm; anywhere else it is the identity.
    bool_likely,

    // Hasher operations
    dict_pseudo_seed,
    hasher_finish,
    hasher_write_bool,
    hasher_write_u8,
    hasher_write_u16,
    hasher_write_u32,
    hasher_write_u64,
    hasher_write_u128,
    hasher_write_i8,
    hasher_write_i16,
    hasher_write_i32,
    hasher_write_i64,
    hasher_write_i128,
    hasher_write_f32,
    hasher_write_f64,
    hasher_write_dec,
    hasher_write_bytes,
    hasher_write_str,

    // Crypto operations
    crypto_sha256_hash_bytes,
    crypto_sha256_hasher_empty,
    crypto_sha256_hasher_write,
    crypto_sha256_hasher_finish,
    crypto_blake3_hash_bytes,
    crypto_blake3_hasher_empty,
    crypto_blake3_hasher_write,
    crypto_blake3_hasher_finish,

    // Numeric comparison operations
    num_is_eq,
    num_is_gt,
    num_is_gte,
    num_is_lt,
    num_is_lte,

    // Numeric arithmetic operations
    num_negate,
    num_abs,
    num_abs_diff,
    num_plus,
    num_minus,
    num_times,
    num_int_add_wrap,
    num_int_add_crash_on_overflow,
    num_int_add_overflows,
    num_int_add_proven_cannot_overflow,
    num_int_sub_wrap,
    num_int_sub_crash_on_overflow,
    num_int_sub_overflows,
    num_int_sub_proven_cannot_overflow,
    num_int_mul_wrap,
    num_int_mul_crash_on_overflow,
    num_int_mul_overflows,
    num_int_mul_proven_cannot_overflow,
    num_float_add,
    num_float_sub,
    num_float_mul,
    dec_mul,
    num_div_by,
    num_div_by_checked,
    num_div_trunc_by,
    num_div_trunc_by_checked,
    num_rem_by,
    num_rem_by_checked,
    num_mod_by,
    num_mod_by_checked,
    num_negate_checked,
    num_abs_checked,
    num_pow,
    num_atan2,
    num_sqrt,
    num_sin,
    num_cos,
    num_tan,
    num_asin,
    num_acos,
    num_atan,
    num_floor,
    num_ceiling,
    f32_to_bits,
    f32_from_bits,
    f64_to_bits,
    f64_from_bits,

    // Bitwise shift operations
    num_shift_left_by,
    num_shift_right_by,
    num_shift_right_zf_by,

    // Bitwise logical operations
    num_bitwise_and,
    num_bitwise_or,
    num_bitwise_xor,
    num_bitwise_not,

    // Bit-counting operations (integer types only). Each takes an integer
    // operand and returns a U8, regardless of the operand's width.
    num_count_one_bits,
    num_count_leading_zero_bits,
    num_count_trailing_zero_bits,

    // Read a little-endian integer out of a byte list. The result layout
    // carries the width, so a single low-level op serves every multi-byte
    // integer type. The bounds check lives in the Roc wrapper, so this op
    // requires the caller to have proven the bytes are in range.
    num_from_le_bytes_unchecked,

    // Fixed-width integer SIMD operations. Lane width and signedness are
    // carried by the operand/result layouts; these operations never encode a
    // concrete vector type in their identity.
    // Load 16 bytes from a list, indexed in its element units. The input list
    // layout commits the stride (bytes or typed integer lanes).
    simd_load_16_unchecked,
    simd_store_16_unchecked,
    simd_append_16,
    simd_splat,
    simd_get_lane_unchecked,
    simd_with_lane_unchecked,
    simd_to_u128_bits,
    simd_from_u128_bits,
    simd_add_wrap,
    simd_sub_wrap,
    simd_add_sat,
    simd_sub_sat,
    simd_neg_wrap,
    simd_abs_wrap,
    simd_min,
    simd_max,
    simd_abs_diff,
    simd_avg_rounded,
    simd_mul_wrap,
    simd_mul_high,
    simd_mul_q15_sat,
    simd_mul_wide_lo,
    simd_mul_wide_hi,
    simd_dot_pairs,
    simd_dot_pairs_sat,
    simd_sad,
    simd_and,
    simd_or,
    simd_xor,
    simd_not,
    simd_bit_select,
    simd_eq_lanes,
    simd_gt_lanes,
    simd_gte_lanes,
    simd_bitmask,
    simd_shl_wrap,
    simd_shr_wrap,
    simd_shr_zf_wrap,
    simd_shr_rounded,
    simd_interleave_lo,
    simd_interleave_hi,
    simd_even_lanes,
    simd_odd_lanes,
    simd_reverse_lanes,
    simd_table_lookup,
    simd_concat_shift_bytes,
    simd_widen_lo,
    simd_widen_hi,
    simd_pairwise_add_widen,
    simd_narrow_wrap,
    simd_narrow_sat,
    simd_sum_lanes,
    simd_sum_lanes_wrap,
    simd_clmul_lo,
    simd_clmul_hi,
    // Numeric parsing operations
    u8_from_str,
    i8_from_str,
    u16_from_str,
    i16_from_str,
    u32_from_str,
    i32_from_str,
    u64_from_str,
    i64_from_str,
    u128_from_str,
    i128_from_str,
    dec_from_str,
    dec_to_attos,
    dec_from_attos,
    f32_from_str,
    f64_from_str,
    // Prefix numeric parsing: parse the longest numeric prefix and return
    // `{ err : U8, rest : Str | List(U8), value : T }`.
    u8_from_str_prefix,
    u8_from_utf8_prefix,
    i8_from_str_prefix,
    i8_from_utf8_prefix,
    u16_from_str_prefix,
    u16_from_utf8_prefix,
    i16_from_str_prefix,
    i16_from_utf8_prefix,
    u32_from_str_prefix,
    u32_from_utf8_prefix,
    i32_from_str_prefix,
    i32_from_utf8_prefix,
    u64_from_str_prefix,
    u64_from_utf8_prefix,
    i64_from_str_prefix,
    i64_from_utf8_prefix,
    u128_from_str_prefix,
    u128_from_utf8_prefix,
    i128_from_str_prefix,
    i128_from_utf8_prefix,
    dec_from_str_prefix,
    dec_from_utf8_prefix,
    f32_from_str_prefix,
    f32_from_utf8_prefix,
    f64_from_str_prefix,
    f64_from_utf8_prefix,

    // Numeric conversion operations (U8)
    u8_to_i8_wrap,
    u8_to_i8_try,
    u8_to_i16,
    u8_to_i32,
    u8_to_i64,
    u8_to_i128,
    u8_to_u16,
    u8_to_u32,
    u8_to_u64,
    u8_to_u128,
    u8_to_f32,
    u8_to_f64,
    u8_to_dec,

    // Numeric conversion operations (I8)
    i8_to_i16,
    i8_to_i32,
    i8_to_i64,
    i8_to_i128,
    i8_to_u8_wrap,
    i8_to_u8_try,
    i8_to_u16_wrap,
    i8_to_u16_try,
    i8_to_u32_wrap,
    i8_to_u32_try,
    i8_to_u64_wrap,
    i8_to_u64_try,
    i8_to_u128_wrap,
    i8_to_u128_try,
    i8_to_f32,
    i8_to_f64,
    i8_to_dec,

    // Numeric conversion operations (U16)
    u16_to_i8_wrap,
    u16_to_i8_try,
    u16_to_i16_wrap,
    u16_to_i16_try,
    u16_to_i32,
    u16_to_i64,
    u16_to_i128,
    u16_to_u8_wrap,
    u16_to_u8_try,
    u16_to_u32,
    u16_to_u64,
    u16_to_u128,
    u16_to_f32,
    u16_to_f64,
    u16_to_dec,

    // Numeric conversion operations (I16)
    i16_to_i8_wrap,
    i16_to_i8_try,
    i16_to_i32,
    i16_to_i64,
    i16_to_i128,
    i16_to_u8_wrap,
    i16_to_u8_try,
    i16_to_u16_wrap,
    i16_to_u16_try,
    i16_to_u32_wrap,
    i16_to_u32_try,
    i16_to_u64_wrap,
    i16_to_u64_try,
    i16_to_u128_wrap,
    i16_to_u128_try,
    i16_to_f32,
    i16_to_f64,
    i16_to_dec,

    // Numeric conversion operations (U32)
    u32_to_i8_wrap,
    u32_to_i8_try,
    u32_to_i16_wrap,
    u32_to_i16_try,
    u32_to_i32_wrap,
    u32_to_i32_try,
    u32_to_i64,
    u32_to_i128,
    u32_to_u8_wrap,
    u32_to_u8_try,
    u32_to_u16_wrap,
    u32_to_u16_try,
    u32_to_u64,
    u32_to_u128,
    u32_to_f32,
    u32_to_f64,
    u32_to_dec,

    // Numeric conversion operations (I32)
    i32_to_i8_wrap,
    i32_to_i8_try,
    i32_to_i16_wrap,
    i32_to_i16_try,
    i32_to_i64,
    i32_to_i128,
    i32_to_u8_wrap,
    i32_to_u8_try,
    i32_to_u16_wrap,
    i32_to_u16_try,
    i32_to_u32_wrap,
    i32_to_u32_try,
    i32_to_u64_wrap,
    i32_to_u64_try,
    i32_to_u128_wrap,
    i32_to_u128_try,
    i32_to_f32,
    i32_to_f64,
    i32_to_dec,

    // Numeric conversion operations (U64)
    u64_to_i8_wrap,
    u64_to_i8_try,
    u64_to_i16_wrap,
    u64_to_i16_try,
    u64_to_i32_wrap,
    u64_to_i32_try,
    u64_to_i64_wrap,
    u64_to_i64_try,
    u64_to_i128,
    u64_to_u8_wrap,
    u64_to_u8_try,
    u64_to_u16_wrap,
    u64_to_u16_try,
    u64_to_u32_wrap,
    u64_to_u32_try,
    u64_to_u128,
    u64_to_f32,
    u64_to_f64,
    u64_to_dec,

    // Numeric conversion operations (I64)
    i64_to_i8_wrap,
    i64_to_i8_try,
    i64_to_i16_wrap,
    i64_to_i16_try,
    i64_to_i32_wrap,
    i64_to_i32_try,
    i64_to_i128,
    i64_to_u8_wrap,
    i64_to_u8_try,
    i64_to_u16_wrap,
    i64_to_u16_try,
    i64_to_u32_wrap,
    i64_to_u32_try,
    i64_to_u64_wrap,
    i64_to_u64_try,
    i64_to_u128_wrap,
    i64_to_u128_try,
    i64_to_f32,
    i64_to_f64,
    i64_to_dec,

    // Numeric conversion operations (U128)
    u128_to_i8_wrap,
    u128_to_i8_try,
    u128_to_i16_wrap,
    u128_to_i16_try,
    u128_to_i32_wrap,
    u128_to_i32_try,
    u128_to_i64_wrap,
    u128_to_i64_try,
    u128_to_i128_wrap,
    u128_to_i128_try,
    u128_to_u8_wrap,
    u128_to_u8_try,
    u128_to_u16_wrap,
    u128_to_u16_try,
    u128_to_u32_wrap,
    u128_to_u32_try,
    u128_to_u64_wrap,
    u128_to_u64_try,
    u128_to_f32,
    u128_to_f64,
    u128_to_dec_try_unsafe,

    // Numeric conversion operations (I128)
    i128_to_i8_wrap,
    i128_to_i8_try,
    i128_to_i16_wrap,
    i128_to_i16_try,
    i128_to_i32_wrap,
    i128_to_i32_try,
    i128_to_i64_wrap,
    i128_to_i64_try,
    i128_to_u8_wrap,
    i128_to_u8_try,
    i128_to_u16_wrap,
    i128_to_u16_try,
    i128_to_u32_wrap,
    i128_to_u32_try,
    i128_to_u64_wrap,
    i128_to_u64_try,
    i128_to_u128_wrap,
    i128_to_u128_try,
    i128_to_f32,
    i128_to_f64,
    i128_to_dec_try_unsafe,

    // Numeric conversion operations (F32)
    f32_to_i8_trunc,
    f32_to_i8_try_unsafe,
    f32_to_i16_trunc,
    f32_to_i16_try_unsafe,
    f32_to_i32_trunc,
    f32_to_i32_try_unsafe,
    f32_to_i64_trunc,
    f32_to_i64_try_unsafe,
    f32_to_i128_trunc,
    f32_to_i128_try_unsafe,
    f32_to_u8_trunc,
    f32_to_u8_try_unsafe,
    f32_to_u16_trunc,
    f32_to_u16_try_unsafe,
    f32_to_u32_trunc,
    f32_to_u32_try_unsafe,
    f32_to_u64_trunc,
    f32_to_u64_try_unsafe,
    f32_to_u128_trunc,
    f32_to_u128_try_unsafe,
    f32_to_f64,

    // Numeric conversion operations (F64)
    f64_to_i8_trunc,
    f64_to_i8_try_unsafe,
    f64_to_i16_trunc,
    f64_to_i16_try_unsafe,
    f64_to_i32_trunc,
    f64_to_i32_try_unsafe,
    f64_to_i64_trunc,
    f64_to_i64_try_unsafe,
    f64_to_i128_trunc,
    f64_to_i128_try_unsafe,
    f64_to_u8_trunc,
    f64_to_u8_try_unsafe,
    f64_to_u16_trunc,
    f64_to_u16_try_unsafe,
    f64_to_u32_trunc,
    f64_to_u32_try_unsafe,
    f64_to_u64_trunc,
    f64_to_u64_try_unsafe,
    f64_to_u128_trunc,
    f64_to_u128_try_unsafe,
    f64_to_f32_wrap,
    f64_to_f32_try_unsafe,

    // Numeric conversion operations (Dec)
    dec_to_i8_trunc,
    dec_to_i8_try_unsafe,
    dec_to_i16_trunc,
    dec_to_i16_try_unsafe,
    dec_to_i32_trunc,
    dec_to_i32_try_unsafe,
    dec_to_i64_trunc,
    dec_to_i64_try_unsafe,
    dec_to_i128_trunc,
    dec_to_u8_trunc,
    dec_to_u8_try_unsafe,
    dec_to_u16_trunc,
    dec_to_u16_try_unsafe,
    dec_to_u32_trunc,
    dec_to_u32_try_unsafe,
    dec_to_u64_trunc,
    dec_to_u64_try_unsafe,
    dec_to_u128_trunc,
    dec_to_u128_try_unsafe,
    dec_to_f32_wrap,
    dec_to_f32_try_unsafe,
    dec_to_f64,

    // Box operations
    box_box,
    box_unbox,
    /// ARC-only borrowing variant of box_unbox.
    box_unbox_borrowed,
    /// Box(T) -> Box(T): consume a box and return a unique box containing the
    /// same payload. Reuses the allocation when uniqueness is known or the
    /// runtime check succeeds; otherwise copies the payload into a fresh box,
    /// retains nested payload children, and releases the consumed input.
    box_prepare_update,
    erased_capture_load,

    // Compiler-internal pointer operations, introduced by the TRMC pass
    // (src/lir/trmc.zig). Never produced by user code or canonicalization.
    // Sizes always come from local layouts: the target local for ptr_alloca /
    // box_alloc_zeroed (inner of ptr/box) and ptr_load, the value arg for
    // ptr_store.
    /// () -> Ptr(T): reserve a zeroed stack/frame slot for T, yield its address.
    /// Emitted once per proc entry (pre-loop); backends may hoist to the prologue.
    ptr_alloca,
    /// () -> Box(T): heap cell via allocateWithRefcount (rc=1), payload zero-filled.
    /// Bit-identical to a box_box whose payload is all zeroes.
    box_alloc_zeroed,
    /// (Box(T) | Ptr(T), T) -> {}: copy sizeOf(T) bytes from the value into
    /// the pointed-at payload. A Box address is borrowed for the store, so the
    /// store keeps its allocation alive.
    ptr_store,
    /// (Ptr(T)) -> T: copy sizeOf(T) bytes out of *ptr.
    ptr_load,
    /// (Box(T) | Ptr(T)) -> Ptr(T): identity bits.
    ptr_cast,

    // Comparison
    compare,

    // Crash/panic
    crash,

    /// Index into `builtins.simd.Op`. The SIMD entries are intentionally one
    /// contiguous, order-pinned block so the interpreter and compile-time
    /// evaluator share the semantic oracle's operation vocabulary without a
    /// module cycle.
    pub fn simdOpIndex(self: LowLevel) ?u8 {
        const raw = @backingInt(self);
        const first = @backingInt(LowLevel.simd_load_16_unchecked);
        const last = @backingInt(LowLevel.simd_clmul_hi);
        if (raw < first or raw > last) return null;
        return @intCast(raw - first);
    }

    /// Reference-counting behavior exposed by this primitive before LIR ARC
    /// insertion. This is explicit primitive metadata, not backend policy.
    ///
    /// ## What a row promises
    ///
    /// Every row is a claim about the Zig builtin the op lowers to, and ARC
    /// generates code that is correct only if the claim is true. Writing a row
    /// means discharging these obligations for the implementation, not
    /// pattern-matching a neighboring row:
    ///
    /// - `may_allocate`: the op may call `allocateWithRefcount`. A row that
    ///   omits it promises the op never births an allocation.
    /// - `may_retain_or_release`: the op may change some allocation's count.
    ///   A row that omits it promises every count the op can reach is
    ///   untouched.
    /// - `may_runtime_uniqueness_check_args`: the op reads those arguments'
    ///   counts to choose between mutating in place and copying. ARC may prove
    ///   the check redundant and pass `unique_args`, which lets the builtin
    ///   take the in-place path unconditionally—so a named position must be
    ///   one the op consumes, and the in-place path must be sound whenever the
    ///   argument really is unique.
    /// - `consume_args`: the op takes one ownership unit of those arguments.
    ///   ARC stops accounting for them at this statement, so the op must
    ///   release each one (or move it into the result) on every path.
    /// - `result_aliases_consumed_args`: the unit taken from those consumed
    ///   arguments lives on in the result. Only consumed positions may appear.
    /// - `retain_args`: the op adds one count to those arguments—typically
    ///   because it stores a handle to them inside the result. ARC emits no
    ///   retain of its own, so an op that declares this and does not retain
    ///   leaves the stored handle undercounted.
    /// - `retain_result`: the result aliases storage that stays live (an
    ///   element, box payload, capture, or borrowed view), so ARC retains it
    ///   after the op rather than treating it as freshly owned.
    /// - `result_borrows_args`: the result points into those arguments'
    ///   payloads without owning them. ARC keeps the lender live across every
    ///   use of the result instead of retaining the result.
    /// - `result_shares_args`: the result is a fresh owned outer value whose
    ///   interior shares those arguments' allocations (seamless slices, byte
    ///   reinterpretations). Host-visibility analysis links result and
    ///   argument in both directions.
    /// - `result_unique`: the result's outermost allocation has count 1 on
    ///   return, so ARC records a birth. A result that shares an argument's
    ///   outer allocation is never unique; interior sharing is irrelevant.
    ///
    /// ## What checks the claims
    ///
    /// - `base/rc_effect_rules.zig` rejects rows whose fields contradict each
    ///   other, over the whole table, at comptime.
    /// - `eval/rc_conformance.zig` runs each op through the interpreter and
    ///   compares the refcount traffic it actually produces against the row.
    pub const RcEffect = struct {
        may_allocate: bool = false,
        may_retain_or_release: bool = false,
        may_runtime_uniqueness_check_args: u64 = 0,
        consume_args: u64 = 0,
        result_aliases_consumed_args: u64 = 0,
        retain_args: u64 = 0,
        retain_result: bool = false,
        /// Argument positions whose payload data the result may point into
        /// without owning it. When ARC insertion solves the result's binding
        /// as borrowed, it emits no retain for the result and instead keeps
        /// the lender argument live across every use of the result.
        result_borrows_args: u64 = 0,
        /// Argument positions whose allocations the result may hold handles
        /// into even though unit accounting says nothing: the op returns a
        /// fresh owned outer value whose interior shares the argument's
        /// allocation (seamless slices, byte/string reinterpretations).
        /// Host-visibility analysis links the result to these arguments in
        /// both directions.
        result_shares_args: u64 = 0,
        /// The result's outermost allocation has count 1 on return. Runtime
        /// uniqueness-checking ops qualify on both of their paths (in place
        /// keeps an allocation whose count was already 1, the copy path
        /// returns a fresh one), as do ops that always allocate their
        /// outermost result—interior sharing described by
        /// `result_shares_args` is irrelevant to the outermost count.
        result_unique: bool = false,

        pub fn none() RcEffect {
            return .{};
        }

        pub fn allocates() RcEffect {
            return .{
                .may_allocate = true,
                .result_unique = true,
            };
        }

        pub fn allocatesRetainingArgs(mask: u64) RcEffect {
            return .{
                .may_allocate = true,
                .may_retain_or_release = mask != 0,
                .retain_args = mask,
                .result_unique = true,
            };
        }

        pub fn allocatesConsumingArgs(mask: u64) RcEffect {
            return .{
                .may_allocate = true,
                .may_retain_or_release = mask != 0,
                .consume_args = mask,
                .result_unique = true,
            };
        }

        pub fn retainsResult() RcEffect {
            return .{
                .may_retain_or_release = true,
                .retain_result = true,
            };
        }

        pub fn retainsResultBorrowingArgs(mask: u64) RcEffect {
            return .{
                .may_retain_or_release = true,
                .retain_result = true,
                .result_borrows_args = mask,
            };
        }

        pub fn runtimeUniqueness(mask: u64) RcEffect {
            return .{
                .may_allocate = true,
                .may_retain_or_release = true,
                .may_runtime_uniqueness_check_args = mask,
                .consume_args = mask,
                .result_aliases_consumed_args = mask,
                .result_unique = true,
            };
        }

        pub fn runtimeUniquenessMaybeSharedResult(mask: u64) RcEffect {
            return .{
                .may_allocate = true,
                .may_retain_or_release = true,
                .may_runtime_uniqueness_check_args = mask,
                .consume_args = mask,
                .result_aliases_consumed_args = mask,
            };
        }

        pub fn runtimeUniquenessRetainingArgs(runtime_mask: u64, retain_mask: u64) RcEffect {
            return .{
                .may_allocate = true,
                .may_retain_or_release = true,
                .may_runtime_uniqueness_check_args = runtime_mask,
                .consume_args = runtime_mask,
                .result_aliases_consumed_args = runtime_mask,
                .retain_args = retain_mask,
                .result_unique = true,
            };
        }

        pub fn retainsOrReleasesSharingArgs(mask: u64) RcEffect {
            return .{
                .may_retain_or_release = true,
                .result_shares_args = mask,
            };
        }

        pub fn retainsSharingArgs(mask: u64) RcEffect {
            return .{
                .may_retain_or_release = mask != 0,
                .retain_args = mask,
                .result_shares_args = mask,
            };
        }

        pub fn allocatesAndRetainsOrReleasesSharingArgs(mask: u64) RcEffect {
            return .{
                .may_allocate = true,
                .may_retain_or_release = true,
                .result_shares_args = mask,
            };
        }

        pub fn consumesArgsRetainingArgs(consume_mask: u64, retain_mask: u64) RcEffect {
            return .{
                .may_retain_or_release = consume_mask != 0 or retain_mask != 0,
                .consume_args = consume_mask,
                .retain_args = retain_mask,
            };
        }

        pub fn consumesArgsReturningConsumedArgsRetainingArgs(consume_mask: u64, retain_mask: u64) RcEffect {
            return .{
                .may_retain_or_release = consume_mask != 0 or retain_mask != 0,
                .consume_args = consume_mask,
                .result_aliases_consumed_args = consume_mask,
                .retain_args = retain_mask,
                .result_unique = true,
            };
        }

        /// Move consumed ownership units into the result without claiming that
        /// their allocations are unique.
        pub fn consumesArgsReturningConsumedArgs(mask: u64) RcEffect {
            return .{
                .may_retain_or_release = mask != 0,
                .consume_args = mask,
                .result_aliases_consumed_args = mask,
            };
        }
    };

    /// The operations whose answer LIR lowering decides from committed
    /// layouts, so a `match` on one can keep only the arm that answer selects
    /// (design.md "In-Place List Transforms").
    pub const LayoutResolved = enum {
        list_map_can_reuse,
        list_item_size_at_most,
    };

    /// This operation as a layout-resolved one, or null for every other.
    pub fn layoutResolved(self: LowLevel) ?LayoutResolved {
        if (self == .list_map_can_reuse) return .list_map_can_reuse;
        if (self == .list_item_size_at_most) return .list_item_size_at_most;
        return null;
    }

    /// Operands read solely from their by-value representation. These reads
    /// need the saved descriptor, but never its backing allocation. Other
    /// operands retain their ordinary allocation-lifetime requirements.
    /// Kept as static operation data rather than widening every LIR statement.
    pub fn representationArgs(self: LowLevel) u64 {
        if (self == .list_len or self == .list_capacity or self == .list_prefetch) return argMask(&.{0});
        return 0;
    }

    /// Whether a Builtin procedure whose body is exactly this operation over its
    /// parameters is identified by its argument and result layouts instead of
    /// its types. This is the explicit allow list of design.md "Layout-Keyed
    /// Builtin Procedures": each member's lowering to LIR reads only the
    /// layouts of its operands and result, so every specialization of its
    /// wrapper whose closed request types commit the same layouts emits the
    /// same procedure. An operation whose lowering reads a solved type (for
    /// example `list_map_can_reuse`, which inspects its transform's function
    /// type) or program-wide lowering state (`dict_pseudo_seed`) is not a
    /// member, and neither is any operation whose meaning depends on anything
    /// but its operands' runtime representation.
    pub fn procedureKeyedByLayout(self: LowLevel) bool {
        return op_info[@backingInt(self)].keyed_by_layout;
    }

    /// Return the explicit RC metadata for this primitive. The masks identify
    /// argument positions whose refcount may be inspected for copy-on-write.
    pub fn rcEffect(self: LowLevel) RcEffect {
        return op_info[@backingInt(self)].rc_effect;
    }

    /// The per-operation properties behind `procedureKeyedByLayout` and
    /// `rcEffect`.
    const OpInfo = struct {
        keyed_by_layout: bool,
        rc_effect: RcEffect,
    };

    /// An operation on `procedureKeyedByLayout`'s allow list.
    fn byLayout(rc_effect: RcEffect) OpInfo {
        return .{ .keyed_by_layout = true, .rc_effect = rc_effect };
    }

    /// An operation whose Builtin procedures are identified by their types.
    fn byType(rc_effect: RcEffect) OpInfo {
        return .{ .keyed_by_layout = false, .rc_effect = rc_effect };
    }

    /// The properties of every operation that converts between two machine
    /// number types, which `numeric_conversion.getConversionSpec` classifies
    /// from the operation's name at comptime. Its operands and result are
    /// scalars, so there is no refcount for it to touch.
    const numeric_conversion_info = byType(.none());

    /// Every operation's `OpInfo`, indexed by enum value.
    const op_info = blk: {
        @setEvalBranchQuota(1 << 14);
        const ops = std.enums.values(LowLevel);
        var infos: [ops.len]OpInfo = undefined;
        for (ops) |op| {
            infos[@backingInt(op)] = if (numeric_conversion.getConversionSpec(op) != null)
                numeric_conversion_info
            else
                @field(explicit_op_info, @tagName(op));
        }
        break :blk infos;
    };

    /// A struct with one field for each operation that is not a numeric
    /// conversion, so `explicit_op_info` must give every such operation its
    /// own row and cannot give one to a numeric conversion.
    const ExplicitOpInfo = blk: {
        @setEvalBranchQuota(1 << 14);
        var names: []const []const u8 = &.{};
        for (std.enums.values(LowLevel)) |op| {
            if (numeric_conversion.getConversionSpec(op) == null) names = names ++ .{@tagName(op)};
        }
        break :blk @Struct(.auto, null, names, &@splat(OpInfo), &@splat(.{}));
    };

    const arg_0 = argMask(&.{0});
    const arg_1 = argMask(&.{1});
    const arg_2 = argMask(&.{2});

    /// The properties of every operation that is not a numeric conversion.
    const explicit_op_info: ExplicitOpInfo = .{
        .str_concat = byType(.runtimeUniqueness(arg_0)),

        // Trimming a shared string returns a seamless slice of it rather
        // than copying, so the result's outermost allocation is the
        // argument's and its count is whatever the argument's was. Same
        // regime as the list slice ops below.
        .str_trim = byType(.runtimeUniquenessMaybeSharedResult(arg_0)),
        .str_trim_start = byType(.runtimeUniquenessMaybeSharedResult(arg_0)),
        .str_trim_end = byType(.runtimeUniquenessMaybeSharedResult(arg_0)),

        .str_with_ascii_lowercased = byType(.runtimeUniqueness(arg_0)),
        .str_with_ascii_uppercased = byType(.runtimeUniqueness(arg_0)),
        .str_reserve = byType(.runtimeUniqueness(arg_0)),
        .str_release_excess_capacity = byType(.runtimeUniqueness(arg_0)),

        .str_drop_prefix = byType(.retainsSharingArgs(arg_0)),
        .str_drop_suffix = byType(.retainsSharingArgs(arg_0)),
        .str_substring_unsafe = byType(.retainsSharingArgs(arg_0)),

        .str_drop_prefix_caseless_ascii = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .str_split_first = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .str_split_last = byType(.retainsOrReleasesSharingArgs(arg_0)),

        // Prefix number parsing borrows its input and returns `rest` as an
        // owned seamless slice of it: the builtin retains the source
        // allocation itself, exactly like `str_drop_prefix_caseless_ascii`.
        .u8_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .u8_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .i8_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .i8_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .u16_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .u16_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .i16_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .i16_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .u32_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .u32_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .i32_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .i32_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .u64_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .u64_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .i64_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .i64_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .u128_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .u128_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .i128_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .i128_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .dec_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .dec_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .f32_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .f32_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .f64_from_str_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),
        .f64_from_utf8_prefix = byType(.retainsOrReleasesSharingArgs(arg_0)),

        .str_from_utf8 = byType(.retainsOrReleasesSharingArgs(arg_0)),

        // Valid UTF-8 retains and shares the borrowed list allocation;
        // invalid UTF-8 allocates replacement bytes. Neither path consumes
        // the list, and a shared result is not born unique.
        .str_from_utf8_lossy = byType(.allocatesAndRetainsOrReleasesSharingArgs(arg_0)),

        .str_to_utf8 = byType(.allocatesAndRetainsOrReleasesSharingArgs(arg_0)),
        .str_from_utf8_validated = byType(.retainsOrReleasesSharingArgs(arg_0)),

        .list_drop_at = byLayout(.runtimeUniquenessMaybeSharedResult(arg_0)),
        .list_sublist = byLayout(.runtimeUniquenessMaybeSharedResult(arg_0)),
        .list_take_first = byType(.runtimeUniquenessMaybeSharedResult(arg_0)),
        .list_take_last = byType(.runtimeUniquenessMaybeSharedResult(arg_0)),

        .list_sublist_borrowed = byType(.retainsResultBorrowingArgs(arg_0)),

        .list_reverse = byType(.runtimeUniqueness(arg_0)),
        .list_reserve = byLayout(.runtimeUniqueness(arg_0)),
        .list_reserve_for_append = byLayout(.runtimeUniqueness(arg_0)),
        .list_release_excess_capacity = byLayout(.runtimeUniqueness(arg_0)),
        .list_clear = byType(.runtimeUniqueness(arg_0)),

        // SIMD byte stores and appends consume/update their List(U8),
        // which is argument 1 in both primitive signatures.
        .simd_store_16_unchecked = byType(.runtimeUniqueness(arg_1)),
        .simd_append_16 = byType(.runtimeUniqueness(arg_1)),

        .list_prepend = byLayout(.runtimeUniquenessRetainingArgs(arg_0, arg_1)),

        .list_append_unsafe = byLayout(.consumesArgsReturningConsumedArgsRetainingArgs(arg_0, arg_1)),

        // Like `list_append_unsafe`: stores into a list the loop promotion
        // pass proved uniquely owned, so there is nothing to check at
        // runtime. The displaced element is released inside the builtin.
        .list_set_in_place_unsafe = byType(.consumesArgsReturningConsumedArgsRetainingArgs(arg_0, arg_2)),

        // Like `list_append_unsafe`: mutates a list the loop-append
        // promotion pass proved uniquely owned with enough capacity, so
        // there is nothing to check at runtime. Copied refcounted
        // elements gain their references inside the builtin.
        .list_append_range_within_unsafe = byType(.consumesArgsReturningConsumedArgsRetainingArgs(arg_0, 0)),

        // Moves the list's ownership unit into a new local before the
        // reuse query, forcing ARC to preserve every later use first.
        // List.map, List.update, and loop promotion use this transfer.
        .list_map_prepare_reuse = byLayout(.consumesArgsReturningConsumedArgs(arg_0)),

        // The same list; LIR lowering replaces it with an alias, so this
        // describes the source-level operation only.
        .list_prefetched = byType(.consumesArgsReturningConsumedArgs(arg_0)),

        // Reads the prepared list's refcount (and slice bit) without
        // changing it. List.map additionally gates this result on item
        // representation compatibility during lowering.
        .list_map_can_reuse = byType(.none()),
        // The owned flag reads the list's count and nothing else; it is
        // the check a versioned loop dispatches on, so a list proven
        // unique makes it a constant.
        .list_owned_unique = byType(.{ .may_runtime_uniqueness_check_args = arg_0 }),

        // Retypes a unique non-slice list to the output element type,
        // keeping the same allocation. Only reachable behind a true
        // `list_map_can_reuse`, so the result's count is 1 on return.
        .list_map_cast_unsafe = byLayout(.consumesArgsReturningConsumedArgsRetainingArgs(arg_0, 0)),

        // Moves one element's ownership out of a unique list's buffer.
        // The buffer slot keeps stale bytes until `list_map_write_unsafe`
        // stores the replacement; the op itself performs no RC work.
        .list_map_extract_unsafe = byLayout(.none()),

        // Stores an owned element into the slot vacated by
        // `list_map_extract_unsafe`, mirroring `list_append_unsafe`.
        .list_map_write_unsafe = byLayout(.consumesArgsReturningConsumedArgsRetainingArgs(arg_0, arg_2)),

        // Reads the bound and the list's layout, never the list itself; the
        // bound is an operand value, so the operation is not layout-keyed.
        .list_item_size_at_most = byType(.none()),

        .list_swap = byLayout(.runtimeUniqueness(arg_0)),

        .list_set = byLayout(.runtimeUniquenessRetainingArgs(arg_0, arg_2)),
        .list_replace_unsafe = byLayout(.runtimeUniquenessRetainingArgs(arg_0, arg_2)),

        .list_concat = byLayout(.runtimeUniqueness(argMask(&.{ 0, 1 }))),

        // Both appends mutate only their first list; `list_append_sublist`
        // reads its source without consuming or retaining it. The
        // within-list range copy mutates the one list it is given.
        .list_append_range_within = byLayout(.runtimeUniqueness(arg_0)),
        .list_copy_range_within = byLayout(.runtimeUniqueness(arg_0)),
        .list_append_sublist = byLayout(.runtimeUniqueness(arg_0)),
        .list_append_le_bytes = byType(.runtimeUniqueness(arg_0)),

        .list_get_unsafe = byLayout(.retainsResultBorrowingArgs(arg_0)),

        // Allocates the list of segments, and counts the source string
        // once per segment it slices out of it.
        .str_split_on = byType(.allocatesAndRetainsOrReleasesSharingArgs(arg_0)),

        .str_repeat = byType(.allocates()),
        .str_from_utf16_le_short = byType(.allocates()),
        .str_from_utf16_be_short = byType(.allocates()),
        .str_from_utf32_le_short = byType(.allocates()),
        .str_from_utf32_be_short = byType(.allocates()),
        .str_with_capacity = byType(.allocates()),
        .str_inspect = byType(.allocates()),
        .u8_to_str = byType(.allocates()),
        .i8_to_str = byType(.allocates()),
        .u16_to_str = byType(.allocates()),
        .i16_to_str = byType(.allocates()),
        .u32_to_str = byType(.allocates()),
        .i32_to_str = byType(.allocates()),
        .u64_to_str = byType(.allocates()),
        .i64_to_str = byType(.allocates()),
        .u128_to_str = byType(.allocates()),
        .i128_to_str = byType(.allocates()),
        .dec_to_str = byType(.allocates()),
        .f32_to_str = byType(.allocates()),
        .f64_to_str = byType(.allocates()),
        .list_with_capacity = byLayout(.allocates()),
        .crypto_sha256_hash_bytes = byType(.allocates()),
        .crypto_sha256_hasher_empty = byType(.allocates()),
        .crypto_sha256_hasher_write = byType(.allocates()),
        .crypto_sha256_hasher_finish = byType(.allocates()),
        .crypto_blake3_hash_bytes = byType(.allocates()),
        .crypto_blake3_hasher_empty = byType(.allocates()),
        .crypto_blake3_hasher_write = byType(.allocates()),
        .crypto_blake3_hasher_finish = byType(.allocates()),

        .str_join_with = byType(.allocatesConsumingArgs(arg_0)),

        .box_box = byLayout(.allocatesRetainingArgs(arg_0)),

        .box_unbox = byLayout(.{ .may_retain_or_release = true, .consume_args = arg_0, .result_shares_args = arg_0 }),

        .box_unbox_borrowed = byType(.retainsResultBorrowingArgs(arg_0)),

        .box_prepare_update = byType(.runtimeUniqueness(arg_0)),

        // The capture environment is read through the executing frame's
        // closure, not through an explicit refcounted argument, so the
        // result cannot name a lender to borrow from.
        .erased_capture_load = byType(.retainsResult()),

        .box_alloc_zeroed = byType(.allocates()),

        // The stored value's ownership transfers into the pointed-at structure.
        // A ptr address is never refcounted; a Box address is borrowed.
        .ptr_store = byType(.consumesArgsRetainingArgs(arg_1, 0)),

        .ptr_alloca = byType(.none()),
        .ptr_load = byType(.none()),
        .ptr_cast = byType(.none()),

        .str_is_eq = byType(.none()),
        .str_is_eq_static_small = byType(.none()),
        .str_static_small_word_eq = byType(.none()),
        .str_static_small_word_caseless_eq = byType(.none()),
        .str_contains = byType(.none()),
        .str_caseless_ascii_equals = byType(.none()),
        .str_starts_with = byType(.none()),
        .str_ends_with = byType(.none()),
        .str_count_utf8_bytes = byType(.none()),
        .str_get_utf8_byte_unsafe = byType(.none()),
        .list_len = byLayout(.none()),
        .list_capacity = byLayout(.none()),
        .list_prefetch = byLayout(.none()),
        .list_slack_unique = byType(.none()),
        .bool_not = byType(.none()),
        .bool_likely = byType(.none()),
        .dict_pseudo_seed = byType(.none()),
        .hasher_finish = byType(.none()),
        .hasher_write_bool = byType(.none()),
        .hasher_write_u8 = byType(.none()),
        .hasher_write_u16 = byType(.none()),
        .hasher_write_u32 = byType(.none()),
        .hasher_write_u64 = byType(.none()),
        .hasher_write_u128 = byType(.none()),
        .hasher_write_i8 = byType(.none()),
        .hasher_write_i16 = byType(.none()),
        .hasher_write_i32 = byType(.none()),
        .hasher_write_i64 = byType(.none()),
        .hasher_write_i128 = byType(.none()),
        .hasher_write_f32 = byType(.none()),
        .hasher_write_f64 = byType(.none()),
        .hasher_write_dec = byType(.none()),
        .hasher_write_bytes = byType(.none()),
        .hasher_write_str = byType(.none()),
        .num_is_eq = byType(.none()),
        .num_is_gt = byType(.none()),
        .num_is_gte = byType(.none()),
        .num_is_lt = byType(.none()),
        .num_is_lte = byType(.none()),
        .num_negate = byType(.none()),
        .num_negate_checked = byType(.none()),
        .num_abs = byType(.none()),
        .num_abs_checked = byType(.none()),
        .num_abs_diff = byType(.none()),
        .num_plus = byType(.none()),
        .num_minus = byType(.none()),
        .num_times = byType(.none()),
        .num_int_add_wrap = byType(.none()),
        .num_int_add_crash_on_overflow = byType(.none()),
        .num_int_add_overflows = byType(.none()),
        .num_int_add_proven_cannot_overflow = byType(.none()),
        .num_int_sub_wrap = byType(.none()),
        .num_int_sub_crash_on_overflow = byType(.none()),
        .num_int_sub_overflows = byType(.none()),
        .num_int_sub_proven_cannot_overflow = byType(.none()),
        .num_int_mul_wrap = byType(.none()),
        .num_int_mul_crash_on_overflow = byType(.none()),
        .num_int_mul_overflows = byType(.none()),
        .num_int_mul_proven_cannot_overflow = byType(.none()),
        .num_float_add = byType(.none()),
        .num_float_sub = byType(.none()),
        .num_float_mul = byType(.none()),
        .dec_mul = byType(.none()),
        .num_div_by = byType(.none()),
        .num_div_by_checked = byType(.none()),
        .num_div_trunc_by = byType(.none()),
        .num_div_trunc_by_checked = byType(.none()),
        .num_rem_by = byType(.none()),
        .num_rem_by_checked = byType(.none()),
        .num_mod_by = byType(.none()),
        .num_mod_by_checked = byType(.none()),
        .num_pow = byType(.none()),
        .num_atan2 = byType(.none()),
        .num_sqrt = byType(.none()),
        .num_sin = byType(.none()),
        .num_cos = byType(.none()),
        .num_tan = byType(.none()),
        .num_asin = byType(.none()),
        .num_acos = byType(.none()),
        .num_atan = byType(.none()),
        .num_floor = byType(.none()),
        .num_ceiling = byType(.none()),
        .f32_to_bits = byType(.none()),
        .f32_from_bits = byType(.none()),
        .f64_to_bits = byType(.none()),
        .f64_from_bits = byType(.none()),
        .dec_to_attos = byType(.none()),
        .dec_from_attos = byType(.none()),
        .num_shift_left_by = byType(.none()),
        .num_shift_right_by = byType(.none()),
        .num_shift_right_zf_by = byType(.none()),
        .num_bitwise_and = byType(.none()),
        .num_bitwise_or = byType(.none()),
        .num_bitwise_xor = byType(.none()),
        .num_bitwise_not = byType(.none()),
        .num_count_one_bits = byType(.none()),
        .num_count_leading_zero_bits = byType(.none()),
        .num_count_trailing_zero_bits = byType(.none()),
        .num_from_le_bytes_unchecked = byType(.none()),
        .simd_load_16_unchecked = byType(.none()),
        .simd_splat = byType(.none()),
        .simd_get_lane_unchecked = byType(.none()),
        .simd_with_lane_unchecked = byType(.none()),
        .simd_to_u128_bits = byType(.none()),
        .simd_from_u128_bits = byType(.none()),
        .simd_add_wrap = byType(.none()),
        .simd_sub_wrap = byType(.none()),
        .simd_add_sat = byType(.none()),
        .simd_sub_sat = byType(.none()),
        .simd_neg_wrap = byType(.none()),
        .simd_abs_wrap = byType(.none()),
        .simd_min = byType(.none()),
        .simd_max = byType(.none()),
        .simd_abs_diff = byType(.none()),
        .simd_avg_rounded = byType(.none()),
        .simd_mul_wrap = byType(.none()),
        .simd_mul_high = byType(.none()),
        .simd_mul_q15_sat = byType(.none()),
        .simd_mul_wide_lo = byType(.none()),
        .simd_mul_wide_hi = byType(.none()),
        .simd_dot_pairs = byType(.none()),
        .simd_dot_pairs_sat = byType(.none()),
        .simd_sad = byType(.none()),
        .simd_and = byType(.none()),
        .simd_or = byType(.none()),
        .simd_xor = byType(.none()),
        .simd_not = byType(.none()),
        .simd_bit_select = byType(.none()),
        .simd_eq_lanes = byType(.none()),
        .simd_gt_lanes = byType(.none()),
        .simd_gte_lanes = byType(.none()),
        .simd_bitmask = byType(.none()),
        .simd_shl_wrap = byType(.none()),
        .simd_shr_wrap = byType(.none()),
        .simd_shr_zf_wrap = byType(.none()),
        .simd_shr_rounded = byType(.none()),
        .simd_interleave_lo = byType(.none()),
        .simd_interleave_hi = byType(.none()),
        .simd_even_lanes = byType(.none()),
        .simd_odd_lanes = byType(.none()),
        .simd_reverse_lanes = byType(.none()),
        .simd_table_lookup = byType(.none()),
        .simd_concat_shift_bytes = byType(.none()),
        .simd_widen_lo = byType(.none()),
        .simd_widen_hi = byType(.none()),
        .simd_pairwise_add_widen = byType(.none()),
        .simd_narrow_wrap = byType(.none()),
        .simd_narrow_sat = byType(.none()),
        .simd_sum_lanes = byType(.none()),
        .simd_sum_lanes_wrap = byType(.none()),
        .simd_clmul_lo = byType(.none()),
        .simd_clmul_hi = byType(.none()),
        .u8_from_str = byType(.none()),
        .i8_from_str = byType(.none()),
        .u16_from_str = byType(.none()),
        .i16_from_str = byType(.none()),
        .u32_from_str = byType(.none()),
        .i32_from_str = byType(.none()),
        .u64_from_str = byType(.none()),
        .i64_from_str = byType(.none()),
        .u128_from_str = byType(.none()),
        .i128_from_str = byType(.none()),
        .dec_from_str = byType(.none()),
        .f32_from_str = byType(.none()),
        .f64_from_str = byType(.none()),
        .compare = byType(.none()),
        .crash = byType(.none()),
    };

    /// ARC-only primitive variant whose result may borrow from an argument.
    /// Neutral LIR keeps the source operation; ARC uses this explicit mapping
    /// while solving and materializes exactly one of the two operations.
    pub fn arcBorrowedResultVariant(self: LowLevel) ?LowLevel {
        if (self == .list_sublist) return .list_sublist_borrowed;
        if (self == .box_unbox) return .box_unbox_borrowed;
        return null;
    }

    /// Ownership signature ARC solves for this neutral low-level statement.
    /// Operations without an ARC-only borrowed variant retain their declared
    /// statement effect, including synthetic effects used by focused tests.
    pub fn arcInferenceRcEffect(self: LowLevel, declared: RcEffect) RcEffect {
        // Box.unbox must establish an independent payload ownership place in
        // the solved graph. Emission may still select the borrowing variant
        // when the box lender survives, but an owned variant must not leave
        // the payload's unit keyed to the consumed box allocation.
        if (!self.arcOwnedResultConsumesLender()) return declared;
        return self.arcBorrowedResultVariant().?.rcEffect();
    }

    /// Whether ARC solves this operation through its borrowed-result variant
    /// and materializes the ordinary operation, which consumes the lender
    /// argument, exactly when the result is owned. An ownership demand on such
    /// a result is therefore a demand on its lender's unit.
    pub fn arcOwnedResultConsumesLender(self: LowLevel) bool {
        if (self == .box_unbox) return false;
        return self.arcBorrowedResultVariant() != null;
    }

    /// Whether this primitive can consume borrowed string views directly,
    /// without first materializing them into RocStr values.
    pub fn acceptsStrViewArgs(self: LowLevel) bool {
        return self == .str_count_utf8_bytes or
            self == .str_is_eq or
            self == .str_contains or
            self == .str_starts_with or
            self == .str_ends_with or
            self == .str_caseless_ascii_equals or
            self == .str_drop_prefix or
            self == .str_drop_suffix;
    }

    fn argMask(comptime args: []const u6) u64 {
        comptime var mask: u64 = 0;
        inline for (args) |arg| {
            mask |= @as(u64, 1) << arg;
        }
        return mask;
    }
};

// Regression test for https://github.com/roc-lang/roc/issues/12204
// No builtin produces a single-element list split, and the LLVM backend has no
// lowering for one, so `LowLevel` must not declare `list_split_first` or
// `list_split_last`.
test "LowLevel declares no list_split_first or list_split_last op" {
    try std.testing.expect(!@hasField(LowLevel, "list_split_first"));
    try std.testing.expect(!@hasField(LowLevel, "list_split_last"));
}
