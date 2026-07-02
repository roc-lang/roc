//! Shared canonical primitive-op vocabulary for canonicalization and LIR.
//!
//! This is the single source of truth for primitive names. Backends may still
//! reject specific ops that should have been lowered away earlier, but there is
//! no separate semantic/backend enum pair.

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
    str_find_first,
    str_count_utf8_bytes,
    str_with_capacity,
    str_reserve,
    str_release_excess_capacity,
    str_to_utf8,
    str_from_utf8_lossy,
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
    list_get_unsafe,
    list_append_unsafe,
    list_concat,
    list_with_capacity,
    list_drop_at,
    list_sublist,
    list_set,
    list_replace_unsafe,
    list_swap,
    list_prepend,
    list_first,
    list_last,
    list_drop_first,
    list_drop_last,
    list_take_first,
    list_take_last,
    list_reverse,
    list_reserve,
    list_release_excess_capacity,
    list_split_first,
    list_split_last,
    list_map_can_reuse,
    list_map_cast_unsafe,
    list_map_extract_unsafe,
    list_map_write_unsafe,

    // Bool operations
    bool_not,

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
    num_div_by,
    num_div_trunc_by,
    num_rem_by,
    num_mod_by,
    num_pow,
    num_sqrt,
    num_sin,
    num_cos,
    num_tan,
    num_asin,
    num_acos,
    num_atan,
    num_log,
    num_round,
    num_floor,
    num_ceiling,
    num_to_str,
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

    // Numeric parsing operations
    num_from_numeral,
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
    f32_from_str,
    f64_from_str,

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
    dec_to_i128_try_unsafe,
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
    /// (Ptr(T), T) -> {}: copy sizeOf(T) bytes from the value into *ptr.
    ptr_store,
    /// (Ptr(T)) -> T: copy sizeOf(T) bytes out of *ptr.
    ptr_load,
    /// (Box(T) | Ptr(T)) -> Ptr(T): identity bits.
    ptr_cast,

    // Comparison
    compare,

    // Crash/panic
    crash,

    /// Reference-counting behavior exposed by this primitive before LIR ARC
    /// insertion. This is explicit primitive metadata, not backend policy.
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
        /// outermost result — interior sharing described by
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

        pub fn retainsOrReleases() RcEffect {
            return .{ .may_retain_or_release = true };
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

        pub fn allocatesAndRetainsOrReleases() RcEffect {
            return .{
                .may_allocate = true,
                .may_retain_or_release = true,
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
                .result_unique = true,
            };
        }

        pub fn retainsSharingArgs(mask: u64) RcEffect {
            return .{
                .may_retain_or_release = mask != 0,
                .retain_args = mask,
                .result_shares_args = mask,
                .result_unique = true,
            };
        }

        pub fn allocatesSharingArgs(mask: u64) RcEffect {
            return .{
                .may_allocate = true,
                .result_shares_args = mask,
                .result_unique = true,
            };
        }

        pub fn allocatesAndRetainsOrReleasesSharingArgs(mask: u64) RcEffect {
            return .{
                .may_allocate = true,
                .may_retain_or_release = true,
                .result_shares_args = mask,
                .result_unique = true,
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
    };

    /// Return the explicit RC metadata for this primitive. The masks identify
    /// argument positions whose refcount may be inspected for copy-on-write.
    pub fn rcEffect(self: LowLevel) RcEffect {
        return switch (self) {
            .str_concat => RcEffect.runtimeUniqueness(argMask(&.{0})),
            .str_trim,
            .str_trim_start,
            .str_trim_end,
            .str_with_ascii_lowercased,
            .str_with_ascii_uppercased,
            .str_reserve,
            .str_release_excess_capacity,
            => RcEffect.runtimeUniqueness(argMask(&.{0})),

            .str_drop_prefix,
            .str_drop_prefix_caseless_ascii,
            .str_drop_suffix,
            .str_find_first,
            => RcEffect.retainsSharingArgs(argMask(&.{0})),

            .str_from_utf8 => RcEffect.retainsOrReleasesSharingArgs(argMask(&.{0})),

            .str_to_utf8 => RcEffect.allocatesAndRetainsOrReleasesSharingArgs(argMask(&.{0})),

            .list_drop_at,
            .list_sublist,
            .list_drop_first,
            .list_drop_last,
            .list_take_first,
            .list_take_last,
            .list_split_first,
            .list_split_last,
            => RcEffect.runtimeUniquenessMaybeSharedResult(argMask(&.{0})),

            .list_reverse,
            .list_reserve,
            .list_release_excess_capacity,
            => RcEffect.runtimeUniqueness(argMask(&.{0})),

            .list_prepend => RcEffect.runtimeUniquenessRetainingArgs(argMask(&.{0}), argMask(&.{1})),

            .list_append_unsafe => RcEffect.consumesArgsReturningConsumedArgsRetainingArgs(argMask(&.{0}), argMask(&.{1})),

            // Reads the list's refcount (and slice bit) without changing it.
            .list_map_can_reuse => RcEffect.none(),

            // Retypes a unique non-slice list to the output element type,
            // keeping the same allocation. Only reachable behind a true
            // `list_map_can_reuse`, so the result's count is 1 on return.
            .list_map_cast_unsafe => RcEffect.consumesArgsReturningConsumedArgsRetainingArgs(argMask(&.{0}), 0),

            // Moves one element's ownership out of a unique list's buffer.
            // The buffer slot keeps stale bytes until `list_map_write_unsafe`
            // stores the replacement; the op itself performs no RC work.
            .list_map_extract_unsafe => RcEffect.none(),

            // Stores an owned element into the slot vacated by
            // `list_map_extract_unsafe`, mirroring `list_append_unsafe`.
            .list_map_write_unsafe => RcEffect.consumesArgsReturningConsumedArgsRetainingArgs(argMask(&.{0}), argMask(&.{2})),

            .list_swap => RcEffect.runtimeUniqueness(argMask(&.{0})),

            .list_set,
            .list_replace_unsafe,
            => RcEffect.runtimeUniquenessRetainingArgs(argMask(&.{0}), argMask(&.{2})),

            .list_concat => RcEffect.runtimeUniqueness(argMask(&.{ 0, 1 })),

            .list_first,
            .list_last,
            .list_get_unsafe,
            => RcEffect.retainsResultBorrowingArgs(argMask(&.{0})),

            .str_split_on => RcEffect.allocatesSharingArgs(argMask(&.{0})),

            .str_repeat,
            .str_from_utf8_lossy,
            .str_with_capacity,
            .str_inspect,
            .u8_to_str,
            .i8_to_str,
            .u16_to_str,
            .i16_to_str,
            .u32_to_str,
            .i32_to_str,
            .u64_to_str,
            .i64_to_str,
            .u128_to_str,
            .i128_to_str,
            .dec_to_str,
            .f32_to_str,
            .f64_to_str,
            .num_to_str,
            .list_with_capacity,
            .crypto_sha256_hash_bytes,
            .crypto_sha256_hasher_empty,
            .crypto_sha256_hasher_write,
            .crypto_sha256_hasher_finish,
            .crypto_blake3_hash_bytes,
            .crypto_blake3_hasher_empty,
            .crypto_blake3_hasher_write,
            .crypto_blake3_hasher_finish,
            => RcEffect.allocates(),

            .str_join_with => RcEffect.allocatesConsumingArgs(argMask(&.{0})),

            .box_box => RcEffect.allocatesRetainingArgs(argMask(&.{0})),

            .box_unbox => RcEffect.retainsResultBorrowingArgs(argMask(&.{0})),

            // The capture environment is read through the executing frame's
            // closure, not through an explicit refcounted argument, so the
            // result cannot name a lender to borrow from.
            .erased_capture_load => RcEffect.retainsResult(),

            .box_alloc_zeroed => RcEffect.allocates(),

            // The stored value's ownership transfers into the pointed-at structure.
            // The pointer args/results are ptr layouts, which are never refcounted.
            .ptr_store => RcEffect.consumesArgsRetainingArgs(argMask(&.{1}), 0),

            .ptr_alloca,
            .ptr_load,
            .ptr_cast,
            => RcEffect.none(),

            .str_is_eq,
            .str_is_eq_static_small,
            .str_static_small_word_eq,
            .str_static_small_word_caseless_eq,
            .str_contains,
            .str_caseless_ascii_equals,
            .str_starts_with,
            .str_ends_with,
            .str_count_utf8_bytes,
            .list_len,
            .bool_not,
            .dict_pseudo_seed,
            .hasher_finish,
            .hasher_write_bool,
            .hasher_write_u8,
            .hasher_write_u16,
            .hasher_write_u32,
            .hasher_write_u64,
            .hasher_write_u128,
            .hasher_write_i8,
            .hasher_write_i16,
            .hasher_write_i32,
            .hasher_write_i64,
            .hasher_write_i128,
            .hasher_write_f32,
            .hasher_write_f64,
            .hasher_write_dec,
            .hasher_write_bytes,
            .hasher_write_str,
            .num_is_eq,
            .num_is_gt,
            .num_is_gte,
            .num_is_lt,
            .num_is_lte,
            .num_negate,
            .num_abs,
            .num_abs_diff,
            .num_plus,
            .num_minus,
            .num_times,
            .num_div_by,
            .num_div_trunc_by,
            .num_rem_by,
            .num_mod_by,
            .num_pow,
            .num_sqrt,
            .num_sin,
            .num_cos,
            .num_tan,
            .num_asin,
            .num_acos,
            .num_atan,
            .num_log,
            .num_round,
            .num_floor,
            .num_ceiling,
            .f32_to_bits,
            .f32_from_bits,
            .f64_to_bits,
            .f64_from_bits,
            .num_shift_left_by,
            .num_shift_right_by,
            .num_shift_right_zf_by,
            .num_bitwise_and,
            .num_bitwise_or,
            .num_bitwise_xor,
            .num_bitwise_not,
            .num_from_numeral,
            .u8_from_str,
            .i8_from_str,
            .u16_from_str,
            .i16_from_str,
            .u32_from_str,
            .i32_from_str,
            .u64_from_str,
            .i64_from_str,
            .u128_from_str,
            .i128_from_str,
            .dec_from_str,
            .f32_from_str,
            .f64_from_str,
            .u8_to_i8_wrap,
            .u8_to_i8_try,
            .u8_to_i16,
            .u8_to_i32,
            .u8_to_i64,
            .u8_to_i128,
            .u8_to_u16,
            .u8_to_u32,
            .u8_to_u64,
            .u8_to_u128,
            .u8_to_f32,
            .u8_to_f64,
            .u8_to_dec,
            .i8_to_i16,
            .i8_to_i32,
            .i8_to_i64,
            .i8_to_i128,
            .i8_to_u8_wrap,
            .i8_to_u8_try,
            .i8_to_u16_wrap,
            .i8_to_u16_try,
            .i8_to_u32_wrap,
            .i8_to_u32_try,
            .i8_to_u64_wrap,
            .i8_to_u64_try,
            .i8_to_u128_wrap,
            .i8_to_u128_try,
            .i8_to_f32,
            .i8_to_f64,
            .i8_to_dec,
            .u16_to_i8_wrap,
            .u16_to_i8_try,
            .u16_to_i16_wrap,
            .u16_to_i16_try,
            .u16_to_i32,
            .u16_to_i64,
            .u16_to_i128,
            .u16_to_u8_wrap,
            .u16_to_u8_try,
            .u16_to_u32,
            .u16_to_u64,
            .u16_to_u128,
            .u16_to_f32,
            .u16_to_f64,
            .u16_to_dec,
            .i16_to_i8_wrap,
            .i16_to_i8_try,
            .i16_to_i32,
            .i16_to_i64,
            .i16_to_i128,
            .i16_to_u8_wrap,
            .i16_to_u8_try,
            .i16_to_u16_wrap,
            .i16_to_u16_try,
            .i16_to_u32_wrap,
            .i16_to_u32_try,
            .i16_to_u64_wrap,
            .i16_to_u64_try,
            .i16_to_u128_wrap,
            .i16_to_u128_try,
            .i16_to_f32,
            .i16_to_f64,
            .i16_to_dec,
            .u32_to_i8_wrap,
            .u32_to_i8_try,
            .u32_to_i16_wrap,
            .u32_to_i16_try,
            .u32_to_i32_wrap,
            .u32_to_i32_try,
            .u32_to_i64,
            .u32_to_i128,
            .u32_to_u8_wrap,
            .u32_to_u8_try,
            .u32_to_u16_wrap,
            .u32_to_u16_try,
            .u32_to_u64,
            .u32_to_u128,
            .u32_to_f32,
            .u32_to_f64,
            .u32_to_dec,
            .i32_to_i8_wrap,
            .i32_to_i8_try,
            .i32_to_i16_wrap,
            .i32_to_i16_try,
            .i32_to_i64,
            .i32_to_i128,
            .i32_to_u8_wrap,
            .i32_to_u8_try,
            .i32_to_u16_wrap,
            .i32_to_u16_try,
            .i32_to_u32_wrap,
            .i32_to_u32_try,
            .i32_to_u64_wrap,
            .i32_to_u64_try,
            .i32_to_u128_wrap,
            .i32_to_u128_try,
            .i32_to_f32,
            .i32_to_f64,
            .i32_to_dec,
            .u64_to_i8_wrap,
            .u64_to_i8_try,
            .u64_to_i16_wrap,
            .u64_to_i16_try,
            .u64_to_i32_wrap,
            .u64_to_i32_try,
            .u64_to_i64_wrap,
            .u64_to_i64_try,
            .u64_to_i128,
            .u64_to_u8_wrap,
            .u64_to_u8_try,
            .u64_to_u16_wrap,
            .u64_to_u16_try,
            .u64_to_u32_wrap,
            .u64_to_u32_try,
            .u64_to_u128,
            .u64_to_f32,
            .u64_to_f64,
            .u64_to_dec,
            .i64_to_i8_wrap,
            .i64_to_i8_try,
            .i64_to_i16_wrap,
            .i64_to_i16_try,
            .i64_to_i32_wrap,
            .i64_to_i32_try,
            .i64_to_i128,
            .i64_to_u8_wrap,
            .i64_to_u8_try,
            .i64_to_u16_wrap,
            .i64_to_u16_try,
            .i64_to_u32_wrap,
            .i64_to_u32_try,
            .i64_to_u64_wrap,
            .i64_to_u64_try,
            .i64_to_u128_wrap,
            .i64_to_u128_try,
            .i64_to_f32,
            .i64_to_f64,
            .i64_to_dec,
            .u128_to_i8_wrap,
            .u128_to_i8_try,
            .u128_to_i16_wrap,
            .u128_to_i16_try,
            .u128_to_i32_wrap,
            .u128_to_i32_try,
            .u128_to_i64_wrap,
            .u128_to_i64_try,
            .u128_to_i128_wrap,
            .u128_to_i128_try,
            .u128_to_u8_wrap,
            .u128_to_u8_try,
            .u128_to_u16_wrap,
            .u128_to_u16_try,
            .u128_to_u32_wrap,
            .u128_to_u32_try,
            .u128_to_u64_wrap,
            .u128_to_u64_try,
            .u128_to_f32,
            .u128_to_f64,
            .u128_to_dec_try_unsafe,
            .i128_to_i8_wrap,
            .i128_to_i8_try,
            .i128_to_i16_wrap,
            .i128_to_i16_try,
            .i128_to_i32_wrap,
            .i128_to_i32_try,
            .i128_to_i64_wrap,
            .i128_to_i64_try,
            .i128_to_u8_wrap,
            .i128_to_u8_try,
            .i128_to_u16_wrap,
            .i128_to_u16_try,
            .i128_to_u32_wrap,
            .i128_to_u32_try,
            .i128_to_u64_wrap,
            .i128_to_u64_try,
            .i128_to_u128_wrap,
            .i128_to_u128_try,
            .i128_to_f32,
            .i128_to_f64,
            .i128_to_dec_try_unsafe,
            .f32_to_i8_trunc,
            .f32_to_i8_try_unsafe,
            .f32_to_i16_trunc,
            .f32_to_i16_try_unsafe,
            .f32_to_i32_trunc,
            .f32_to_i32_try_unsafe,
            .f32_to_i64_trunc,
            .f32_to_i64_try_unsafe,
            .f32_to_i128_trunc,
            .f32_to_i128_try_unsafe,
            .f32_to_u8_trunc,
            .f32_to_u8_try_unsafe,
            .f32_to_u16_trunc,
            .f32_to_u16_try_unsafe,
            .f32_to_u32_trunc,
            .f32_to_u32_try_unsafe,
            .f32_to_u64_trunc,
            .f32_to_u64_try_unsafe,
            .f32_to_u128_trunc,
            .f32_to_u128_try_unsafe,
            .f32_to_f64,
            .f64_to_i8_trunc,
            .f64_to_i8_try_unsafe,
            .f64_to_i16_trunc,
            .f64_to_i16_try_unsafe,
            .f64_to_i32_trunc,
            .f64_to_i32_try_unsafe,
            .f64_to_i64_trunc,
            .f64_to_i64_try_unsafe,
            .f64_to_i128_trunc,
            .f64_to_i128_try_unsafe,
            .f64_to_u8_trunc,
            .f64_to_u8_try_unsafe,
            .f64_to_u16_trunc,
            .f64_to_u16_try_unsafe,
            .f64_to_u32_trunc,
            .f64_to_u32_try_unsafe,
            .f64_to_u64_trunc,
            .f64_to_u64_try_unsafe,
            .f64_to_u128_trunc,
            .f64_to_u128_try_unsafe,
            .f64_to_f32_wrap,
            .f64_to_f32_try_unsafe,
            .dec_to_i8_trunc,
            .dec_to_i8_try_unsafe,
            .dec_to_i16_trunc,
            .dec_to_i16_try_unsafe,
            .dec_to_i32_trunc,
            .dec_to_i32_try_unsafe,
            .dec_to_i64_trunc,
            .dec_to_i64_try_unsafe,
            .dec_to_i128_trunc,
            .dec_to_i128_try_unsafe,
            .dec_to_u8_trunc,
            .dec_to_u8_try_unsafe,
            .dec_to_u16_trunc,
            .dec_to_u16_try_unsafe,
            .dec_to_u32_trunc,
            .dec_to_u32_try_unsafe,
            .dec_to_u64_trunc,
            .dec_to_u64_try_unsafe,
            .dec_to_u128_trunc,
            .dec_to_u128_try_unsafe,
            .dec_to_f32_wrap,
            .dec_to_f32_try_unsafe,
            .dec_to_f64,
            .compare,
            .crash,
            => RcEffect.none(),
        };
    }

    /// Whether this primitive can consume borrowed string views directly,
    /// without first materializing them into RocStr values.
    pub fn acceptsStrViewArgs(self: LowLevel) bool {
        return switch (self) {
            .str_count_utf8_bytes,
            .str_is_eq,
            .str_contains,
            .str_starts_with,
            .str_ends_with,
            .str_caseless_ascii_equals,
            .str_drop_prefix,
            .str_drop_suffix,
            => true,
            else => false,
        };
    }

    fn argMask(comptime args: []const u6) u64 {
        comptime var mask: u64 = 0;
        inline for (args) |arg| {
            mask |= @as(u64, 1) << arg;
        }
        return mask;
    }

    pub const NumericParseSpec = union(enum) {
        int: struct {
            width_bytes: u8,
            signed: bool,
        },
        float: struct {
            width_bytes: u8,
        },
        dec,
    };

    pub fn numericParseSpec(self: LowLevel) ?NumericParseSpec {
        return switch (self) {
            .u8_from_str => .{ .int = .{ .width_bytes = 1, .signed = false } },
            .i8_from_str => .{ .int = .{ .width_bytes = 1, .signed = true } },
            .u16_from_str => .{ .int = .{ .width_bytes = 2, .signed = false } },
            .i16_from_str => .{ .int = .{ .width_bytes = 2, .signed = true } },
            .u32_from_str => .{ .int = .{ .width_bytes = 4, .signed = false } },
            .i32_from_str => .{ .int = .{ .width_bytes = 4, .signed = true } },
            .u64_from_str => .{ .int = .{ .width_bytes = 8, .signed = false } },
            .i64_from_str => .{ .int = .{ .width_bytes = 8, .signed = true } },
            .u128_from_str => .{ .int = .{ .width_bytes = 16, .signed = false } },
            .i128_from_str => .{ .int = .{ .width_bytes = 16, .signed = true } },
            .f32_from_str => .{ .float = .{ .width_bytes = 4 } },
            .f64_from_str => .{ .float = .{ .width_bytes = 8 } },
            .dec_from_str => .dec,
            else => null,
        };
    }
};
