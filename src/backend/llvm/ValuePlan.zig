//! How the LLVM backend represents each local of one procedure.
//!
//! A local whose layout has a scalar LLVM type (a Bool, an integer, a float,
//! a Dec, or a SIMD vector) and whose every operand position the emitter reads
//! and writes through value helpers lives as LLVM SSA values instead of a
//! stack slot. Every other local keeps its addressable entry-block slot: its
//! layout has no scalar carrier, or some statement, call boundary, or builtin
//! needs its address. `Rep` records the choice.
//!
//! An SSA local is either `dominating` or `merged`. A dominating local has one
//! definition (a statement's write, or the prologue's for a parameter no
//! statement writes) that dominates every read: no path reaches a read without
//! passing the definition, which `lir.StackPlan` liveness proves by the local
//! not being live on entry to the procedure. Its value is recorded once and
//! read everywhere. Every other SSA local is merged: its current definition
//! depends on the path taken, so each control-flow merge point at which it is
//! live on entry receives a phi. The merge points are exactly the blocks the
//! emitter shares between several incoming edges: a join's body, entered by
//! the join's jumps, and a statement reached by more than one edge.
//!
//! The plan consumes only explicit LIR: statements and their operands, layouts,
//! join declarations, the emitter's shared-statement counts, and the callee
//! ABI placements the caller supplies. It is computed per procedure immediately
//! before that procedure's body is emitted and is invalid after it; the
//! store-indexed representation column is reset from the procedure's own local
//! inventory, so sibling specializations that reuse a `LocalId` never observe
//! each other's representations.
const std = @import("std");
const lir = @import("lir");

const Allocator = std.mem.Allocator;
const CFStmtId = lir.CFStmtId;
const LocalId = lir.LocalId;
const LocalSpan = lir.LocalSpan;
const LirStore = lir.LirStore;
const LowLevel = lir.LowLevel;
const GuardedList = LirStore.GuardedList;
const StackPlan = lir.StackPlan;
const numeric_conversion = @import("base").numeric_conversion;
const Self = @This();

/// A local's representation in the procedure being emitted.
pub const Rep = enum(u8) {
    /// An addressable entry-block slot.
    memory,
    /// SSA values; the single definition dominates every read.
    dominating,
    /// SSA values; merge points where the local is live get phis.
    merged,
};

/// How the emitter accesses one operand.
pub const Access = enum {
    /// Only through the representation-agnostic value helpers.
    value,
    /// Through the address of the local's slot.
    address,
};

/// The operand accesses of one low-level operation's emitter.
pub const OpAccess = struct {
    /// Bit `i` is set when argument `i` is accessed by value.
    value_args: u64 = 0,
    value_target: bool = false,

    const all_values: OpAccess = .{ .value_args = std.math.maxInt(u64), .value_target = true };

    fn args(mask: u64, value_target: bool) OpAccess {
        return .{ .value_args = mask, .value_target = value_target };
    }

    pub fn arg(self: OpAccess, index: usize) Access {
        if (index >= 64) return .address;
        return if ((self.value_args >> @intCast(index)) & 1 != 0) .value else .address;
    }

    pub fn target(self: OpAccess) Access {
        return if (self.value_target) .value else .address;
    }
};

/// How the emitter accesses the operands of `op`. Each listed operation's
/// emitter reaches the listed operands only through the value helpers; every
/// other operand of every operation is read and written through its address,
/// which is what the remaining emitters (and the builtins they call with
/// pointers) require.
pub fn lowLevelAccess(op: LowLevel) OpAccess {
    if (numeric_conversion.getConversionSpec(op)) |spec| {
        // The wrapping float-to-integer builtin writes its result through a
        // pointer to the target.
        if (spec.src.class() == .float and spec.dst.class() == .int and spec.mode == .trunc) return OpAccess.args(1, false);
        return .all_values;
    }
    return switch (op) {
        .bool_not,
        .bool_likely,
        .num_is_eq,
        .num_is_gt,
        .num_is_gte,
        .num_is_lt,
        .num_is_lte,
        .compare,
        .num_int_add_wrap,
        .num_int_add_crash_on_overflow,
        .num_int_add_overflows,
        .num_int_add_proven_cannot_overflow,
        .num_int_sub_wrap,
        .num_int_sub_crash_on_overflow,
        .num_int_sub_overflows,
        .num_int_sub_proven_cannot_overflow,
        .num_int_mul_wrap,
        .num_int_mul_crash_on_overflow,
        .num_int_mul_overflows,
        .num_int_mul_proven_cannot_overflow,
        .num_float_add,
        .num_float_sub,
        .num_float_mul,
        .dec_mul,
        .num_div_by,
        .num_div_by_checked,
        .num_div_trunc_by,
        .num_div_trunc_by_checked,
        .num_rem_by,
        .num_rem_by_checked,
        .num_mod_by,
        .num_mod_by_checked,
        .num_shift_left_by,
        .num_shift_right_by,
        .num_shift_right_zf_by,
        .num_bitwise_and,
        .num_bitwise_or,
        .num_bitwise_xor,
        .num_bitwise_not,
        .num_count_one_bits,
        .num_count_leading_zero_bits,
        .num_count_trailing_zero_bits,
        .num_negate,
        .num_negate_checked,
        .num_abs,
        .num_abs_checked,
        .num_abs_diff,
        .num_pow,
        .num_atan2,
        .num_sqrt,
        .num_sin,
        .num_cos,
        .num_tan,
        .num_asin,
        .num_acos,
        .num_atan,
        .num_floor,
        .num_ceiling,
        .f32_to_bits,
        .f32_from_bits,
        .f64_to_bits,
        .f64_from_bits,
        .dec_from_attos,
        .dec_to_attos,
        .simd_splat,
        .simd_get_lane_unchecked,
        .simd_with_lane_unchecked,
        .simd_to_u128_bits,
        .simd_from_u128_bits,
        .simd_add_wrap,
        .simd_sub_wrap,
        .simd_add_sat,
        .simd_sub_sat,
        .simd_neg_wrap,
        .simd_abs_wrap,
        .simd_min,
        .simd_max,
        .simd_abs_diff,
        .simd_avg_rounded,
        .simd_mul_wrap,
        .simd_mul_high,
        .simd_mul_q15_sat,
        .simd_mul_wide_lo,
        .simd_mul_wide_hi,
        .simd_dot_pairs,
        .simd_dot_pairs_sat,
        .simd_sad,
        .simd_and,
        .simd_or,
        .simd_xor,
        .simd_not,
        .simd_bit_select,
        .simd_eq_lanes,
        .simd_gt_lanes,
        .simd_gte_lanes,
        .simd_bitmask,
        .simd_shl_wrap,
        .simd_shr_wrap,
        .simd_shr_zf_wrap,
        .simd_shr_rounded,
        .simd_interleave_lo,
        .simd_interleave_hi,
        .simd_even_lanes,
        .simd_odd_lanes,
        .simd_reverse_lanes,
        .simd_table_lookup,
        .simd_concat_shift_bytes,
        .simd_widen_lo,
        .simd_widen_hi,
        .simd_pairwise_add_widen,
        .simd_narrow_wrap,
        .simd_narrow_sat,
        .simd_sum_lanes,
        .simd_sum_lanes_wrap,
        .simd_clmul_lo,
        .simd_clmul_hi,
        => .all_values,
        // (list, index): the element loads from the list's bytes.
        .list_get_unsafe => OpAccess.args(0b10, true),
        // (bytes, index)
        .num_from_le_bytes_unchecked, .simd_load_16_unchecked => OpAccess.args(0b10, true),
        // (list, index)
        .list_prefetch => OpAccess.args(0b10, false),
        // (list, i, j)
        .list_swap => OpAccess.args(0b110, false),
        // (list, index, element): the builtin takes the element's address.
        .list_set, .list_set_in_place_unsafe, .list_replace_unsafe => OpAccess.args(0b010, false),
        // (list, element): the element's bytes are stored after the last.
        .list_append_unsafe => OpAccess.args(0b10, false),
        .list_len, .list_capacity => OpAccess.args(0, true),
        // (vector, list, index)
        .simd_store_16_unchecked => OpAccess.args(0b101, false),
        // (vector, list)
        .simd_append_16 => OpAccess.args(0b01, false),
        .str_is_eq,
        .str_is_eq_static_small,
        .str_static_small_word_eq,
        .str_static_small_word_caseless_eq,
        .str_concat,
        .str_contains,
        .str_trim,
        .str_trim_start,
        .str_trim_end,
        .str_caseless_ascii_equals,
        .str_with_ascii_lowercased,
        .str_with_ascii_uppercased,
        .str_starts_with,
        .str_ends_with,
        .str_repeat,
        .str_drop_prefix,
        .str_drop_prefix_caseless_ascii,
        .str_drop_suffix,
        .str_split_first,
        .str_split_last,
        .str_count_utf8_bytes,
        .str_get_utf8_byte_unsafe,
        .str_substring_unsafe,
        .str_with_capacity,
        .str_reserve,
        .str_release_excess_capacity,
        .str_to_utf8,
        .str_from_utf8_lossy,
        .str_from_utf8_validated,
        .str_from_utf16_le_short,
        .str_from_utf16_be_short,
        .str_from_utf32_le_short,
        .str_from_utf32_be_short,
        .str_from_utf8,
        .str_split_on,
        .str_join_with,
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
        .list_prefetched,
        .list_concat,
        .list_append_range_within,
        .list_copy_range_within,
        .list_append_range_within_unsafe,
        .list_append_sublist,
        .list_append_le_bytes,
        .list_append_le_bytes_unsafe,
        .list_slack_unique,
        .list_owned_unique,
        .list_with_capacity,
        .list_drop_at,
        .list_sublist,
        .list_sublist_borrowed,
        .list_prepend,
        .list_take_first,
        .list_take_last,
        .list_reverse,
        .list_reserve,
        .list_reserve_for_append,
        .list_release_excess_capacity,
        .list_clear,
        .list_map_prepare_reuse,
        .list_map_can_reuse,
        .list_map_cast_unsafe,
        .list_map_extract_unsafe,
        .list_map_write_unsafe,
        .list_item_size_at_most,
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
        .crypto_sha256_hash_bytes,
        .crypto_sha256_hasher_empty,
        .crypto_sha256_hasher_write,
        .crypto_sha256_hasher_finish,
        .crypto_blake3_hash_bytes,
        .crypto_blake3_hasher_empty,
        .crypto_blake3_hasher_write,
        .crypto_blake3_hasher_finish,
        .num_plus,
        .num_minus,
        .num_times,
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
        .u8_from_str_prefix,
        .u8_from_utf8_prefix,
        .i8_from_str_prefix,
        .i8_from_utf8_prefix,
        .u16_from_str_prefix,
        .u16_from_utf8_prefix,
        .i16_from_str_prefix,
        .i16_from_utf8_prefix,
        .u32_from_str_prefix,
        .u32_from_utf8_prefix,
        .i32_from_str_prefix,
        .i32_from_utf8_prefix,
        .u64_from_str_prefix,
        .u64_from_utf8_prefix,
        .i64_from_str_prefix,
        .i64_from_utf8_prefix,
        .u128_from_str_prefix,
        .u128_from_utf8_prefix,
        .i128_from_str_prefix,
        .i128_from_utf8_prefix,
        .dec_from_str_prefix,
        .dec_from_utf8_prefix,
        .f32_from_str_prefix,
        .f32_from_utf8_prefix,
        .f64_from_str_prefix,
        .f64_from_utf8_prefix,
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
        .box_box,
        .box_unbox,
        .box_unbox_borrowed,
        .box_prepare_update,
        .erased_capture_load,
        .ptr_alloca,
        .box_alloc_zeroed,
        .ptr_store,
        .ptr_load,
        .ptr_cast,
        .crash,
        => .{},
    };
}

/// An operand of a direct call: one of its arguments, or its result.
pub const CallOperand = union(enum) {
    result,
    arg: usize,
};

/// One merge point's merged locals that are live on entry to it.
pub const Merge = struct { start: u32, len: u32 };

allocator: Allocator,
indexes: StackPlan.Indexes,
/// Each local's representation, by local id. Rows outside the current
/// procedure's inventory hold `.memory`.
reps: std.ArrayList(Rep) = .empty,
/// Whether some operand of the current procedure needs the local's address,
/// by local id. Cleared from the procedure's inventory.
addressed: std.ArrayList(bool) = .empty,
/// The current procedure's SSA locals.
ssa_locals: std.ArrayList(LocalId) = .empty,
/// By merge-point statement, its span of `merge_locals`.
merges: std.AutoHashMapUnmanaged(u32, Merge) = .empty,
merge_locals: std.ArrayList(LocalId) = .empty,
visited: std.AutoHashMapUnmanaged(u32, void) = .empty,
work: std.ArrayList(CFStmtId) = .empty,
planned: std.ArrayList(CFStmtId) = .empty,
value_accesses: std.ArrayList(StackPlan.Range) = .empty,
merge_nodes: std.ArrayList(MergeNode) = .empty,
pairs: std.ArrayList(MergePair) = .empty,

const MergeNode = struct { position: u32, node: u32, stmt: u32 };
const MergePair = struct { stmt: u32, local: LocalId };

pub fn init(allocator: Allocator) Self {
    return .{ .allocator = allocator, .indexes = StackPlan.Indexes.init(allocator) };
}

pub fn deinit(self: *Self) void {
    self.indexes.deinit();
    self.reps.deinit(self.allocator);
    self.addressed.deinit(self.allocator);
    self.ssa_locals.deinit(self.allocator);
    self.merges.deinit(self.allocator);
    self.merge_locals.deinit(self.allocator);
    self.visited.deinit(self.allocator);
    self.work.deinit(self.allocator);
    self.planned.deinit(self.allocator);
    self.value_accesses.deinit(self.allocator);
    self.merge_nodes.deinit(self.allocator);
    self.pairs.deinit(self.allocator);
}

/// The representation of `local` in the planned procedure.
pub fn rep(self: *const Self, local: LocalId) Rep {
    const raw = @backingInt(local);
    if (raw >= self.reps.items.len) return .memory;
    return self.reps.items[raw];
}

/// The merged locals live on entry to the merge point `stmt`: a join body or
/// a statement the emitter shares between several incoming edges.
pub fn mergeLocals(self: *const Self, stmt: CFStmtId) []const LocalId {
    const merge = self.merges.get(@backingInt(stmt)) orelse return &.{};
    return self.merge_locals.items[merge.start..][0..merge.len];
}

/// The planned procedure's SSA locals.
pub fn ssaLocals(self: *const Self) []const LocalId {
    return self.ssa_locals.items;
}

/// Forget the planned procedure.
pub fn clear(self: *Self) void {
    for (self.ssa_locals.items) |local| self.reps.items[@backingInt(local)] = .memory;
    self.ssa_locals.clearRetainingCapacity();
    self.merges.clearRetainingCapacity();
    self.merge_locals.clearRetainingCapacity();
}

/// Plan the representations of the procedure whose parameters are `params`,
/// whose other locals are `frame_locals`, and whose body is rooted at `root`.
///
/// `ops` supplies the facts the plan consumes from the emitter's contracts:
/// `isCandidate(local)` (the layout has a scalar carrier), `paramAccess(index)`
/// (how the prologue binds parameter `index`), `callAccess(call, operand)`
/// (how a direct call passes argument `operand.arg` or receives its result),
/// and `isShared(stmt)` (the emitter enters `stmt` through a shared block).
pub fn plan(
    self: *Self,
    store: *const LirStore,
    params: LocalSpan,
    frame_locals: LocalSpan,
    root: CFStmtId,
    ops: anytype,
) Allocator.Error!void {
    self.clear();
    const local_count = store.localCount();
    if (self.reps.items.len < local_count) {
        const old_len = self.reps.items.len;
        try self.reps.resize(self.allocator, local_count);
        @memset(self.reps.items[old_len..], .memory);
        try self.addressed.resize(self.allocator, local_count);
        @memset(self.addressed.items[old_len..], false);
    }
    const param_locals = store.getLocalSpan(params);
    const frame = store.getLocalSpan(frame_locals);
    defer {
        for (0..param_locals.len) |i| self.addressed.items[@backingInt(GuardedList.at(param_locals, i))] = false;
        for (0..frame.len) |i| self.addressed.items[@backingInt(GuardedList.at(frame, i))] = false;
    }

    // Operands the emitter reaches through an address.
    for (0..param_locals.len) |i| {
        if (ops.paramAccess(i) == .address) self.addressed.items[@backingInt(GuardedList.at(param_locals, i))] = true;
    }
    self.visited.clearRetainingCapacity();
    self.work.clearRetainingCapacity();
    try self.work.append(self.allocator, root);
    while (self.work.pop()) |id| {
        if ((try self.visited.getOrPut(self.allocator, @backingInt(id))).found_existing) continue;
        self.noteAddressedOperands(store, store.getCFStmt(id), ops);
        try lir.BodyClone.appendSuccessors(store, &self.work, id, self.allocator);
    }

    var param_count: usize = 0;
    for ([_]@TypeOf(frame){ param_locals, frame }, 0..) |locals, list| {
        for (0..locals.len) |i| {
            const local = GuardedList.at(locals, i);
            if (self.addressed.items[@backingInt(local)] or !ops.isCandidate(local)) continue;
            if (self.reps.items[@backingInt(local)] != .memory) continue;
            self.reps.items[@backingInt(local)] = .dominating;
            try self.ssa_locals.append(self.allocator, local);
        }
        if (list == 0) param_count = self.ssa_locals.items.len;
    }
    if (self.ssa_locals.items.len == 0) return;
    try self.classify(store, param_count, root, ops);
}

const Tracked = struct {
    plan: *const Self,

    pub fn tracks(self: Tracked, local: LocalId) bool {
        return self.plan.rep(local) != .memory;
    }

    pub fn statement(_: Tracked, _: *StackPlan, _: u32, _: lir.LIR.CFStmt) Allocator.Error!void {}
};

/// Split the SSA locals into dominating and merged ones, and give every merge
/// point the merged locals live on entry to it.
/// The first `param_count` SSA locals are parameters.
fn classify(self: *Self, store: *const LirStore, param_count: usize, root: CFStmtId, ops: anytype) Allocator.Error!void {
    var stack_plan = self.indexes.takePlan();
    defer self.indexes.keepPlan(&stack_plan);
    self.planned.clearRetainingCapacity();
    defer self.indexes.clear(self.planned.items);
    try stack_plan.addProcGraph(store, &self.indexes, root, &self.planned, Tracked{ .plan = self });
    for (stack_plan.values.items) |*value| value.size = 1;
    try stack_plan.solveLifetimes();

    // Each solved value's run of the sorted accesses.
    const accesses = stack_plan.sortedAccesses();
    self.value_accesses.clearRetainingCapacity();
    try self.value_accesses.appendNTimes(self.allocator, .{ .start = 0, .end = 0 }, stack_plan.values.items.len);
    var begin: usize = 0;
    while (begin < accesses.len) {
        const value = accesses[begin].value;
        var end = begin;
        while (end < accesses.len and accesses[end].value == value) : (end += 1) {}
        self.value_accesses.items[value] = .{ .start = @intCast(begin), .end = @intCast(end) };
        begin = end;
    }

    const entry = self.indexes.nodeOf(root).?;
    var any_merged = false;
    for (self.ssa_locals.items, 0..) |local, position| {
        const index = stack_plan.localIndex(local) orelse continue;
        const definitions = stack_plan.values.items[index].definitions;
        const is_param = position < param_count;
        const span = self.value_accesses.items[index];
        const dominating = if (is_param)
            definitions == 0
        else
            definitions == 1 and !stack_plan.liveIn(index, accesses[span.start..span.end], entry);
        if (!dominating) {
            self.reps.items[@backingInt(local)] = .merged;
            any_merged = true;
        }
    }
    if (!any_merged) return;

    // The merge points, by solved position: every join body, entered only by
    // its jumps, and every statement the emitter shares between edges.
    self.merge_nodes.clearRetainingCapacity();
    var joins = self.indexes.joins.iterator();
    while (joins.next()) |join| try self.appendMergeNode(&stack_plan, join.value_ptr.*);
    for (self.planned.items) |stmt| {
        if (ops.isShared(stmt)) try self.appendMergeNode(&stack_plan, stmt);
    }
    if (self.merge_nodes.items.len == 0) return;
    std.mem.sort(MergeNode, self.merge_nodes.items, {}, struct {
        fn lessThan(_: void, a: MergeNode, b: MergeNode) bool {
            return a.position < b.position;
        }
    }.lessThan);

    // A merged local can be live only where its ranges cover a position.
    self.pairs.clearRetainingCapacity();
    for (self.ssa_locals.items) |local| {
        if (self.reps.items[@backingInt(local)] != .merged) continue;
        const index = stack_plan.localIndex(local) orelse continue;
        const span = self.value_accesses.items[index];
        const value_accesses = accesses[span.start..span.end];
        for (stack_plan.liveRanges(index)) |range| {
            var at = lowerBound(self.merge_nodes.items, range.start);
            while (at < self.merge_nodes.items.len and self.merge_nodes.items[at].position < range.end) : (at += 1) {
                const merge = self.merge_nodes.items[at];
                if (!stack_plan.liveIn(index, value_accesses, merge.node)) continue;
                try self.pairs.append(self.allocator, .{ .stmt = merge.stmt, .local = local });
            }
        }
    }
    std.mem.sort(MergePair, self.pairs.items, {}, struct {
        fn lessThan(_: void, a: MergePair, b: MergePair) bool {
            if (a.stmt != b.stmt) return a.stmt < b.stmt;
            return @backingInt(a.local) < @backingInt(b.local);
        }
    }.lessThan);
    var i: usize = 0;
    while (i < self.pairs.items.len) {
        const stmt = self.pairs.items[i].stmt;
        const start: u32 = @intCast(self.merge_locals.items.len);
        while (i < self.pairs.items.len and self.pairs.items[i].stmt == stmt) : (i += 1) {
            try self.merge_locals.append(self.allocator, self.pairs.items[i].local);
        }
        try self.merges.put(self.allocator, stmt, .{ .start = start, .len = @intCast(self.merge_locals.items.len - start) });
    }
}

fn appendMergeNode(self: *Self, stack_plan: *const StackPlan, stmt: CFStmtId) Allocator.Error!void {
    const node = self.indexes.nodeOf(stmt).?;
    try self.merge_nodes.append(self.allocator, .{ .position = stack_plan.nodePosition(node), .node = node, .stmt = @backingInt(stmt) });
}

fn lowerBound(nodes: []const MergeNode, position: u32) usize {
    var low: usize = 0;
    var high: usize = nodes.len;
    while (low < high) {
        const mid = low + (high - low) / 2;
        if (nodes[mid].position < position) low = mid + 1 else high = mid;
    }
    return low;
}

fn markAddressed(self: *Self, local: LocalId) void {
    const raw = @backingInt(local);
    if (raw < self.addressed.items.len) self.addressed.items[raw] = true;
}

fn noteAccess(self: *Self, local: LocalId, access: Access) void {
    if (access == .address) self.markAddressed(local);
}

fn noteAllAddressed(self: *Self, store: *const LirStore, stmt: lir.LIR.CFStmt) void {
    lir.BodyClone.forEachStmtRead(store, stmt, self, markAddressed);
    lir.BodyClone.forEachStmtDef(store, stmt, self, markAddressed);
}

/// Mark every operand of `stmt` the emitter reaches through its address.
fn noteAddressedOperands(self: *Self, store: *const LirStore, stmt: lir.LIR.CFStmt, ops: anytype) void {
    switch (stmt) {
        // Writes nothing: the local keeps whatever it held.
        .init_uninitialized => {},
        .assign_ref => |assign| switch (assign.op.kind) {
            // A copy reads and writes values in either representation.
            .local, .list_reinterpret, .nominal => {},
            // The aggregate is read in memory; the result is a value.
            .discriminant, .field, .tag_payload, .tag_payload_struct => self.markAddressed(assign.op.source),
        },
        .assign_literal => |assign| switch (assign.value) {
            .i64_literal, .i128_literal, .f64_literal, .f32_literal, .dec_literal => {},
            .str_literal, .boxy_dynamic_num_literal, .boxy_dynamic_frac_literal, .static_data, .bytes_literal, .proc_ref => self.markAddressed(assign.target),
        },
        .assign_call => |call| {
            const extra = store.stmtRowOrDefault(call.extra);
            if (extra.out_desc) |out_desc| self.markAddressed(out_desc);
            if (extra.result_desc) |desc| if (desc.localOrNull()) |local| self.markAddressed(local);
            // A pending drive writes the result through its address.
            if (extra.drive.canDriveHere()) self.markAddressed(call.target);
            if (!extra.replaces_frame) self.noteAccess(call.target, ops.callAccess(call, .result));
            const args = store.getLocalSpan(call.args);
            for (0..args.len) |i| self.noteAccess(GuardedList.at(args, i), ops.callAccess(call, .{ .arg = i }));
        },
        .assign_low_level => |assign| {
            const access = lowLevelAccess(assign.op);
            self.noteAccess(assign.target, access.target());
            const args = store.getLocalSpan(assign.args);
            for (0..args.len) |i| self.noteAccess(GuardedList.at(args, i), access.arg(i));
        },
        // The aggregate is built in memory from field values.
        inline .assign_list, .assign_struct => |assign| self.markAddressed(assign.target),
        .assign_tag => |assign| {
            self.markAddressed(assign.target);
            if (store.optionalStmtRow(assign.target_desc)) |desc| if (desc.localOrNull()) |local| self.markAddressed(local);
        },
        .store_struct => |assign| self.markAddressed(assign.dest),
        .store_tag => |assign| self.markAddressed(assign.dest),
        // A copy reads and writes values in either representation.
        .set_local => {},
        .expect => {},
        .switch_stmt => {},
        .switch_initialized_payload => |sw| self.markAddressed(sw.payload),
        .decref_if_initialized => |dec| self.markAddressed(dec.value),
        .ret => {},
        .join, .jump, .comptime_branch_taken, .runtime_error, .comptime_exhaustiveness_failed => {},
        .assign_call_erased,
        .assign_packed_erased_fn,
        .assign_boxy_desc_ref,
        .assign_boxy_dict_ref,
        .assign_boxy_box,
        .assign_boxy_record_update,
        .assign_boxy_unbox,
        .assign_boxy_adapt,
        .assign_boxy_inspect,
        .assign_boxy_eq,
        .assign_boxy_hash,
        .assign_boxy_tag,
        .assign_boxy_tag_payload,
        .boxy_tag_match,
        .assign_call_dict,
        .debug,
        .expect_err,
        .crash,
        .incref,
        .decref,
        .free,
        .str_match,
        .str_match_set,
        => self.noteAllAddressed(store, stmt),
    }
}

const TestOps = struct {
    store: *const LirStore,

    pub fn isCandidate(self: TestOps, local: LocalId) bool {
        return switch (self.store.getLocal(local).layout_idx) {
            .bool, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .dec, .f32, .f64 => true,
            .str, .opaque_ptr, .zst, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2, _ => false,
        };
    }

    pub fn paramAccess(_: TestOps, _: usize) Access {
        return .value;
    }

    pub fn callAccess(_: TestOps, _: @FieldType(lir.LIR.CFStmt, "assign_call"), _: CallOperand) Access {
        return .value;
    }

    pub fn isShared(_: TestOps, _: CFStmtId) bool {
        return false;
    }
};

/// Join point ids a fixture mints in order, like a lowerer's per-proc counter.
const JoinPointIds = struct {
    next: u32 = 0,

    fn fresh(self: *JoinPointIds) lir.LIR.JoinPointId {
        const id: lir.LIR.JoinPointId = @fromBackingInt(self.next);
        self.next += 1;
        return id;
    }
};

/// Test fixture: `loop(n)` swaps `a` and `b` and counts `i` up to `n`, then returns
/// `a - b`. `tmp` holds the old `a` within one iteration only.
pub const SwapLoop = struct {
    n: LocalId,
    a: LocalId,
    b: LocalId,
    i: LocalId,
    tmp: LocalId,
    body: CFStmtId,
    loop_body: CFStmtId,
    frame: LocalSpan,
    params: LocalSpan,

    pub fn build(store: *LirStore) Allocator.Error!SwapLoop {
        const local = struct {
            fn add(s: *LirStore) Allocator.Error!LocalId {
                return s.addLocal(.{ .layout_idx = .i64 });
            }
        }.add;
        const n = try local(store);
        const a = try local(store);
        const b = try local(store);
        const i = try local(store);
        const tmp = try local(store);
        const one = try local(store);
        const next_i = try local(store);
        const result = try local(store);
        const done = try store.addLocal(.{ .layout_idx = .bool });
        var join_points = JoinPointIds{};
        const loop_id = join_points.fresh();
        const write = struct {
            fn set(s: *LirStore, target: LocalId, value: LocalId, next: CFStmtId) Allocator.Error!CFStmtId {
                return s.addCFStmt(.{ .set_local = .{ .target = target, .value = value, .mode = .replace_existing, .next = next } }, .test_fixture);
            }
            fn literal(s: *LirStore, target: LocalId, value: i64, next: CFStmtId) Allocator.Error!CFStmtId {
                return s.addCFStmt(.{ .assign_literal = .{ .target = target, .value = .{ .i64_literal = .{ .value = value, .layout_idx = .i64 } }, .next = next } }, .test_fixture);
            }
        };

        const again = try store.addCFStmt(.{ .jump = .{ .target = loop_id } }, .test_fixture);
        var step = try write.set(store, i, next_i, again);
        step = try store.addLowLevelStmt(next_i, .num_int_add_wrap, &.{ i, one }, step, .test_fixture);
        step = try write.literal(store, one, 1, step);
        step = try write.set(store, b, tmp, step);
        step = try write.set(store, a, b, step);
        step = try store.addCFStmt(.{ .assign_ref = .{ .target = tmp, .op = lir.LIR.RefOp.local(a), .next = step } }, .test_fixture);
        const ret = try store.addCFStmt(.{ .ret = .{ .value = result } }, .test_fixture);
        const finish = try store.addLowLevelStmt(result, .num_int_sub_wrap, &.{ a, b }, ret, .test_fixture);
        const branch = try store.addCFStmt(.{ .switch_stmt = .{
            .cond = done,
            .branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = finish }}),
            .default_branch = step,
        } }, .test_fixture);
        const loop_body = try store.addLowLevelStmt(done, .num_is_gte, &.{ i, n }, branch, .test_fixture);

        var start = try store.addCFStmt(.{ .jump = .{ .target = loop_id } }, .test_fixture);
        start = try write.literal(store, i, 0, start);
        start = try write.literal(store, b, 2, start);
        start = try write.literal(store, a, 1, start);
        const body = try store.addCFStmt(.{ .join = .{
            .id = loop_id,
            .params = try store.addLocalSpan(&.{ a, b, i }),
            .body = loop_body,
            .remainder = start,
        } }, .test_fixture);
        return .{
            .n = n,
            .a = a,
            .b = b,
            .i = i,
            .tmp = tmp,
            .body = body,
            .loop_body = loop_body,
            .frame = try store.addLocalSpan(&.{ a, b, i, tmp, one, next_i, result, done }),
            .params = try store.addLocalSpan(&.{n}),
        };
    }
};

test "loop-carried locals merge at the loop header and single definitions dominate" {
    const gpa = std.testing.allocator;
    var store = LirStore.init(gpa);
    defer store.deinit();
    const program = try SwapLoop.build(&store);

    var value_plan = Self.init(gpa);
    defer value_plan.deinit();
    try value_plan.plan(&store, program.params, program.frame, program.body, TestOps{ .store = &store });

    // The parameter only the prologue defines, and `tmp`, defined once per
    // iteration before every read, need no phi.
    try std.testing.expectEqual(Rep.dominating, value_plan.rep(program.n));
    try std.testing.expectEqual(Rep.dominating, value_plan.rep(program.tmp));
    // The swapped pair and the counter are redefined around the back edge.
    try std.testing.expectEqual(Rep.merged, value_plan.rep(program.a));
    try std.testing.expectEqual(Rep.merged, value_plan.rep(program.b));
    try std.testing.expectEqual(Rep.merged, value_plan.rep(program.i));
    try std.testing.expectEqualSlices(LocalId, &.{ program.a, program.b, program.i }, value_plan.mergeLocals(program.loop_body));

    value_plan.clear();
    try std.testing.expectEqual(Rep.memory, value_plan.rep(program.a));
    try std.testing.expectEqual(@as(usize, 0), value_plan.mergeLocals(program.loop_body).len);
}

test "an operand the emitter reads through an address keeps a scalar in memory" {
    const gpa = std.testing.allocator;
    var store = LirStore.init(gpa);
    defer store.deinit();
    // `bits = to_i64_trunc(x)`: the wrapping builtin writes through a
    // pointer to `bits`, while it reads `x` as a value.
    const x = try store.addLocal(.{ .layout_idx = .f64 });
    const bits = try store.addLocal(.{ .layout_idx = .i64 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = bits } }, .test_fixture);
    const body = try store.addLowLevelStmt(bits, .f64_to_i64_trunc, &.{x}, ret, .test_fixture);

    var value_plan = Self.init(gpa);
    defer value_plan.deinit();
    try value_plan.plan(&store, try store.addLocalSpan(&.{x}), try store.addLocalSpan(&.{bits}), body, TestOps{ .store = &store });
    try std.testing.expectEqual(Rep.dominating, value_plan.rep(x));
    try std.testing.expectEqual(Rep.memory, value_plan.rep(bits));
}

test "a local live into a shared continuation from two arms merges there" {
    const gpa = std.testing.allocator;
    var store = LirStore.init(gpa);
    defer store.deinit();
    // `v = if c then 1 else 2; ret v`, with both arms reaching one shared
    // return statement.
    const c = try store.addLocal(.{ .layout_idx = .bool });
    const v = try store.addLocal(.{ .layout_idx = .u8 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = v } }, .test_fixture);
    const literal = struct {
        fn add(s: *LirStore, target: LocalId, value: i64, next: CFStmtId) Allocator.Error!CFStmtId {
            return s.addCFStmt(.{ .assign_literal = .{ .target = target, .value = .{ .i64_literal = .{ .value = value, .layout_idx = .u8 } }, .next = next } }, .test_fixture);
        }
    }.add;
    const then_arm = try literal(&store, v, 1, ret);
    const else_arm = try literal(&store, v, 2, ret);
    const body = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = c,
        .branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = then_arm }}),
        .default_branch = else_arm,
    } }, .test_fixture);

    const SharedReturn = struct {
        inner: TestOps,
        shared: CFStmtId,

        pub fn isCandidate(self: @This(), local: LocalId) bool {
            return self.inner.isCandidate(local);
        }
        pub fn paramAccess(self: @This(), index: usize) Access {
            return self.inner.paramAccess(index);
        }
        pub fn callAccess(self: @This(), call: @FieldType(lir.LIR.CFStmt, "assign_call"), operand: CallOperand) Access {
            return self.inner.callAccess(call, operand);
        }
        pub fn isShared(self: @This(), stmt: CFStmtId) bool {
            return stmt == self.shared;
        }
    };
    var value_plan = Self.init(gpa);
    defer value_plan.deinit();
    try value_plan.plan(&store, try store.addLocalSpan(&.{c}), try store.addLocalSpan(&.{v}), body, SharedReturn{ .inner = .{ .store = &store }, .shared = ret });
    try std.testing.expectEqual(Rep.merged, value_plan.rep(v));
    try std.testing.expectEqualSlices(LocalId, &.{v}, value_plan.mergeLocals(ret));
}
