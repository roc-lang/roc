# Generated, exhaustive cross-check of compiler SIMD low-levels against the
# independent scalar { bits : U128 } oracle. Every invocation checks every
# supported low-level/type combination over 64 deterministic pseudo-random
# vectors and the pinned edge corpus. Shift operations additionally cover all
# 256 possible U8 counts without repeating count-independent operations.

import oracle.SimdOracle

SimdDifferential := [].{

	check_u8x16 : U128, U128, U128, U8, List(U8) -> {}
	check_u8x16 = |a, b, c, count, bytes| {
		index_u8x16 = count % 16
		offset = count.to_u64() % 33
		# U8x16
		if U8x16.splat(a.to_u8_wrap()).to_u128_bits() != SimdOracle.U8x16.splat(a.to_u8_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.splat"
		} else {}
		if U8x16.to_u128_bits(U8x16.from_u128_bits(a)) != SimdOracle.U8x16.to_u128_bits(SimdOracle.U8x16.from_u128_bits(a)) {
			crash "SIMD differential mismatch: U8x16.to_u128_bits"
		} else {}
		if U8x16.from_u128_bits(a).to_u128_bits() != SimdOracle.U8x16.from_u128_bits(a).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.from_u128_bits"
		} else {}
		if U8x16.plus_wrap(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.plus_wrap(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.plus_wrap"
		} else {}
		if U8x16.minus_wrap(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.minus_wrap(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.minus_wrap"
		} else {}
		if U8x16.plus_saturated(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.plus_saturated(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.plus_saturated"
		} else {}
		if U8x16.minus_saturated(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.minus_saturated(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.minus_saturated"
		} else {}
		if U8x16.min(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.min(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.min"
		} else {}
		if U8x16.max(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.max(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.max"
		} else {}
		if U8x16.abs_diff(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.abs_diff(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.abs_diff"
		} else {}
		if U8x16.avg_rounded(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.avg_rounded(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.avg_rounded"
		} else {}
		if U8x16.bitwise_and(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.bitwise_and(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.bitwise_and"
		} else {}
		if U8x16.bitwise_or(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.bitwise_or(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.bitwise_or"
		} else {}
		if U8x16.bitwise_xor(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.bitwise_xor(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.bitwise_xor"
		} else {}
		if U8x16.bitwise_not(U8x16.from_u128_bits(a)).to_u128_bits() != SimdOracle.U8x16.bitwise_not(SimdOracle.U8x16.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.bitwise_not"
		} else {}
		if U8x16.bit_select(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b), U8x16.from_u128_bits(c)).to_u128_bits() != SimdOracle.U8x16.bit_select(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b), SimdOracle.U8x16.from_u128_bits(c)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.bit_select"
		} else {}
		if U8x16.eq_lanes(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.eq_lanes(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.eq_lanes"
		} else {}
		if U8x16.gt_lanes(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.gt_lanes(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.gt_lanes"
		} else {}
		if U8x16.gte_lanes(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.gte_lanes(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.gte_lanes"
		} else {}
		if U8x16.to_bitmask(U8x16.from_u128_bits(a)) != SimdOracle.U8x16.to_bitmask(SimdOracle.U8x16.from_u128_bits(a)) {
			crash "SIMD differential mismatch: U8x16.to_bitmask"
		} else {}
		if U8x16.shl_wrap(U8x16.from_u128_bits(a), count).to_u128_bits() != SimdOracle.U8x16.shl_wrap(SimdOracle.U8x16.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.shl_wrap"
		} else {}
		if U8x16.shr_wrap(U8x16.from_u128_bits(a), count).to_u128_bits() != SimdOracle.U8x16.shr_wrap(SimdOracle.U8x16.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.shr_wrap"
		} else {}
		if U8x16.shr_zf_wrap(U8x16.from_u128_bits(a), count).to_u128_bits() != SimdOracle.U8x16.shr_zf_wrap(SimdOracle.U8x16.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.shr_zf_wrap"
		} else {}
		if U8x16.get_lane(U8x16.from_u128_bits(a), index_u8x16.to_u64()) != SimdOracle.U8x16.get_lane(SimdOracle.U8x16.from_u128_bits(a), index_u8x16.to_u64()) {
			crash "SIMD differential mismatch: U8x16.get_lane"
		} else {}
		if U8x16.with_lane(U8x16.from_u128_bits(a), index_u8x16.to_u64(), c.to_u8_wrap()).to_u128_bits() != SimdOracle.U8x16.with_lane(SimdOracle.U8x16.from_u128_bits(a), index_u8x16.to_u64(), c.to_u8_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.with_lane"
		} else {}
		if U8x16.interleave_lo(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.interleave_lo(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.interleave_lo"
		} else {}
		if U8x16.interleave_hi(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.interleave_hi(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.interleave_hi"
		} else {}
		if U8x16.even_lanes(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.even_lanes(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.even_lanes"
		} else {}
		if U8x16.odd_lanes(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.odd_lanes(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.odd_lanes"
		} else {}
		if U8x16.reverse_lanes(U8x16.from_u128_bits(a)).to_u128_bits() != SimdOracle.U8x16.reverse_lanes(SimdOracle.U8x16.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.reverse_lanes"
		} else {}
		if U8x16.concat_shift_bytes(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b), count % 17).to_u128_bits() != SimdOracle.U8x16.concat_shift_bytes(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b), count % 17).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.concat_shift_bytes"
		} else {}
		if U8x16.table_lookup(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.table_lookup(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.table_lookup"
		} else {}
		if U8x16.to_u16x8_lo(U8x16.from_u128_bits(a)).to_u128_bits() != SimdOracle.U8x16.to_u16x8_lo(SimdOracle.U8x16.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.to_u16x8_lo"
		} else {}
		if U8x16.to_u16x8_hi(U8x16.from_u128_bits(a)).to_u128_bits() != SimdOracle.U8x16.to_u16x8_hi(SimdOracle.U8x16.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.to_u16x8_hi"
		} else {}
		if U8x16.pairwise_plus_to_u16x8(U8x16.from_u128_bits(a)).to_u128_bits() != SimdOracle.U8x16.pairwise_plus_to_u16x8(SimdOracle.U8x16.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.pairwise_plus_to_u16x8"
		} else {}
		if U8x16.times_wide_lo(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.times_wide_lo(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.times_wide_lo"
		} else {}
		if U8x16.times_wide_hi(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.times_wide_hi(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.times_wide_hi"
		} else {}
		if U8x16.dot_pairs_saturated(U8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.dot_pairs_saturated(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.dot_pairs_saturated"
		} else {}
		if U8x16.sums_of_abs_diffs(U8x16.from_u128_bits(a), U8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.U8x16.sums_of_abs_diffs(SimdOracle.U8x16.from_u128_bits(a), SimdOracle.U8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U8x16.sums_of_abs_diffs"
		} else {}
		if U8x16.sum_lanes(U8x16.from_u128_bits(a)) != SimdOracle.U8x16.sum_lanes(SimdOracle.U8x16.from_u128_bits(a)) {
			crash "SIMD differential mismatch: U8x16.sum_lanes"
		} else {}
		if U8x16.store(U8x16.from_u128_bits(a), bytes, offset) != SimdOracle.U8x16.store(SimdOracle.U8x16.from_u128_bits(a), bytes, offset) {
			crash "SIMD differential mismatch: U8x16.store"
		} else {}
		if U8x16.append_to(U8x16.from_u128_bits(a), bytes) != SimdOracle.U8x16.append_to(SimdOracle.U8x16.from_u128_bits(a), bytes) {
			crash "SIMD differential mismatch: U8x16.append_to"
		} else {}
		{}
	}

	check_i8x16 : U128, U128, U128, U8, List(U8) -> {}
	check_i8x16 = |a, b, c, count, bytes| {
		index_i8x16 = count % 16
		offset = count.to_u64() % 33
		# I8x16
		if I8x16.splat(a.to_i8_wrap()).to_u128_bits() != SimdOracle.I8x16.splat(a.to_i8_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.splat"
		} else {}
		if I8x16.to_u128_bits(I8x16.from_u128_bits(a)) != SimdOracle.I8x16.to_u128_bits(SimdOracle.I8x16.from_u128_bits(a)) {
			crash "SIMD differential mismatch: I8x16.to_u128_bits"
		} else {}
		if I8x16.from_u128_bits(a).to_u128_bits() != SimdOracle.I8x16.from_u128_bits(a).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.from_u128_bits"
		} else {}
		if I8x16.plus_wrap(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.plus_wrap(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.plus_wrap"
		} else {}
		if I8x16.minus_wrap(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.minus_wrap(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.minus_wrap"
		} else {}
		if I8x16.plus_saturated(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.plus_saturated(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.plus_saturated"
		} else {}
		if I8x16.minus_saturated(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.minus_saturated(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.minus_saturated"
		} else {}
		if I8x16.negate_wrap(I8x16.from_u128_bits(a)).to_u128_bits() != SimdOracle.I8x16.negate_wrap(SimdOracle.I8x16.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.negate_wrap"
		} else {}
		if I8x16.abs_wrap(I8x16.from_u128_bits(a)).to_u128_bits() != SimdOracle.I8x16.abs_wrap(SimdOracle.I8x16.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.abs_wrap"
		} else {}
		if I8x16.min(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.min(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.min"
		} else {}
		if I8x16.max(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.max(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.max"
		} else {}
		if I8x16.bitwise_and(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.bitwise_and(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.bitwise_and"
		} else {}
		if I8x16.bitwise_or(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.bitwise_or(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.bitwise_or"
		} else {}
		if I8x16.bitwise_xor(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.bitwise_xor(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.bitwise_xor"
		} else {}
		if I8x16.bitwise_not(I8x16.from_u128_bits(a)).to_u128_bits() != SimdOracle.I8x16.bitwise_not(SimdOracle.I8x16.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.bitwise_not"
		} else {}
		if I8x16.bit_select(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b), I8x16.from_u128_bits(c)).to_u128_bits() != SimdOracle.I8x16.bit_select(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b), SimdOracle.I8x16.from_u128_bits(c)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.bit_select"
		} else {}
		if I8x16.eq_lanes(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.eq_lanes(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.eq_lanes"
		} else {}
		if I8x16.gt_lanes(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.gt_lanes(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.gt_lanes"
		} else {}
		if I8x16.gte_lanes(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.gte_lanes(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.gte_lanes"
		} else {}
		if I8x16.to_bitmask(I8x16.from_u128_bits(a)) != SimdOracle.I8x16.to_bitmask(SimdOracle.I8x16.from_u128_bits(a)) {
			crash "SIMD differential mismatch: I8x16.to_bitmask"
		} else {}
		if I8x16.shl_wrap(I8x16.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I8x16.shl_wrap(SimdOracle.I8x16.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.shl_wrap"
		} else {}
		if I8x16.shr_zf_wrap(I8x16.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I8x16.shr_zf_wrap(SimdOracle.I8x16.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.shr_zf_wrap"
		} else {}
		if I8x16.shr_wrap(I8x16.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I8x16.shr_wrap(SimdOracle.I8x16.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.shr_wrap"
		} else {}
		if I8x16.get_lane(I8x16.from_u128_bits(a), index_i8x16.to_u64()) != SimdOracle.I8x16.get_lane(SimdOracle.I8x16.from_u128_bits(a), index_i8x16.to_u64()) {
			crash "SIMD differential mismatch: I8x16.get_lane"
		} else {}
		if I8x16.with_lane(I8x16.from_u128_bits(a), index_i8x16.to_u64(), c.to_i8_wrap()).to_u128_bits() != SimdOracle.I8x16.with_lane(SimdOracle.I8x16.from_u128_bits(a), index_i8x16.to_u64(), c.to_i8_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.with_lane"
		} else {}
		if I8x16.interleave_lo(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.interleave_lo(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.interleave_lo"
		} else {}
		if I8x16.interleave_hi(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.interleave_hi(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.interleave_hi"
		} else {}
		if I8x16.even_lanes(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.even_lanes(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.even_lanes"
		} else {}
		if I8x16.odd_lanes(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.odd_lanes(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.odd_lanes"
		} else {}
		if I8x16.reverse_lanes(I8x16.from_u128_bits(a)).to_u128_bits() != SimdOracle.I8x16.reverse_lanes(SimdOracle.I8x16.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.reverse_lanes"
		} else {}
		if I8x16.to_i16x8_lo(I8x16.from_u128_bits(a)).to_u128_bits() != SimdOracle.I8x16.to_i16x8_lo(SimdOracle.I8x16.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.to_i16x8_lo"
		} else {}
		if I8x16.to_i16x8_hi(I8x16.from_u128_bits(a)).to_u128_bits() != SimdOracle.I8x16.to_i16x8_hi(SimdOracle.I8x16.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.to_i16x8_hi"
		} else {}
		if I8x16.pairwise_plus_to_i16x8(I8x16.from_u128_bits(a)).to_u128_bits() != SimdOracle.I8x16.pairwise_plus_to_i16x8(SimdOracle.I8x16.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.pairwise_plus_to_i16x8"
		} else {}
		if I8x16.times_wide_lo(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.times_wide_lo(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.times_wide_lo"
		} else {}
		if I8x16.times_wide_hi(I8x16.from_u128_bits(a), I8x16.from_u128_bits(b)).to_u128_bits() != SimdOracle.I8x16.times_wide_hi(SimdOracle.I8x16.from_u128_bits(a), SimdOracle.I8x16.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I8x16.times_wide_hi"
		} else {}
		if I8x16.sum_lanes(I8x16.from_u128_bits(a)) != SimdOracle.I8x16.sum_lanes(SimdOracle.I8x16.from_u128_bits(a)) {
			crash "SIMD differential mismatch: I8x16.sum_lanes"
		} else {}
		if I8x16.store(I8x16.from_u128_bits(a), bytes, offset) != SimdOracle.I8x16.store(SimdOracle.I8x16.from_u128_bits(a), bytes, offset) {
			crash "SIMD differential mismatch: I8x16.store"
		} else {}
		if I8x16.append_to(I8x16.from_u128_bits(a), bytes) != SimdOracle.I8x16.append_to(SimdOracle.I8x16.from_u128_bits(a), bytes) {
			crash "SIMD differential mismatch: I8x16.append_to"
		} else {}
		{}
	}

	check_u16x8 : U128, U128, U128, U8, List(U8) -> {}
	check_u16x8 = |a, b, c, count, bytes| {
		index_u16x8 = count % 8
		offset = count.to_u64() % 33
		# U16x8
		if U16x8.splat(a.to_u16_wrap()).to_u128_bits() != SimdOracle.U16x8.splat(a.to_u16_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.splat"
		} else {}
		if U16x8.to_u128_bits(U16x8.from_u128_bits(a)) != SimdOracle.U16x8.to_u128_bits(SimdOracle.U16x8.from_u128_bits(a)) {
			crash "SIMD differential mismatch: U16x8.to_u128_bits"
		} else {}
		if U16x8.from_u128_bits(a).to_u128_bits() != SimdOracle.U16x8.from_u128_bits(a).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.from_u128_bits"
		} else {}
		if U16x8.plus_wrap(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.plus_wrap(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.plus_wrap"
		} else {}
		if U16x8.minus_wrap(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.minus_wrap(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.minus_wrap"
		} else {}
		if U16x8.plus_saturated(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.plus_saturated(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.plus_saturated"
		} else {}
		if U16x8.minus_saturated(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.minus_saturated(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.minus_saturated"
		} else {}
		if U16x8.min(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.min(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.min"
		} else {}
		if U16x8.max(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.max(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.max"
		} else {}
		if U16x8.abs_diff(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.abs_diff(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.abs_diff"
		} else {}
		if U16x8.avg_rounded(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.avg_rounded(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.avg_rounded"
		} else {}
		if U16x8.times_wrap(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.times_wrap(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.times_wrap"
		} else {}
		if U16x8.times_high(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.times_high(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.times_high"
		} else {}
		if U16x8.bitwise_and(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.bitwise_and(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.bitwise_and"
		} else {}
		if U16x8.bitwise_or(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.bitwise_or(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.bitwise_or"
		} else {}
		if U16x8.bitwise_xor(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.bitwise_xor(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.bitwise_xor"
		} else {}
		if U16x8.bitwise_not(U16x8.from_u128_bits(a)).to_u128_bits() != SimdOracle.U16x8.bitwise_not(SimdOracle.U16x8.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.bitwise_not"
		} else {}
		if U16x8.bit_select(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b), U16x8.from_u128_bits(c)).to_u128_bits() != SimdOracle.U16x8.bit_select(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b), SimdOracle.U16x8.from_u128_bits(c)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.bit_select"
		} else {}
		if U16x8.eq_lanes(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.eq_lanes(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.eq_lanes"
		} else {}
		if U16x8.gt_lanes(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.gt_lanes(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.gt_lanes"
		} else {}
		if U16x8.gte_lanes(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.gte_lanes(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.gte_lanes"
		} else {}
		if U16x8.to_bitmask(U16x8.from_u128_bits(a)) != SimdOracle.U16x8.to_bitmask(SimdOracle.U16x8.from_u128_bits(a)) {
			crash "SIMD differential mismatch: U16x8.to_bitmask"
		} else {}
		if U16x8.shl_wrap(U16x8.from_u128_bits(a), count).to_u128_bits() != SimdOracle.U16x8.shl_wrap(SimdOracle.U16x8.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.shl_wrap"
		} else {}
		if U16x8.shr_wrap(U16x8.from_u128_bits(a), count).to_u128_bits() != SimdOracle.U16x8.shr_wrap(SimdOracle.U16x8.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.shr_wrap"
		} else {}
		if U16x8.shr_zf_wrap(U16x8.from_u128_bits(a), count).to_u128_bits() != SimdOracle.U16x8.shr_zf_wrap(SimdOracle.U16x8.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.shr_zf_wrap"
		} else {}
		if U16x8.get_lane(U16x8.from_u128_bits(a), index_u16x8.to_u64()) != SimdOracle.U16x8.get_lane(SimdOracle.U16x8.from_u128_bits(a), index_u16x8.to_u64()) {
			crash "SIMD differential mismatch: U16x8.get_lane"
		} else {}
		if U16x8.with_lane(U16x8.from_u128_bits(a), index_u16x8.to_u64(), c.to_u16_wrap()).to_u128_bits() != SimdOracle.U16x8.with_lane(SimdOracle.U16x8.from_u128_bits(a), index_u16x8.to_u64(), c.to_u16_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.with_lane"
		} else {}
		if U16x8.interleave_lo(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.interleave_lo(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.interleave_lo"
		} else {}
		if U16x8.interleave_hi(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.interleave_hi(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.interleave_hi"
		} else {}
		if U16x8.even_lanes(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.even_lanes(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.even_lanes"
		} else {}
		if U16x8.odd_lanes(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.odd_lanes(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.odd_lanes"
		} else {}
		if U16x8.reverse_lanes(U16x8.from_u128_bits(a)).to_u128_bits() != SimdOracle.U16x8.reverse_lanes(SimdOracle.U16x8.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.reverse_lanes"
		} else {}
		if U16x8.to_u32x4_lo(U16x8.from_u128_bits(a)).to_u128_bits() != SimdOracle.U16x8.to_u32x4_lo(SimdOracle.U16x8.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.to_u32x4_lo"
		} else {}
		if U16x8.to_u32x4_hi(U16x8.from_u128_bits(a)).to_u128_bits() != SimdOracle.U16x8.to_u32x4_hi(SimdOracle.U16x8.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.to_u32x4_hi"
		} else {}
		if U16x8.pairwise_plus_to_u32x4(U16x8.from_u128_bits(a)).to_u128_bits() != SimdOracle.U16x8.pairwise_plus_to_u32x4(SimdOracle.U16x8.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.pairwise_plus_to_u32x4"
		} else {}
		if U16x8.times_wide_lo(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.times_wide_lo(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.times_wide_lo"
		} else {}
		if U16x8.times_wide_hi(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.times_wide_hi(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.times_wide_hi"
		} else {}
		if U16x8.narrow_to_u8x16_wrap(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.narrow_to_u8x16_wrap(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.narrow_to_u8x16_wrap"
		} else {}
		if U16x8.narrow_to_u8x16_saturated(U16x8.from_u128_bits(a), U16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.U16x8.narrow_to_u8x16_saturated(SimdOracle.U16x8.from_u128_bits(a), SimdOracle.U16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U16x8.narrow_to_u8x16_saturated"
		} else {}
		if U16x8.sum_lanes(U16x8.from_u128_bits(a)) != SimdOracle.U16x8.sum_lanes(SimdOracle.U16x8.from_u128_bits(a)) {
			crash "SIMD differential mismatch: U16x8.sum_lanes"
		} else {}
		if U16x8.store(U16x8.from_u128_bits(a), bytes, offset) != SimdOracle.U16x8.store(SimdOracle.U16x8.from_u128_bits(a), bytes, offset) {
			crash "SIMD differential mismatch: U16x8.store"
		} else {}
		if U16x8.append_to(U16x8.from_u128_bits(a), bytes) != SimdOracle.U16x8.append_to(SimdOracle.U16x8.from_u128_bits(a), bytes) {
			crash "SIMD differential mismatch: U16x8.append_to"
		} else {}
		{}
	}

	check_i16x8 : U128, U128, U128, U8, List(U8) -> {}
	check_i16x8 = |a, b, c, count, bytes| {
		index_i16x8 = count % 8
		offset = count.to_u64() % 33
		# I16x8
		if I16x8.splat(a.to_i16_wrap()).to_u128_bits() != SimdOracle.I16x8.splat(a.to_i16_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.splat"
		} else {}
		if I16x8.to_u128_bits(I16x8.from_u128_bits(a)) != SimdOracle.I16x8.to_u128_bits(SimdOracle.I16x8.from_u128_bits(a)) {
			crash "SIMD differential mismatch: I16x8.to_u128_bits"
		} else {}
		if I16x8.from_u128_bits(a).to_u128_bits() != SimdOracle.I16x8.from_u128_bits(a).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.from_u128_bits"
		} else {}
		if I16x8.plus_wrap(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.plus_wrap(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.plus_wrap"
		} else {}
		if I16x8.minus_wrap(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.minus_wrap(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.minus_wrap"
		} else {}
		if I16x8.plus_saturated(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.plus_saturated(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.plus_saturated"
		} else {}
		if I16x8.minus_saturated(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.minus_saturated(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.minus_saturated"
		} else {}
		if I16x8.negate_wrap(I16x8.from_u128_bits(a)).to_u128_bits() != SimdOracle.I16x8.negate_wrap(SimdOracle.I16x8.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.negate_wrap"
		} else {}
		if I16x8.abs_wrap(I16x8.from_u128_bits(a)).to_u128_bits() != SimdOracle.I16x8.abs_wrap(SimdOracle.I16x8.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.abs_wrap"
		} else {}
		if I16x8.min(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.min(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.min"
		} else {}
		if I16x8.max(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.max(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.max"
		} else {}
		if I16x8.times_wrap(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.times_wrap(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.times_wrap"
		} else {}
		if I16x8.times_high(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.times_high(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.times_high"
		} else {}
		if I16x8.times_fixed_q15_saturated(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.times_fixed_q15_saturated(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.times_fixed_q15_saturated"
		} else {}
		if I16x8.dot_pairs(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.dot_pairs(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.dot_pairs"
		} else {}
		if I16x8.bitwise_and(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.bitwise_and(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.bitwise_and"
		} else {}
		if I16x8.bitwise_or(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.bitwise_or(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.bitwise_or"
		} else {}
		if I16x8.bitwise_xor(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.bitwise_xor(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.bitwise_xor"
		} else {}
		if I16x8.bitwise_not(I16x8.from_u128_bits(a)).to_u128_bits() != SimdOracle.I16x8.bitwise_not(SimdOracle.I16x8.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.bitwise_not"
		} else {}
		if I16x8.bit_select(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b), I16x8.from_u128_bits(c)).to_u128_bits() != SimdOracle.I16x8.bit_select(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b), SimdOracle.I16x8.from_u128_bits(c)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.bit_select"
		} else {}
		if I16x8.eq_lanes(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.eq_lanes(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.eq_lanes"
		} else {}
		if I16x8.gt_lanes(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.gt_lanes(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.gt_lanes"
		} else {}
		if I16x8.gte_lanes(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.gte_lanes(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.gte_lanes"
		} else {}
		if I16x8.to_bitmask(I16x8.from_u128_bits(a)) != SimdOracle.I16x8.to_bitmask(SimdOracle.I16x8.from_u128_bits(a)) {
			crash "SIMD differential mismatch: I16x8.to_bitmask"
		} else {}
		if I16x8.shl_wrap(I16x8.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I16x8.shl_wrap(SimdOracle.I16x8.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.shl_wrap"
		} else {}
		if I16x8.shr_wrap(I16x8.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I16x8.shr_wrap(SimdOracle.I16x8.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.shr_wrap"
		} else {}
		if I16x8.shr_zf_wrap(I16x8.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I16x8.shr_zf_wrap(SimdOracle.I16x8.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.shr_zf_wrap"
		} else {}
		if I16x8.shift_right_rounded_by(I16x8.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I16x8.shift_right_rounded_by(SimdOracle.I16x8.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.shift_right_rounded_by"
		} else {}
		if I16x8.narrow_to_i8x16_saturated(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.narrow_to_i8x16_saturated(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.narrow_to_i8x16_saturated"
		} else {}
		if I16x8.narrow_to_u8x16_saturated(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.narrow_to_u8x16_saturated(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.narrow_to_u8x16_saturated"
		} else {}
		if I16x8.to_i32x4_lo(I16x8.from_u128_bits(a)).to_u128_bits() != SimdOracle.I16x8.to_i32x4_lo(SimdOracle.I16x8.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.to_i32x4_lo"
		} else {}
		if I16x8.to_i32x4_hi(I16x8.from_u128_bits(a)).to_u128_bits() != SimdOracle.I16x8.to_i32x4_hi(SimdOracle.I16x8.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.to_i32x4_hi"
		} else {}
		if I16x8.pairwise_plus_to_i32x4(I16x8.from_u128_bits(a)).to_u128_bits() != SimdOracle.I16x8.pairwise_plus_to_i32x4(SimdOracle.I16x8.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.pairwise_plus_to_i32x4"
		} else {}
		if I16x8.times_wide_lo(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.times_wide_lo(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.times_wide_lo"
		} else {}
		if I16x8.times_wide_hi(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.times_wide_hi(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.times_wide_hi"
		} else {}
		if I16x8.sum_lanes(I16x8.from_u128_bits(a)) != SimdOracle.I16x8.sum_lanes(SimdOracle.I16x8.from_u128_bits(a)) {
			crash "SIMD differential mismatch: I16x8.sum_lanes"
		} else {}
		if I16x8.get_lane(I16x8.from_u128_bits(a), index_i16x8.to_u64()) != SimdOracle.I16x8.get_lane(SimdOracle.I16x8.from_u128_bits(a), index_i16x8.to_u64()) {
			crash "SIMD differential mismatch: I16x8.get_lane"
		} else {}
		if I16x8.with_lane(I16x8.from_u128_bits(a), index_i16x8.to_u64(), c.to_i16_wrap()).to_u128_bits() != SimdOracle.I16x8.with_lane(SimdOracle.I16x8.from_u128_bits(a), index_i16x8.to_u64(), c.to_i16_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.with_lane"
		} else {}
		if I16x8.interleave_lo(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.interleave_lo(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.interleave_lo"
		} else {}
		if I16x8.interleave_hi(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.interleave_hi(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.interleave_hi"
		} else {}
		if I16x8.even_lanes(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.even_lanes(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.even_lanes"
		} else {}
		if I16x8.odd_lanes(I16x8.from_u128_bits(a), I16x8.from_u128_bits(b)).to_u128_bits() != SimdOracle.I16x8.odd_lanes(SimdOracle.I16x8.from_u128_bits(a), SimdOracle.I16x8.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.odd_lanes"
		} else {}
		if I16x8.reverse_lanes(I16x8.from_u128_bits(a)).to_u128_bits() != SimdOracle.I16x8.reverse_lanes(SimdOracle.I16x8.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I16x8.reverse_lanes"
		} else {}
		match (I16x8.store(I16x8.from_u128_bits(a), bytes, offset), SimdOracle.I16x8.store(SimdOracle.I16x8.from_u128_bits(a), bytes, offset)) {
			(Ok(native), Ok(reference)) =>
				if native == reference {
					{}
				} else if List.get(native, offset) != List.get(reference, offset) {
					if List.get(native, offset) != Ok(a.to_u8_wrap()) {
						crash "SIMD differential mismatch: I16x8.store native first byte"
					} else {
						crash "SIMD differential mismatch: I16x8.store oracle first byte"
					}
				} else if List.get(native, offset + 15) != List.get(reference, offset + 15) {
					crash "SIMD differential mismatch: I16x8.store last byte"
				} else if List.get(native, 0) != List.get(reference, 0) {
					crash "SIMD differential mismatch: I16x8.store prefix"
				} else {
					crash "SIMD differential mismatch: I16x8.store suffix"
				}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: I16x8.store bounds"
			}
		}
		if I16x8.append_to(I16x8.from_u128_bits(a), bytes) != SimdOracle.I16x8.append_to(SimdOracle.I16x8.from_u128_bits(a), bytes) {
			crash "SIMD differential mismatch: I16x8.append_to"
		} else {}
		{}
	}

	check_u32x4 : U128, U128, U128, U8, List(U8) -> {}
	check_u32x4 = |a, b, c, count, bytes| {
		index_u32x4 = count % 4
		offset = count.to_u64() % 33
		# U32x4
		if U32x4.splat(a.to_u32_wrap()).to_u128_bits() != SimdOracle.U32x4.splat(a.to_u32_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.splat"
		} else {}
		if U32x4.to_u128_bits(U32x4.from_u128_bits(a)) != SimdOracle.U32x4.to_u128_bits(SimdOracle.U32x4.from_u128_bits(a)) {
			crash "SIMD differential mismatch: U32x4.to_u128_bits"
		} else {}
		if U32x4.from_u128_bits(a).to_u128_bits() != SimdOracle.U32x4.from_u128_bits(a).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.from_u128_bits"
		} else {}
		if U32x4.plus_wrap(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.plus_wrap(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.plus_wrap"
		} else {}
		if U32x4.minus_wrap(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.minus_wrap(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.minus_wrap"
		} else {}
		if U32x4.min(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.min(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.min"
		} else {}
		if U32x4.max(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.max(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.max"
		} else {}
		if U32x4.times_wrap(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.times_wrap(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.times_wrap"
		} else {}
		if U32x4.times_wide_lo(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.times_wide_lo(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.times_wide_lo"
		} else {}
		if U32x4.times_wide_hi(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.times_wide_hi(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.times_wide_hi"
		} else {}
		if U32x4.bitwise_and(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.bitwise_and(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.bitwise_and"
		} else {}
		if U32x4.bitwise_or(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.bitwise_or(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.bitwise_or"
		} else {}
		if U32x4.bitwise_xor(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.bitwise_xor(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.bitwise_xor"
		} else {}
		if U32x4.bitwise_not(U32x4.from_u128_bits(a)).to_u128_bits() != SimdOracle.U32x4.bitwise_not(SimdOracle.U32x4.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.bitwise_not"
		} else {}
		if U32x4.bit_select(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b), U32x4.from_u128_bits(c)).to_u128_bits() != SimdOracle.U32x4.bit_select(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b), SimdOracle.U32x4.from_u128_bits(c)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.bit_select"
		} else {}
		if U32x4.eq_lanes(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.eq_lanes(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.eq_lanes"
		} else {}
		if U32x4.gt_lanes(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.gt_lanes(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.gt_lanes"
		} else {}
		if U32x4.gte_lanes(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.gte_lanes(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.gte_lanes"
		} else {}
		if U32x4.to_bitmask(U32x4.from_u128_bits(a)) != SimdOracle.U32x4.to_bitmask(SimdOracle.U32x4.from_u128_bits(a)) {
			crash "SIMD differential mismatch: U32x4.to_bitmask"
		} else {}
		if U32x4.shl_wrap(U32x4.from_u128_bits(a), count).to_u128_bits() != SimdOracle.U32x4.shl_wrap(SimdOracle.U32x4.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.shl_wrap"
		} else {}
		if U32x4.shr_wrap(U32x4.from_u128_bits(a), count).to_u128_bits() != SimdOracle.U32x4.shr_wrap(SimdOracle.U32x4.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.shr_wrap"
		} else {}
		if U32x4.shr_zf_wrap(U32x4.from_u128_bits(a), count).to_u128_bits() != SimdOracle.U32x4.shr_zf_wrap(SimdOracle.U32x4.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.shr_zf_wrap"
		} else {}
		if U32x4.get_lane(U32x4.from_u128_bits(a), index_u32x4.to_u64()) != SimdOracle.U32x4.get_lane(SimdOracle.U32x4.from_u128_bits(a), index_u32x4.to_u64()) {
			crash "SIMD differential mismatch: U32x4.get_lane"
		} else {}
		if U32x4.with_lane(U32x4.from_u128_bits(a), index_u32x4.to_u64(), c.to_u32_wrap()).to_u128_bits() != SimdOracle.U32x4.with_lane(SimdOracle.U32x4.from_u128_bits(a), index_u32x4.to_u64(), c.to_u32_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.with_lane"
		} else {}
		if U32x4.interleave_lo(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.interleave_lo(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.interleave_lo"
		} else {}
		if U32x4.interleave_hi(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.interleave_hi(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.interleave_hi"
		} else {}
		if U32x4.even_lanes(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.even_lanes(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.even_lanes"
		} else {}
		if U32x4.odd_lanes(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.odd_lanes(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.odd_lanes"
		} else {}
		if U32x4.reverse_lanes(U32x4.from_u128_bits(a)).to_u128_bits() != SimdOracle.U32x4.reverse_lanes(SimdOracle.U32x4.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.reverse_lanes"
		} else {}
		if U32x4.to_u64x2_lo(U32x4.from_u128_bits(a)).to_u128_bits() != SimdOracle.U32x4.to_u64x2_lo(SimdOracle.U32x4.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.to_u64x2_lo"
		} else {}
		if U32x4.to_u64x2_hi(U32x4.from_u128_bits(a)).to_u128_bits() != SimdOracle.U32x4.to_u64x2_hi(SimdOracle.U32x4.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.to_u64x2_hi"
		} else {}
		if U32x4.narrow_to_u16x8_wrap(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.narrow_to_u16x8_wrap(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.narrow_to_u16x8_wrap"
		} else {}
		if U32x4.narrow_to_u16x8_saturated(U32x4.from_u128_bits(a), U32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.U32x4.narrow_to_u16x8_saturated(SimdOracle.U32x4.from_u128_bits(a), SimdOracle.U32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U32x4.narrow_to_u16x8_saturated"
		} else {}
		if U32x4.sum_lanes(U32x4.from_u128_bits(a)) != SimdOracle.U32x4.sum_lanes(SimdOracle.U32x4.from_u128_bits(a)) {
			crash "SIMD differential mismatch: U32x4.sum_lanes"
		} else {}
		if U32x4.store(U32x4.from_u128_bits(a), bytes, offset) != SimdOracle.U32x4.store(SimdOracle.U32x4.from_u128_bits(a), bytes, offset) {
			crash "SIMD differential mismatch: U32x4.store"
		} else {}
		if U32x4.append_to(U32x4.from_u128_bits(a), bytes) != SimdOracle.U32x4.append_to(SimdOracle.U32x4.from_u128_bits(a), bytes) {
			crash "SIMD differential mismatch: U32x4.append_to"
		} else {}
		{}
	}

	check_i32x4 : U128, U128, U128, U8, List(U8) -> {}
	check_i32x4 = |a, b, c, count, bytes| {
		index_i32x4 = count % 4
		offset = count.to_u64() % 33
		# I32x4
		if I32x4.splat(a.to_i32_wrap()).to_u128_bits() != SimdOracle.I32x4.splat(a.to_i32_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.splat"
		} else {}
		if I32x4.to_u128_bits(I32x4.from_u128_bits(a)) != SimdOracle.I32x4.to_u128_bits(SimdOracle.I32x4.from_u128_bits(a)) {
			crash "SIMD differential mismatch: I32x4.to_u128_bits"
		} else {}
		if I32x4.from_u128_bits(a).to_u128_bits() != SimdOracle.I32x4.from_u128_bits(a).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.from_u128_bits"
		} else {}
		if I32x4.plus_wrap(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.plus_wrap(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.plus_wrap"
		} else {}
		if I32x4.minus_wrap(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.minus_wrap(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.minus_wrap"
		} else {}
		if I32x4.negate_wrap(I32x4.from_u128_bits(a)).to_u128_bits() != SimdOracle.I32x4.negate_wrap(SimdOracle.I32x4.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.negate_wrap"
		} else {}
		if I32x4.abs_wrap(I32x4.from_u128_bits(a)).to_u128_bits() != SimdOracle.I32x4.abs_wrap(SimdOracle.I32x4.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.abs_wrap"
		} else {}
		if I32x4.min(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.min(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.min"
		} else {}
		if I32x4.max(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.max(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.max"
		} else {}
		if I32x4.times_wrap(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.times_wrap(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.times_wrap"
		} else {}
		if I32x4.times_wide_lo(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.times_wide_lo(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.times_wide_lo"
		} else {}
		if I32x4.times_wide_hi(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.times_wide_hi(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.times_wide_hi"
		} else {}
		if I32x4.bitwise_and(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.bitwise_and(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.bitwise_and"
		} else {}
		if I32x4.bitwise_or(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.bitwise_or(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.bitwise_or"
		} else {}
		if I32x4.bitwise_xor(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.bitwise_xor(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.bitwise_xor"
		} else {}
		if I32x4.bitwise_not(I32x4.from_u128_bits(a)).to_u128_bits() != SimdOracle.I32x4.bitwise_not(SimdOracle.I32x4.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.bitwise_not"
		} else {}
		if I32x4.bit_select(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b), I32x4.from_u128_bits(c)).to_u128_bits() != SimdOracle.I32x4.bit_select(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b), SimdOracle.I32x4.from_u128_bits(c)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.bit_select"
		} else {}
		if I32x4.eq_lanes(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.eq_lanes(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.eq_lanes"
		} else {}
		if I32x4.gt_lanes(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.gt_lanes(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.gt_lanes"
		} else {}
		if I32x4.gte_lanes(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.gte_lanes(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.gte_lanes"
		} else {}
		if I32x4.to_bitmask(I32x4.from_u128_bits(a)) != SimdOracle.I32x4.to_bitmask(SimdOracle.I32x4.from_u128_bits(a)) {
			crash "SIMD differential mismatch: I32x4.to_bitmask"
		} else {}
		if I32x4.shl_wrap(I32x4.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I32x4.shl_wrap(SimdOracle.I32x4.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.shl_wrap"
		} else {}
		if I32x4.shr_wrap(I32x4.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I32x4.shr_wrap(SimdOracle.I32x4.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.shr_wrap"
		} else {}
		if I32x4.shr_zf_wrap(I32x4.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I32x4.shr_zf_wrap(SimdOracle.I32x4.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.shr_zf_wrap"
		} else {}
		if I32x4.shift_right_rounded_by(I32x4.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I32x4.shift_right_rounded_by(SimdOracle.I32x4.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.shift_right_rounded_by"
		} else {}
		if I32x4.narrow_to_i16x8_saturated(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.narrow_to_i16x8_saturated(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.narrow_to_i16x8_saturated"
		} else {}
		if I32x4.narrow_to_u16x8_saturated(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.narrow_to_u16x8_saturated(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.narrow_to_u16x8_saturated"
		} else {}
		if I32x4.to_i64x2_lo(I32x4.from_u128_bits(a)).to_u128_bits() != SimdOracle.I32x4.to_i64x2_lo(SimdOracle.I32x4.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.to_i64x2_lo"
		} else {}
		if I32x4.to_i64x2_hi(I32x4.from_u128_bits(a)).to_u128_bits() != SimdOracle.I32x4.to_i64x2_hi(SimdOracle.I32x4.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.to_i64x2_hi"
		} else {}
		if I32x4.sum_lanes(I32x4.from_u128_bits(a)) != SimdOracle.I32x4.sum_lanes(SimdOracle.I32x4.from_u128_bits(a)) {
			crash "SIMD differential mismatch: I32x4.sum_lanes"
		} else {}
		if I32x4.get_lane(I32x4.from_u128_bits(a), index_i32x4.to_u64()) != SimdOracle.I32x4.get_lane(SimdOracle.I32x4.from_u128_bits(a), index_i32x4.to_u64()) {
			crash "SIMD differential mismatch: I32x4.get_lane"
		} else {}
		if I32x4.with_lane(I32x4.from_u128_bits(a), index_i32x4.to_u64(), c.to_i32_wrap()).to_u128_bits() != SimdOracle.I32x4.with_lane(SimdOracle.I32x4.from_u128_bits(a), index_i32x4.to_u64(), c.to_i32_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.with_lane"
		} else {}
		if I32x4.interleave_lo(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.interleave_lo(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.interleave_lo"
		} else {}
		if I32x4.interleave_hi(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.interleave_hi(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.interleave_hi"
		} else {}
		if I32x4.even_lanes(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.even_lanes(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.even_lanes"
		} else {}
		if I32x4.odd_lanes(I32x4.from_u128_bits(a), I32x4.from_u128_bits(b)).to_u128_bits() != SimdOracle.I32x4.odd_lanes(SimdOracle.I32x4.from_u128_bits(a), SimdOracle.I32x4.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.odd_lanes"
		} else {}
		if I32x4.reverse_lanes(I32x4.from_u128_bits(a)).to_u128_bits() != SimdOracle.I32x4.reverse_lanes(SimdOracle.I32x4.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I32x4.reverse_lanes"
		} else {}
		if I32x4.store(I32x4.from_u128_bits(a), bytes, offset) != SimdOracle.I32x4.store(SimdOracle.I32x4.from_u128_bits(a), bytes, offset) {
			crash "SIMD differential mismatch: I32x4.store"
		} else {}
		if I32x4.append_to(I32x4.from_u128_bits(a), bytes) != SimdOracle.I32x4.append_to(SimdOracle.I32x4.from_u128_bits(a), bytes) {
			crash "SIMD differential mismatch: I32x4.append_to"
		} else {}
		{}
	}

	check_u64x2 : U128, U128, U128, U8, List(U8) -> {}
	check_u64x2 = |a, b, c, count, bytes| {
		index_u64x2 = count % 2
		offset = count.to_u64() % 33
		# U64x2
		if U64x2.splat(a.to_u64_wrap()).to_u128_bits() != SimdOracle.U64x2.splat(a.to_u64_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.splat"
		} else {}
		if U64x2.to_u128_bits(U64x2.from_u128_bits(a)) != SimdOracle.U64x2.to_u128_bits(SimdOracle.U64x2.from_u128_bits(a)) {
			crash "SIMD differential mismatch: U64x2.to_u128_bits"
		} else {}
		if U64x2.from_u128_bits(a).to_u128_bits() != SimdOracle.U64x2.from_u128_bits(a).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.from_u128_bits"
		} else {}
		if U64x2.plus_wrap(U64x2.from_u128_bits(a), U64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.U64x2.plus_wrap(SimdOracle.U64x2.from_u128_bits(a), SimdOracle.U64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.plus_wrap"
		} else {}
		if U64x2.minus_wrap(U64x2.from_u128_bits(a), U64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.U64x2.minus_wrap(SimdOracle.U64x2.from_u128_bits(a), SimdOracle.U64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.minus_wrap"
		} else {}
		if U64x2.bitwise_and(U64x2.from_u128_bits(a), U64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.U64x2.bitwise_and(SimdOracle.U64x2.from_u128_bits(a), SimdOracle.U64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.bitwise_and"
		} else {}
		if U64x2.bitwise_or(U64x2.from_u128_bits(a), U64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.U64x2.bitwise_or(SimdOracle.U64x2.from_u128_bits(a), SimdOracle.U64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.bitwise_or"
		} else {}
		if U64x2.bitwise_xor(U64x2.from_u128_bits(a), U64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.U64x2.bitwise_xor(SimdOracle.U64x2.from_u128_bits(a), SimdOracle.U64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.bitwise_xor"
		} else {}
		if U64x2.bitwise_not(U64x2.from_u128_bits(a)).to_u128_bits() != SimdOracle.U64x2.bitwise_not(SimdOracle.U64x2.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.bitwise_not"
		} else {}
		if U64x2.bit_select(U64x2.from_u128_bits(a), U64x2.from_u128_bits(b), U64x2.from_u128_bits(c)).to_u128_bits() != SimdOracle.U64x2.bit_select(SimdOracle.U64x2.from_u128_bits(a), SimdOracle.U64x2.from_u128_bits(b), SimdOracle.U64x2.from_u128_bits(c)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.bit_select"
		} else {}
		if U64x2.eq_lanes(U64x2.from_u128_bits(a), U64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.U64x2.eq_lanes(SimdOracle.U64x2.from_u128_bits(a), SimdOracle.U64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.eq_lanes"
		} else {}
		if U64x2.to_bitmask(U64x2.from_u128_bits(a)) != SimdOracle.U64x2.to_bitmask(SimdOracle.U64x2.from_u128_bits(a)) {
			crash "SIMD differential mismatch: U64x2.to_bitmask"
		} else {}
		if U64x2.shl_wrap(U64x2.from_u128_bits(a), count).to_u128_bits() != SimdOracle.U64x2.shl_wrap(SimdOracle.U64x2.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.shl_wrap"
		} else {}
		if U64x2.shr_wrap(U64x2.from_u128_bits(a), count).to_u128_bits() != SimdOracle.U64x2.shr_wrap(SimdOracle.U64x2.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.shr_wrap"
		} else {}
		if U64x2.shr_zf_wrap(U64x2.from_u128_bits(a), count).to_u128_bits() != SimdOracle.U64x2.shr_zf_wrap(SimdOracle.U64x2.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.shr_zf_wrap"
		} else {}
		if U64x2.get_lane(U64x2.from_u128_bits(a), index_u64x2.to_u64()) != SimdOracle.U64x2.get_lane(SimdOracle.U64x2.from_u128_bits(a), index_u64x2.to_u64()) {
			crash "SIMD differential mismatch: U64x2.get_lane"
		} else {}
		if U64x2.with_lane(U64x2.from_u128_bits(a), index_u64x2.to_u64(), c.to_u64_wrap()).to_u128_bits() != SimdOracle.U64x2.with_lane(SimdOracle.U64x2.from_u128_bits(a), index_u64x2.to_u64(), c.to_u64_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.with_lane"
		} else {}
		if U64x2.interleave_lo(U64x2.from_u128_bits(a), U64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.U64x2.interleave_lo(SimdOracle.U64x2.from_u128_bits(a), SimdOracle.U64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.interleave_lo"
		} else {}
		if U64x2.interleave_hi(U64x2.from_u128_bits(a), U64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.U64x2.interleave_hi(SimdOracle.U64x2.from_u128_bits(a), SimdOracle.U64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.interleave_hi"
		} else {}
		if U64x2.reverse_lanes(U64x2.from_u128_bits(a)).to_u128_bits() != SimdOracle.U64x2.reverse_lanes(SimdOracle.U64x2.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.reverse_lanes"
		} else {}
		if U64x2.narrow_to_u32x4_wrap(U64x2.from_u128_bits(a), U64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.U64x2.narrow_to_u32x4_wrap(SimdOracle.U64x2.from_u128_bits(a), SimdOracle.U64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.narrow_to_u32x4_wrap"
		} else {}
		if U64x2.sum_lanes_wrap(U64x2.from_u128_bits(a)) != SimdOracle.U64x2.sum_lanes_wrap(SimdOracle.U64x2.from_u128_bits(a)) {
			crash "SIMD differential mismatch: U64x2.sum_lanes_wrap"
		} else {}
		if U64x2.carryless_times_lo(U64x2.from_u128_bits(a), U64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.U64x2.carryless_times_lo(SimdOracle.U64x2.from_u128_bits(a), SimdOracle.U64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.carryless_times_lo"
		} else {}
		if U64x2.carryless_times_hi(U64x2.from_u128_bits(a), U64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.U64x2.carryless_times_hi(SimdOracle.U64x2.from_u128_bits(a), SimdOracle.U64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: U64x2.carryless_times_hi"
		} else {}
		if U64x2.store(U64x2.from_u128_bits(a), bytes, offset) != SimdOracle.U64x2.store(SimdOracle.U64x2.from_u128_bits(a), bytes, offset) {
			crash "SIMD differential mismatch: U64x2.store"
		} else {}
		if U64x2.append_to(U64x2.from_u128_bits(a), bytes) != SimdOracle.U64x2.append_to(SimdOracle.U64x2.from_u128_bits(a), bytes) {
			crash "SIMD differential mismatch: U64x2.append_to"
		} else {}
		{}
	}

	check_i64x2 : U128, U128, U128, U8, List(U8) -> {}
	check_i64x2 = |a, b, c, count, bytes| {
		index_i64x2 = count % 2
		offset = count.to_u64() % 33
		# I64x2
		if I64x2.splat(a.to_i64_wrap()).to_u128_bits() != SimdOracle.I64x2.splat(a.to_i64_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.splat"
		} else {}
		if I64x2.to_u128_bits(I64x2.from_u128_bits(a)) != SimdOracle.I64x2.to_u128_bits(SimdOracle.I64x2.from_u128_bits(a)) {
			crash "SIMD differential mismatch: I64x2.to_u128_bits"
		} else {}
		if I64x2.from_u128_bits(a).to_u128_bits() != SimdOracle.I64x2.from_u128_bits(a).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.from_u128_bits"
		} else {}
		if I64x2.plus_wrap(I64x2.from_u128_bits(a), I64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.I64x2.plus_wrap(SimdOracle.I64x2.from_u128_bits(a), SimdOracle.I64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.plus_wrap"
		} else {}
		if I64x2.minus_wrap(I64x2.from_u128_bits(a), I64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.I64x2.minus_wrap(SimdOracle.I64x2.from_u128_bits(a), SimdOracle.I64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.minus_wrap"
		} else {}
		if I64x2.negate_wrap(I64x2.from_u128_bits(a)).to_u128_bits() != SimdOracle.I64x2.negate_wrap(SimdOracle.I64x2.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.negate_wrap"
		} else {}
		if I64x2.bitwise_and(I64x2.from_u128_bits(a), I64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.I64x2.bitwise_and(SimdOracle.I64x2.from_u128_bits(a), SimdOracle.I64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.bitwise_and"
		} else {}
		if I64x2.bitwise_or(I64x2.from_u128_bits(a), I64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.I64x2.bitwise_or(SimdOracle.I64x2.from_u128_bits(a), SimdOracle.I64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.bitwise_or"
		} else {}
		if I64x2.bitwise_xor(I64x2.from_u128_bits(a), I64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.I64x2.bitwise_xor(SimdOracle.I64x2.from_u128_bits(a), SimdOracle.I64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.bitwise_xor"
		} else {}
		if I64x2.bitwise_not(I64x2.from_u128_bits(a)).to_u128_bits() != SimdOracle.I64x2.bitwise_not(SimdOracle.I64x2.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.bitwise_not"
		} else {}
		if I64x2.bit_select(I64x2.from_u128_bits(a), I64x2.from_u128_bits(b), I64x2.from_u128_bits(c)).to_u128_bits() != SimdOracle.I64x2.bit_select(SimdOracle.I64x2.from_u128_bits(a), SimdOracle.I64x2.from_u128_bits(b), SimdOracle.I64x2.from_u128_bits(c)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.bit_select"
		} else {}
		if I64x2.eq_lanes(I64x2.from_u128_bits(a), I64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.I64x2.eq_lanes(SimdOracle.I64x2.from_u128_bits(a), SimdOracle.I64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.eq_lanes"
		} else {}
		if I64x2.gt_lanes(I64x2.from_u128_bits(a), I64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.I64x2.gt_lanes(SimdOracle.I64x2.from_u128_bits(a), SimdOracle.I64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.gt_lanes"
		} else {}
		if I64x2.gte_lanes(I64x2.from_u128_bits(a), I64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.I64x2.gte_lanes(SimdOracle.I64x2.from_u128_bits(a), SimdOracle.I64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.gte_lanes"
		} else {}
		if I64x2.to_bitmask(I64x2.from_u128_bits(a)) != SimdOracle.I64x2.to_bitmask(SimdOracle.I64x2.from_u128_bits(a)) {
			crash "SIMD differential mismatch: I64x2.to_bitmask"
		} else {}
		if I64x2.shl_wrap(I64x2.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I64x2.shl_wrap(SimdOracle.I64x2.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.shl_wrap"
		} else {}
		if I64x2.shr_wrap(I64x2.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I64x2.shr_wrap(SimdOracle.I64x2.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.shr_wrap"
		} else {}
		if I64x2.shr_zf_wrap(I64x2.from_u128_bits(a), count).to_u128_bits() != SimdOracle.I64x2.shr_zf_wrap(SimdOracle.I64x2.from_u128_bits(a), count).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.shr_zf_wrap"
		} else {}
		if I64x2.get_lane(I64x2.from_u128_bits(a), index_i64x2.to_u64()) != SimdOracle.I64x2.get_lane(SimdOracle.I64x2.from_u128_bits(a), index_i64x2.to_u64()) {
			crash "SIMD differential mismatch: I64x2.get_lane"
		} else {}
		if I64x2.with_lane(I64x2.from_u128_bits(a), index_i64x2.to_u64(), c.to_i64_wrap()).to_u128_bits() != SimdOracle.I64x2.with_lane(SimdOracle.I64x2.from_u128_bits(a), index_i64x2.to_u64(), c.to_i64_wrap()).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.with_lane"
		} else {}
		if I64x2.interleave_lo(I64x2.from_u128_bits(a), I64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.I64x2.interleave_lo(SimdOracle.I64x2.from_u128_bits(a), SimdOracle.I64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.interleave_lo"
		} else {}
		if I64x2.interleave_hi(I64x2.from_u128_bits(a), I64x2.from_u128_bits(b)).to_u128_bits() != SimdOracle.I64x2.interleave_hi(SimdOracle.I64x2.from_u128_bits(a), SimdOracle.I64x2.from_u128_bits(b)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.interleave_hi"
		} else {}
		if I64x2.reverse_lanes(I64x2.from_u128_bits(a)).to_u128_bits() != SimdOracle.I64x2.reverse_lanes(SimdOracle.I64x2.from_u128_bits(a)).to_u128_bits() {
			crash "SIMD differential mismatch: I64x2.reverse_lanes"
		} else {}
		if I64x2.sum_lanes_wrap(I64x2.from_u128_bits(a)) != SimdOracle.I64x2.sum_lanes_wrap(SimdOracle.I64x2.from_u128_bits(a)) {
			crash "SIMD differential mismatch: I64x2.sum_lanes_wrap"
		} else {}
		if I64x2.store(I64x2.from_u128_bits(a), bytes, offset) != SimdOracle.I64x2.store(SimdOracle.I64x2.from_u128_bits(a), bytes, offset) {
			crash "SIMD differential mismatch: I64x2.store"
		} else {}
		if I64x2.append_to(I64x2.from_u128_bits(a), bytes) != SimdOracle.I64x2.append_to(SimdOracle.I64x2.from_u128_bits(a), bytes) {
			crash "SIMD differential mismatch: I64x2.append_to"
		} else {}
		{}
	}

	check_loads : U8, List(U8) -> {}
	check_loads = |count, bytes| {
		offset = count.to_u64() % 33
		match (U8x16.load(bytes, offset), SimdOracle.U8x16.load(bytes, offset)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: U8x16.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: U8x16.load bounds"
			}
		}
		match (U8x16.load(bytes, 33), SimdOracle.U8x16.load(bytes, 33)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: U8x16.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: U8x16.load bounds"
			}
		}
		match (I8x16.load(bytes, offset), SimdOracle.I8x16.load(bytes, offset)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: I8x16.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: I8x16.load bounds"
			}
		}
		match (I8x16.load(bytes, 33), SimdOracle.I8x16.load(bytes, 33)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: I8x16.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: I8x16.load bounds"
			}
		}
		match (U16x8.load(bytes, offset), SimdOracle.U16x8.load(bytes, offset)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: U16x8.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: U16x8.load bounds"
			}
		}
		match (U16x8.load(bytes, 33), SimdOracle.U16x8.load(bytes, 33)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: U16x8.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: U16x8.load bounds"
			}
		}
		match (I16x8.load(bytes, offset), SimdOracle.I16x8.load(bytes, offset)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: I16x8.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: I16x8.load bounds"
			}
		}
		match (I16x8.load(bytes, 33), SimdOracle.I16x8.load(bytes, 33)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: I16x8.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: I16x8.load bounds"
			}
		}
		match (U32x4.load(bytes, offset), SimdOracle.U32x4.load(bytes, offset)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: U32x4.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: U32x4.load bounds"
			}
		}
		match (U32x4.load(bytes, 33), SimdOracle.U32x4.load(bytes, 33)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: U32x4.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: U32x4.load bounds"
			}
		}
		match (I32x4.load(bytes, offset), SimdOracle.I32x4.load(bytes, offset)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: I32x4.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: I32x4.load bounds"
			}
		}
		match (I32x4.load(bytes, 33), SimdOracle.I32x4.load(bytes, 33)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: I32x4.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: I32x4.load bounds"
			}
		}
		match (U64x2.load(bytes, offset), SimdOracle.U64x2.load(bytes, offset)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: U64x2.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: U64x2.load bounds"
			}
		}
		match (U64x2.load(bytes, 33), SimdOracle.U64x2.load(bytes, 33)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: U64x2.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: U64x2.load bounds"
			}
		}
		match (I64x2.load(bytes, offset), SimdOracle.I64x2.load(bytes, offset)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: I64x2.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: I64x2.load bounds"
			}
		}
		match (I64x2.load(bytes, 33), SimdOracle.I64x2.load(bytes, 33)) {
			(Ok(native), Ok(reference)) =>
				if native.to_u128_bits() != reference.to_u128_bits() {
					crash "SIMD differential mismatch: I64x2.load"
				} else {}
			(Err(OutOfBounds), Err(OutOfBounds)) => {}
			_ => {
				crash "SIMD differential mismatch: I64x2.load bounds"
			}
		}
		{}
	}

	# Directly cross-check each semantically distinct count against the scalar
	# oracle. Counts above a lane width are covered transitively by
	# check_shift_masked_count below.
	check_shift_reference_count : U128, U8 -> {}
	check_shift_reference_count = |bits, count| {
		if count < 8 {
			if U8x16.shl_wrap(U8x16.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.U8x16.shl_wrap(SimdOracle.U8x16.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: U8x16.shl_wrap canonical count"
			} else {}
			if U8x16.shr_wrap(U8x16.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.U8x16.shr_wrap(SimdOracle.U8x16.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: U8x16.shr_wrap canonical count"
			} else {}
			if U8x16.shr_zf_wrap(U8x16.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.U8x16.shr_zf_wrap(SimdOracle.U8x16.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: U8x16.shr_zf_wrap canonical count"
			} else {}
			if I8x16.shl_wrap(I8x16.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.I8x16.shl_wrap(SimdOracle.I8x16.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: I8x16.shl_wrap canonical count"
			} else {}
			if I8x16.shr_wrap(I8x16.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.I8x16.shr_wrap(SimdOracle.I8x16.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: I8x16.shr_wrap canonical count"
			} else {}
			if I8x16.shr_zf_wrap(I8x16.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.I8x16.shr_zf_wrap(SimdOracle.I8x16.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: I8x16.shr_zf_wrap canonical count"
			} else {}
		} else {}
		if count < 16 {
			if U16x8.shl_wrap(U16x8.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.U16x8.shl_wrap(SimdOracle.U16x8.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: U16x8.shl_wrap canonical count"
			} else {}
			if U16x8.shr_wrap(U16x8.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.U16x8.shr_wrap(SimdOracle.U16x8.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: U16x8.shr_wrap canonical count"
			} else {}
			if U16x8.shr_zf_wrap(U16x8.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.U16x8.shr_zf_wrap(SimdOracle.U16x8.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: U16x8.shr_zf_wrap canonical count"
			} else {}
			if I16x8.shl_wrap(I16x8.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.I16x8.shl_wrap(SimdOracle.I16x8.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: I16x8.shl_wrap canonical count"
			} else {}
			if I16x8.shr_wrap(I16x8.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.I16x8.shr_wrap(SimdOracle.I16x8.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: I16x8.shr_wrap canonical count"
			} else {}
			if I16x8.shr_zf_wrap(I16x8.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.I16x8.shr_zf_wrap(SimdOracle.I16x8.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: I16x8.shr_zf_wrap canonical count"
			} else {}
		} else {}
		if count < 32 {
			if U32x4.shl_wrap(U32x4.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.U32x4.shl_wrap(SimdOracle.U32x4.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: U32x4.shl_wrap canonical count"
			} else {}
			if U32x4.shr_wrap(U32x4.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.U32x4.shr_wrap(SimdOracle.U32x4.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: U32x4.shr_wrap canonical count"
			} else {}
			if U32x4.shr_zf_wrap(U32x4.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.U32x4.shr_zf_wrap(SimdOracle.U32x4.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: U32x4.shr_zf_wrap canonical count"
			} else {}
			if I32x4.shl_wrap(I32x4.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.I32x4.shl_wrap(SimdOracle.I32x4.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: I32x4.shl_wrap canonical count"
			} else {}
			if I32x4.shr_wrap(I32x4.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.I32x4.shr_wrap(SimdOracle.I32x4.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: I32x4.shr_wrap canonical count"
			} else {}
			if I32x4.shr_zf_wrap(I32x4.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.I32x4.shr_zf_wrap(SimdOracle.I32x4.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: I32x4.shr_zf_wrap canonical count"
			} else {}
		} else {}
		if count < 64 {
			if U64x2.shl_wrap(U64x2.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.U64x2.shl_wrap(SimdOracle.U64x2.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: U64x2.shl_wrap canonical count"
			} else {}
			if U64x2.shr_wrap(U64x2.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.U64x2.shr_wrap(SimdOracle.U64x2.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: U64x2.shr_wrap canonical count"
			} else {}
			if U64x2.shr_zf_wrap(U64x2.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.U64x2.shr_zf_wrap(SimdOracle.U64x2.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: U64x2.shr_zf_wrap canonical count"
			} else {}
			if I64x2.shl_wrap(I64x2.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.I64x2.shl_wrap(SimdOracle.I64x2.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: I64x2.shl_wrap canonical count"
			} else {}
			if I64x2.shr_wrap(I64x2.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.I64x2.shr_wrap(SimdOracle.I64x2.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: I64x2.shr_wrap canonical count"
			} else {}
			if I64x2.shr_zf_wrap(I64x2.from_u128_bits(bits), count).to_u128_bits() != SimdOracle.I64x2.shr_zf_wrap(SimdOracle.I64x2.from_u128_bits(bits), count).to_u128_bits() {
				crash "SIMD differential mismatch: I64x2.shr_zf_wrap canonical count"
			} else {}
		} else {}
		{}
	}

	# Prove every non-canonical U8 count has the specified modulo-lane-width
	# result. The canonical side of each equality is checked against the scalar
	# oracle above, so these equalities extend that differential proof to all 256
	# possible counts without recomputing the expensive scalar oracle.
	check_shift_masked_count : U128, U8 -> {}
	check_shift_masked_count = |bits, count| {
		if count >= 8 {
			effective = count % 8
			if U8x16.shl_wrap(U8x16.from_u128_bits(bits), count).to_u128_bits() != U8x16.shl_wrap(U8x16.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: U8x16.shl_wrap"
			} else {}
			if U8x16.shr_wrap(U8x16.from_u128_bits(bits), count).to_u128_bits() != U8x16.shr_wrap(U8x16.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: U8x16.shr_wrap"
			} else {}
			if U8x16.shr_zf_wrap(U8x16.from_u128_bits(bits), count).to_u128_bits() != U8x16.shr_zf_wrap(U8x16.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: U8x16.shr_zf_wrap"
			} else {}
			if I8x16.shl_wrap(I8x16.from_u128_bits(bits), count).to_u128_bits() != I8x16.shl_wrap(I8x16.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: I8x16.shl_wrap"
			} else {}
			if I8x16.shr_wrap(I8x16.from_u128_bits(bits), count).to_u128_bits() != I8x16.shr_wrap(I8x16.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: I8x16.shr_wrap"
			} else {}
			if I8x16.shr_zf_wrap(I8x16.from_u128_bits(bits), count).to_u128_bits() != I8x16.shr_zf_wrap(I8x16.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: I8x16.shr_zf_wrap"
			} else {}
		} else {}
		if count >= 16 {
			effective = count % 16
			if U16x8.shl_wrap(U16x8.from_u128_bits(bits), count).to_u128_bits() != U16x8.shl_wrap(U16x8.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: U16x8.shl_wrap"
			} else {}
			if U16x8.shr_wrap(U16x8.from_u128_bits(bits), count).to_u128_bits() != U16x8.shr_wrap(U16x8.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: U16x8.shr_wrap"
			} else {}
			if U16x8.shr_zf_wrap(U16x8.from_u128_bits(bits), count).to_u128_bits() != U16x8.shr_zf_wrap(U16x8.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: U16x8.shr_zf_wrap"
			} else {}
			if I16x8.shl_wrap(I16x8.from_u128_bits(bits), count).to_u128_bits() != I16x8.shl_wrap(I16x8.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: I16x8.shl_wrap"
			} else {}
			if I16x8.shr_wrap(I16x8.from_u128_bits(bits), count).to_u128_bits() != I16x8.shr_wrap(I16x8.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: I16x8.shr_wrap"
			} else {}
			if I16x8.shr_zf_wrap(I16x8.from_u128_bits(bits), count).to_u128_bits() != I16x8.shr_zf_wrap(I16x8.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: I16x8.shr_zf_wrap"
			} else {}
		} else {}
		if count >= 32 {
			effective = count % 32
			if U32x4.shl_wrap(U32x4.from_u128_bits(bits), count).to_u128_bits() != U32x4.shl_wrap(U32x4.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: U32x4.shl_wrap"
			} else {}
			if U32x4.shr_wrap(U32x4.from_u128_bits(bits), count).to_u128_bits() != U32x4.shr_wrap(U32x4.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: U32x4.shr_wrap"
			} else {}
			if U32x4.shr_zf_wrap(U32x4.from_u128_bits(bits), count).to_u128_bits() != U32x4.shr_zf_wrap(U32x4.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: U32x4.shr_zf_wrap"
			} else {}
			if I32x4.shl_wrap(I32x4.from_u128_bits(bits), count).to_u128_bits() != I32x4.shl_wrap(I32x4.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: I32x4.shl_wrap"
			} else {}
			if I32x4.shr_wrap(I32x4.from_u128_bits(bits), count).to_u128_bits() != I32x4.shr_wrap(I32x4.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: I32x4.shr_wrap"
			} else {}
			if I32x4.shr_zf_wrap(I32x4.from_u128_bits(bits), count).to_u128_bits() != I32x4.shr_zf_wrap(I32x4.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: I32x4.shr_zf_wrap"
			} else {}
		} else {}
		if count >= 64 {
			effective = count % 64
			if U64x2.shl_wrap(U64x2.from_u128_bits(bits), count).to_u128_bits() != U64x2.shl_wrap(U64x2.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: U64x2.shl_wrap"
			} else {}
			if U64x2.shr_wrap(U64x2.from_u128_bits(bits), count).to_u128_bits() != U64x2.shr_wrap(U64x2.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: U64x2.shr_wrap"
			} else {}
			if U64x2.shr_zf_wrap(U64x2.from_u128_bits(bits), count).to_u128_bits() != U64x2.shr_zf_wrap(U64x2.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: U64x2.shr_zf_wrap"
			} else {}
			if I64x2.shl_wrap(I64x2.from_u128_bits(bits), count).to_u128_bits() != I64x2.shl_wrap(I64x2.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: I64x2.shl_wrap"
			} else {}
			if I64x2.shr_wrap(I64x2.from_u128_bits(bits), count).to_u128_bits() != I64x2.shr_wrap(I64x2.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: I64x2.shr_wrap"
			} else {}
			if I64x2.shr_zf_wrap(I64x2.from_u128_bits(bits), count).to_u128_bits() != I64x2.shr_zf_wrap(I64x2.from_u128_bits(bits), effective).to_u128_bits() {
				crash "SIMD count masking mismatch: I64x2.shr_zf_wrap"
			} else {}
		} else {}
		{}
	}

	check_load_store_out_of_bounds : U128, List(U8), U64 -> {}
	check_load_store_out_of_bounds = |bits, bytes, index| {
		match U8x16.load(bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: U8x16.load accepted invalid offset"
			}
		}
		match U8x16.store(U8x16.from_u128_bits(bits), bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: U8x16.store accepted invalid offset"
			}
		}
		match I8x16.load(bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: I8x16.load accepted invalid offset"
			}
		}
		match I8x16.store(I8x16.from_u128_bits(bits), bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: I8x16.store accepted invalid offset"
			}
		}
		match U16x8.load(bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: U16x8.load accepted invalid offset"
			}
		}
		match U16x8.store(U16x8.from_u128_bits(bits), bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: U16x8.store accepted invalid offset"
			}
		}
		match I16x8.load(bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: I16x8.load accepted invalid offset"
			}
		}
		match I16x8.store(I16x8.from_u128_bits(bits), bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: I16x8.store accepted invalid offset"
			}
		}
		match U32x4.load(bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: U32x4.load accepted invalid offset"
			}
		}
		match U32x4.store(U32x4.from_u128_bits(bits), bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: U32x4.store accepted invalid offset"
			}
		}
		match I32x4.load(bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: I32x4.load accepted invalid offset"
			}
		}
		match I32x4.store(I32x4.from_u128_bits(bits), bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: I32x4.store accepted invalid offset"
			}
		}
		match U64x2.load(bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: U64x2.load accepted invalid offset"
			}
		}
		match U64x2.store(U64x2.from_u128_bits(bits), bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: U64x2.store accepted invalid offset"
			}
		}
		match I64x2.load(bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: I64x2.load accepted invalid offset"
			}
		}
		match I64x2.store(I64x2.from_u128_bits(bits), bytes, index) {
			Err(OutOfBounds) => {}
			_ => {
				crash "SIMD bounds mismatch: I64x2.store accepted invalid offset"
			}
		}
		{}
	}

	check_store_last_valid : U128, List(U8), U64 -> {}
	check_store_last_valid = |bits, bytes, index| {
		if U8x16.store(U8x16.from_u128_bits(bits), bytes, index) != SimdOracle.U8x16.store(SimdOracle.U8x16.from_u128_bits(bits), bytes, index) {
			crash "SIMD bounds mismatch: U8x16.store at last valid offset"
		} else {}
		if I8x16.store(I8x16.from_u128_bits(bits), bytes, index) != SimdOracle.I8x16.store(SimdOracle.I8x16.from_u128_bits(bits), bytes, index) {
			crash "SIMD bounds mismatch: I8x16.store at last valid offset"
		} else {}
		if U16x8.store(U16x8.from_u128_bits(bits), bytes, index) != SimdOracle.U16x8.store(SimdOracle.U16x8.from_u128_bits(bits), bytes, index) {
			crash "SIMD bounds mismatch: U16x8.store at last valid offset"
		} else {}
		if I16x8.store(I16x8.from_u128_bits(bits), bytes, index) != SimdOracle.I16x8.store(SimdOracle.I16x8.from_u128_bits(bits), bytes, index) {
			crash "SIMD bounds mismatch: I16x8.store at last valid offset"
		} else {}
		if U32x4.store(U32x4.from_u128_bits(bits), bytes, index) != SimdOracle.U32x4.store(SimdOracle.U32x4.from_u128_bits(bits), bytes, index) {
			crash "SIMD bounds mismatch: U32x4.store at last valid offset"
		} else {}
		if I32x4.store(I32x4.from_u128_bits(bits), bytes, index) != SimdOracle.I32x4.store(SimdOracle.I32x4.from_u128_bits(bits), bytes, index) {
			crash "SIMD bounds mismatch: I32x4.store at last valid offset"
		} else {}
		if U64x2.store(U64x2.from_u128_bits(bits), bytes, index) != SimdOracle.U64x2.store(SimdOracle.U64x2.from_u128_bits(bits), bytes, index) {
			crash "SIMD bounds mismatch: U64x2.store at last valid offset"
		} else {}
		if I64x2.store(I64x2.from_u128_bits(bits), bytes, index) != SimdOracle.I64x2.store(SimdOracle.I64x2.from_u128_bits(bits), bytes, index) {
			crash "SIMD bounds mismatch: I64x2.store at last valid offset"
		} else {}
		{}
	}

	check_all : U128, U128, U128, U8, List(U8) -> {}
	check_all = |a, b, c, count, bytes| {
		check_u8x16(a, b, c, count, bytes)
		check_i8x16(a, b, c, count, bytes)
		check_u16x8(a, b, c, count, bytes)
		check_i16x8(a, b, c, count, bytes)
		check_u32x4(a, b, c, count, bytes)
		check_i32x4(a, b, c, count, bytes)
		check_u64x2(a, b, c, count, bytes)
		check_i64x2(a, b, c, count, bytes)
		check_loads(count, bytes)
	}

	next_bits : U128 -> U128
	next_bits = |state| {
		a = state.bitwise_xor(state.shl_wrap(13))
		b = a.bitwise_xor(a.shr_zf_wrap(17))
		b.bitwise_xor(b.shl_wrap(43))
	}

	check_edge_corpus : {} -> {}
	check_edge_corpus = |_| {
		bytes = List.repeat(171.U8, 49)
		# Offset 33 is the last valid 16-byte window. Pin both sides of that
		# boundary, plus the overflow-resistant index > len guard.
		check_loads(33, bytes)
		check_store_last_valid(U128.highest, bytes, 33)
		check_load_store_out_of_bounds(U128.highest, bytes, 34)
		check_load_store_out_of_bounds(U128.highest, bytes, U64.highest)
		# Zero, all-ones, top-bit, and alternating-bit vectors.
		check_all(0, U128.highest, 1, 0, bytes)
		check_all(1, 170141183460469231731687303715884105728, U128.highest, 1, bytes)
		check_all(113868807607784855478016405599735666090, 226413559313153607985358201832032545365, U128.highest, 7, bytes)
		# Every q15 lane at -32768, its neighbor, and +32767.
		check_all(170143779648513184874767057851898691584, 170143779648513184874767057851898691584, 170148972024601091160926566123927863297, 15, bytes)
		check_all(170148972024601091160926566123927863297, 170138587272425278588607549579869519871, 170143779648513184874767057851898691584, 16, bytes)
		# Saturating byte arithmetic and signed/unsigned dot-product extremes.
		check_all(333610163647978885748406477874282560250, 26688813091838310859872518229942604820, U128.highest, 17, bytes)
		check_all(U128.highest, 170808403787765189503184116671632670848, 169473963133173273960190490760135540607, 31, bytes)
		# Narrowing values below, at, and above signed and unsigned boundaries.
		check_all(340279770693590523249073117797199970304, 170136001304969032327431786626608496640, U128.highest, 32, bytes)
		# Dynamic table indices 0, 15, 16, 17, 127, 128, and 255 in one vector.
		check_all(20011376718272490338853433276725592320, 18614374135967779043399751806946905856, 0, 63, bytes)
		# Carryless multiply fuel: 0, 1, 3, 5, the top bit, and all ones.
		check_all(0, 1, 3, 64, bytes)
		check_all(3, 5, 9223372036854775808, 127, bytes)
		check_all(18446744073709551615, 170141183460469231731687303715884105728, U128.highest, 255, bytes)
		{}
	}

	check_properties : U128, U128 -> {}
	check_properties = |a, b| {
		u8a = U8x16.from_u128_bits(a)
		u8b = U8x16.from_u128_bits(b)
		i8a = I8x16.from_u128_bits(a)
		i8b = I8x16.from_u128_bits(b)
		u16a = U16x8.from_u128_bits(a)
		u16b = U16x8.from_u128_bits(b)
		i16a = I16x8.from_u128_bits(a)
		i16b = I16x8.from_u128_bits(b)
		u32a = U32x4.from_u128_bits(a)
		u32b = U32x4.from_u128_bits(b)
		i32a = I32x4.from_u128_bits(a)
		i32b = I32x4.from_u128_bits(b)

		u8lo = U8x16.interleave_lo(u8a, u8b)
		u8hi = U8x16.interleave_hi(u8a, u8b)
		if U8x16.even_lanes(u8lo, u8hi).to_u128_bits() != a or U8x16.odd_lanes(u8lo, u8hi).to_u128_bits() != b {
			crash "SIMD property mismatch: U8x16 interleave inverse"
		} else {}
		i8lo = I8x16.interleave_lo(i8a, i8b)
		i8hi = I8x16.interleave_hi(i8a, i8b)
		if I8x16.even_lanes(i8lo, i8hi).to_u128_bits() != a or I8x16.odd_lanes(i8lo, i8hi).to_u128_bits() != b {
			crash "SIMD property mismatch: I8x16 interleave inverse"
		} else {}
		u16lo = U16x8.interleave_lo(u16a, u16b)
		u16hi = U16x8.interleave_hi(u16a, u16b)
		if U16x8.even_lanes(u16lo, u16hi).to_u128_bits() != a or U16x8.odd_lanes(u16lo, u16hi).to_u128_bits() != b {
			crash "SIMD property mismatch: U16x8 interleave inverse"
		} else {}
		i16lo = I16x8.interleave_lo(i16a, i16b)
		i16hi = I16x8.interleave_hi(i16a, i16b)
		if I16x8.even_lanes(i16lo, i16hi).to_u128_bits() != a or I16x8.odd_lanes(i16lo, i16hi).to_u128_bits() != b {
			crash "SIMD property mismatch: I16x8 interleave inverse"
		} else {}
		u32lo = U32x4.interleave_lo(u32a, u32b)
		u32hi = U32x4.interleave_hi(u32a, u32b)
		if U32x4.even_lanes(u32lo, u32hi).to_u128_bits() != a or U32x4.odd_lanes(u32lo, u32hi).to_u128_bits() != b {
			crash "SIMD property mismatch: U32x4 interleave inverse"
		} else {}
		i32lo = I32x4.interleave_lo(i32a, i32b)
		i32hi = I32x4.interleave_hi(i32a, i32b)
		if I32x4.even_lanes(i32lo, i32hi).to_u128_bits() != a or I32x4.odd_lanes(i32lo, i32hi).to_u128_bits() != b {
			crash "SIMD property mismatch: I32x4 interleave inverse"
		} else {}

		if U16x8.narrow_to_u8x16_wrap(U8x16.to_u16x8_lo(u8a), U8x16.to_u16x8_hi(u8a)).to_u128_bits() != a {
			crash "SIMD property mismatch: U8x16 widen/narrow"
		} else {}
		if I16x8.narrow_to_i8x16_saturated(I8x16.to_i16x8_lo(i8a), I8x16.to_i16x8_hi(i8a)).to_u128_bits() != a {
			crash "SIMD property mismatch: I8x16 widen/narrow"
		} else {}
		if U32x4.narrow_to_u16x8_wrap(U16x8.to_u32x4_lo(u16a), U16x8.to_u32x4_hi(u16a)).to_u128_bits() != a {
			crash "SIMD property mismatch: U16x8 widen/narrow"
		} else {}
		if I32x4.narrow_to_i16x8_saturated(I16x8.to_i32x4_lo(i16a), I16x8.to_i32x4_hi(i16a)).to_u128_bits() != a {
			crash "SIMD property mismatch: I16x8 widen/narrow"
		} else {}
		if U64x2.narrow_to_u32x4_wrap(U32x4.to_u64x2_lo(u32a), U32x4.to_u64x2_hi(u32a)).to_u128_bits() != a {
			crash "SIMD property mismatch: U32x4 widen/narrow"
		} else {}

		u8_lanes = U8x16.to_list(u8a)
		expected_u8_lanes = SimdOracle.U8x16.to_list(SimdOracle.U8x16.from_u128_bits(a))
		if u8_lanes != expected_u8_lanes {
			if List.len(u8_lanes) != 16 {
				crash "SIMD property mismatch: U8x16 to_list length"
			} else if u8_lanes.first() != expected_u8_lanes.first() {
				crash "SIMD property mismatch: U8x16 to_list lane 0"
			} else if List.get(u8_lanes, 1) != List.get(expected_u8_lanes, 1) {
				crash "SIMD property mismatch: U8x16 to_list lane 1"
			} else if List.get(u8_lanes, 15) != List.get(expected_u8_lanes, 15) {
				crash "SIMD property mismatch: U8x16 to_list lane 15"
			} else {
				crash "SIMD property mismatch: U8x16 to_list middle lane"
			}
		} else {}
		match U8x16.from_list(u8_lanes) {
			Ok(vector) => if vector.to_u128_bits() != a {
				crash "SIMD property mismatch: U8x16 list round trip"
			} else {}
			Err(_) => {
				crash "SIMD property mismatch: U8x16 list round trip length"
			}
		}
		match I8x16.from_list(I8x16.to_list(i8a)) {
			Ok(vector) => if vector.to_u128_bits() != a {
				crash "SIMD property mismatch: I8x16 list round trip"
			} else {}
			Err(_) => {
				crash "SIMD property mismatch: I8x16 list round trip length"
			}
		}
		match U16x8.from_list(U16x8.to_list(u16a)) {
			Ok(vector) => if vector.to_u128_bits() != a {
				crash "SIMD property mismatch: U16x8 list round trip"
			} else {}
			Err(_) => {
				crash "SIMD property mismatch: U16x8 list round trip length"
			}
		}
		match I16x8.from_list(I16x8.to_list(i16a)) {
			Ok(vector) => if vector.to_u128_bits() != a {
				crash "SIMD property mismatch: I16x8 list round trip"
			} else {}
			Err(_) => {
				crash "SIMD property mismatch: I16x8 list round trip length"
			}
		}
		match U32x4.from_list(U32x4.to_list(u32a)) {
			Ok(vector) => if vector.to_u128_bits() != a {
				crash "SIMD property mismatch: U32x4 list round trip"
			} else {}
			Err(_) => {
				crash "SIMD property mismatch: U32x4 list round trip length"
			}
		}
		match I32x4.from_list(I32x4.to_list(i32a)) {
			Ok(vector) => if vector.to_u128_bits() != a {
				crash "SIMD property mismatch: I32x4 list round trip"
			} else {}
			Err(_) => {
				crash "SIMD property mismatch: I32x4 list round trip length"
			}
		}
		u64a = U64x2.from_u128_bits(a)
		match U64x2.from_list(U64x2.to_list(u64a)) {
			Ok(vector) => if vector.to_u128_bits() != a {
				crash "SIMD property mismatch: U64x2 list round trip"
			} else {}
			Err(_) => {
				crash "SIMD property mismatch: U64x2 list round trip length"
			}
		}
		i64a = I64x2.from_u128_bits(a)
		match I64x2.from_list(I64x2.to_list(i64a)) {
			Ok(vector) => if vector.to_u128_bits() != a {
				crash "SIMD property mismatch: I64x2 list round trip"
			} else {}
			Err(_) => {
				crash "SIMD property mismatch: I64x2 list round trip length"
			}
		}

		if u8a.to_i8x16_bits().to_u8x16_bits().to_u128_bits() != a or u8a.to_u16x8_bits().to_u8x16_bits().to_u128_bits() != a or u8a.to_i16x8_bits().to_u8x16_bits().to_u128_bits() != a or u8a.to_u32x4_bits().to_u8x16_bits().to_u128_bits() != a or u8a.to_i32x4_bits().to_u8x16_bits().to_u128_bits() != a or u8a.to_u64x2_bits().to_u8x16_bits().to_u128_bits() != a or u8a.to_i64x2_bits().to_u8x16_bits().to_u128_bits() != a {
			crash "SIMD property mismatch: bitcast round trip"
		} else {}
		{}
	}

	run_corpus : U128 -> Bool
	run_corpus = |seed| {
		check_edge_corpus({})
		var $state = seed
		var $i = 0.U64
		while $i < 64 {
			$state = next_bits($state.bitwise_xor($i.to_u128()))
			a = $state
			$state = next_bits($state)
			b = $state
			$state = next_bits($state)
			c = $state
			check_properties(a, b)
			bytes = List.repeat(a.to_u8_wrap(), 49)
			# Count-independent operations, lane access, concat, and memory
			# operations run once for each generated vector.
			check_all(a, b, c, a.to_u8_wrap(), bytes)
			# Shift semantics accept U8, so this loop proves every possible count.
			# Canonical counts are checked directly against the scalar oracle; all
			# other counts are proven equal to their checked modulo-width count.
			var $count = 0.U64
			while $count < 256 {
				count = $count.to_u8_wrap()
				if count < 64 {
					check_shift_reference_count(a, count)
				} else {}
				check_shift_masked_count(a, count)
				$count = $count + 1
			}
			$i = $i + 1
		}
		Bool.True
	}

}
