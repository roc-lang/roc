main! = |args| {
	count = args.len().to_u8_wrap()

	# SIMD ops whose x86 instructions are above the x86-64 baseline: a variable
	# shuffle, a saturating fixed-point multiply, a carryless multiply, a
	# narrowing pack, and a runtime-indexed lane read.
	a = U8x16.splat(count + 1)
	sum = a.plus_wrap(U8x16.splat(3))
	shifted = sum.shl_wrap(count)
	widened = shifted.to_u16x8_lo()
	packed = widened.narrow_to_u8x16_saturated(widened)
	mask = packed.eq_lanes(shifted).to_bitmask()

	q15_input = (count - count).to_i16() + (-32768)
	q15 = I16x8.splat(q15_input).times_fixed_q15_saturated(I16x8.splat(q15_input))

	clmul = U64x2.splat(count.to_u64() + 1).carryless_times_lo(U64x2.splat(5))

	lookup = U8x16.splat(count).table_lookup(U8x16.splat(0))

	# Scalar bit counts: POPCNT, LZCNT, and TZCNT are all above the baseline,
	# and LZCNT/TZCNT decode as BSR/BSF on older CPUs rather than faulting, so a
	# wrong lowering here is silent.
	bits = 255.U64 + count.to_u64() - count.to_u64()

	echo!(
		"${mask.to_str()} ${q15.get_lane(0).to_str()} ${clmul.get_lane(0).to_str()} ${lookup.get_lane(0).to_str()} ${bits.count_leading_zero_bits().to_str()} ${bits.count_trailing_zero_bits().to_str()} ${bits.count_one_bits().to_str()}",
	)
	Ok({})
}
