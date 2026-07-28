app [main!] { pf: platform "../fx-open/platform/main.roc" }

main! = |args| {
	count = args.len().to_u8_wrap()
	a = U8x16.splat(count + 1)
	b = U8x16.splat(3)
	sum = a.plus_wrap(b)
	shifted = sum.shl_wrap(count)
	widened = shifted.to_u16x8_lo()
	packed = widened.narrow_to_u8x16_saturated(widened)
	mask = packed.eq_lanes(shifted).to_bitmask()
	direct_mask = a.eq_lanes(a).to_bitmask()
	runtime_zero = count - count
	q15_input = runtime_zero.to_i16() + (-32768)
	q15 = I16x8.splat(q15_input).times_fixed_q15_saturated(I16x8.splat(q15_input))
	dot = I16x8.splat(count.to_i16()).dot_pairs(I16x8.splat(3))
	clmul_input = count.to_u64() + 1
	clmul = U64x2.splat(clmul_input).carryless_times_lo(U64x2.splat(5))
	bits = (21345817372864405881847059188222722561).bitwise_xor(count.to_u128())
	lookup_table = U8x16.from_u128_bits(bits)
	lookup_index = count.bitwise_and(15)
	lookup = lookup_table.table_lookup(U8x16.splat(lookup_index))
	lookup_value = lookup_table.get_lane(lookup_index.to_u64())
	list_lane = match U8x16.from_u128_bits(bits).to_list().first() {
		Ok(lane) => lane
		Err(_) => 0
	}
	upper_salt = count.to_u128()
	upper_left = { vector: U8x16.from_u128_bits((18446744073709551616).bitwise_xor(upper_salt)) }
	upper_right = { vector: U8x16.from_u128_bits((36893488147419103232).bitwise_xor(upper_salt)) }

	if upper_left == upper_right {
		Err(StructuralEqualityIgnoredUpperBits)
	} else if direct_mask != 65535 {
		Err(DirectMaskMismatch(direct_mask))
	} else if mask != 65535 {
		Err(MaskMismatch(mask, packed.to_u128_bits(), shifted.to_u128_bits()))
	} else if q15.get_lane(0) != 32767 {
		Err(Q15Mismatch(q15.get_lane(0)))
	} else if dot.get_lane(0) != count.to_i32() * 6 {
		Err(DotMismatch(dot.get_lane(0)))
	} else if lookup.get_lane(0) != lookup_value {
		Err(LookupMismatch(lookup.get_lane(0)))
	} else if clmul.get_lane(0) != count.to_u64() * 5 + 5 {
		Err(ClmulMismatch(clmul.get_lane(0)))
	} else if list_lane != bits.to_u8_wrap() {
		Err(ListLaneMismatch(list_lane, bits.to_u8_wrap()))
	} else {
		Ok({})
	}
}
