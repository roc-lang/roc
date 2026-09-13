# Repro for https://github.com/roc-lang/roc/issues/11117
# Keep the whole Teddy m=3 chain and consume the raw bitmask: a bare
# comparison or a zero test can hide scalarization in the full scan.
app [main!] { pf: platform "../fx-open/platform/main.roc" }

prefilter_candidates : List(U8), U8x16, U8x16, U8x16, U8x16, U8x16, U8x16 -> U64
prefilter_candidates = |hay, lo0, hi0, lo1, hi1, lo2, hi2| {
	nibble_mask = U8x16.splat(0x0F)
	zero = U8x16.splat(0)
	var $window = 0.U64
	var $prev1 = zero
	var $prev2 = zero
	var $bits = 0.U64

	while True {
		chunk = match U8x16.load(hay, $window) {
			Ok(v) => v
			Err(_) => {
				break
			}
		}
		low = chunk.bitwise_and(nibble_mask)
		high = chunk.shr_zf_wrap(4)
		a0 = lo0.table_lookup(low).bitwise_and(hi0.table_lookup(high))
		a1 = lo1.table_lookup(low).bitwise_and(hi1.table_lookup(high))
		a2 = lo2.table_lookup(low).bitwise_and(hi2.table_lookup(high))
		s1 = $prev1.concat_shift_bytes(a1, 15)
		s2 = $prev2.concat_shift_bytes(a2, 14)
		candidate = a0.bitwise_and(s1).bitwise_and(s2)
		mask = candidate.eq_lanes(zero).bitwise_not().to_bitmask()
		$bits = $bits.plus_wrap(mask.to_u64())
		$prev1 = a1
		$prev2 = a2
		$window = $window.plus_wrap(16)
	}

	$bits
}

main! = |args| {
	salt = args.len().to_u128()
	length = args.len().to_u64() * 512 + 64
	hay = List.repeat(args.len().to_u8_wrap(), length)
	lo0 = U8x16.from_u128_bits((21345817372864405881847059188222722561).bitwise_xor(salt))
	hi0 = U8x16.from_u128_bits((98126319374817364501927364501927364501).bitwise_xor(salt))
	lo1 = U8x16.from_u128_bits((41827364501927364501927364501927364501).bitwise_xor(salt))
	hi1 = U8x16.from_u128_bits((72645019273645019273645019273645019273).bitwise_xor(salt))
	lo2 = U8x16.from_u128_bits((19273645019273645019273645019273645019).bitwise_xor(salt))
	hi2 = U8x16.from_u128_bits((56102938475610293847561029384756102938).bitwise_xor(salt))
	if prefilter_candidates(hay, lo0, hi0, lo1, hi1, lo2, hi2) > length * 65535 {
		Err(Exit(1))
	} else {
		Ok({})
	}
}
