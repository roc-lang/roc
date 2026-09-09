# Exact results for issue #11117: all byte sign masks with arbitrary low
# bits, and immediate byte alignment compared with the scalar definition.
app [main!] { pf: platform "../fx-open/platform/main.roc" }

aligned_bits : U128, U128, U8 -> U128
aligned_bits = |lo, hi, count|
	if count == 0 {
		lo
	}
		else if count == 16 {
			hi
		}
			else {
				lo.shr_zf_wrap(count * 8).bitwise_or(hi.shl_wrap(128 - count * 8))
			}

main! = |args| {
	noise = U8x16.splat(args.len().to_u8_wrap().bitwise_or(0x35).bitwise_and(0x7F)).to_u128_bits()
	var $mask = 0.U32
	while $mask < 65536 {
		var $bits = noise
		var $lane = 0.U8
		while $lane < 16 {
			sign = $mask.shr_zf_wrap($lane).bitwise_and(1).to_u128()
			$bits = $bits.bitwise_or(sign.shl_wrap($lane * 8 + 7))
			$lane = $lane + 1
		}
		v = U8x16.from_u128_bits($bits)
		if v.to_bitmask() != $mask.to_u16_wrap() or v.to_i8x16_bits().to_bitmask() != $mask.to_u16_wrap() {
			crash "SIMD byte bitmask must pack exactly one MSB per lane"
		} else {}
		if v.bitwise_not().to_bitmask() != $mask.to_u16_wrap().bitwise_not() {
			crash "SIMD inverted bitmask must preserve packed mask semantics"
		} else {}
		$mask = $mask + 1
	}
	lo = U8x16.from_u128_bits((20011376718272490338853433276725592320).bitwise_xor(noise))
	hi = lo.bitwise_not()
	if lo.concat_shift_bytes(hi, 0).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 0) {
		crash "SIMD immediate byte alignment count 0"
	} else {}
	if lo.concat_shift_bytes(hi, 1).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 1) {
		crash "SIMD immediate byte alignment count 1"
	} else {}
	if lo.concat_shift_bytes(hi, 2).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 2) {
		crash "SIMD immediate byte alignment count 2"
	} else {}
	if lo.concat_shift_bytes(hi, 3).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 3) {
		crash "SIMD immediate byte alignment count 3"
	} else {}
	if lo.concat_shift_bytes(hi, 4).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 4) {
		crash "SIMD immediate byte alignment count 4"
	} else {}
	if lo.concat_shift_bytes(hi, 5).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 5) {
		crash "SIMD immediate byte alignment count 5"
	} else {}
	if lo.concat_shift_bytes(hi, 6).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 6) {
		crash "SIMD immediate byte alignment count 6"
	} else {}
	if lo.concat_shift_bytes(hi, 7).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 7) {
		crash "SIMD immediate byte alignment count 7"
	} else {}
	if lo.concat_shift_bytes(hi, 8).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 8) {
		crash "SIMD immediate byte alignment count 8"
	} else {}
	if lo.concat_shift_bytes(hi, 9).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 9) {
		crash "SIMD immediate byte alignment count 9"
	} else {}
	if lo.concat_shift_bytes(hi, 10).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 10) {
		crash "SIMD immediate byte alignment count 10"
	} else {}
	if lo.concat_shift_bytes(hi, 11).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 11) {
		crash "SIMD immediate byte alignment count 11"
	} else {}
	if lo.concat_shift_bytes(hi, 12).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 12) {
		crash "SIMD immediate byte alignment count 12"
	} else {}
	if lo.concat_shift_bytes(hi, 13).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 13) {
		crash "SIMD immediate byte alignment count 13"
	} else {}
	if lo.concat_shift_bytes(hi, 14).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 14) {
		crash "SIMD immediate byte alignment count 14"
	} else {}
	if lo.concat_shift_bytes(hi, 15).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 15) {
		crash "SIMD immediate byte alignment count 15"
	} else {}
	if lo.concat_shift_bytes(hi, 16).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), 16) {
		crash "SIMD immediate byte alignment count 16"
	} else {}
	var $count = 0.U8
	while $count <= 16 {
		if lo.concat_shift_bytes(hi, $count).to_u128_bits() != aligned_bits(lo.to_u128_bits(), hi.to_u128_bits(), $count) {
			crash "SIMD runtime byte alignment"
		} else {}
		$count = $count + 1
	}
	Ok({})
}
