# Independent scalar reference implementation for Roc's integer SIMD API.
#
# This is the original pre-lowering { bits : U128 } implementation moved out
# of Builtin.roc. It deliberately uses only scalar operations and the public
# List API, so compiler SIMD low-levels cannot influence its answers.

SimdOracle := [].{

	## A 128-bit SIMD vector of 16 unsigned 8-bit lanes.
	##
	## Lane `i` occupies bits `[i * 8, (i + 1) * 8)` of the vector, and the
	## byte-serialized form (used by [U8x16.load], [U8x16.store], and
	## [U8x16.to_list]) is little-endian with lane 0 first. Every operation
	## has one pinned meaning that is bit-identical on every target; the
	## compiler lowers each operation to the best instruction sequence for
	## the target CPU (SSE/AVX on x86-64, NEON on AArch64, simd128 on wasm).
	U8x16 :: { bits : U128 }.{

		## Returns the [U8x16] with every lane `0`.
		## ```roc
		## expect U8x16.default() == U8x16.splat(0)
		## ```
		default : () -> U8x16
		default = || U8x16.splat(0)

		## Returns a [U8x16] with every lane set to the given [U8].
		##
		## Lowers to `vpbroadcastb` on x86-64, `dup` on AArch64 NEON, and
		## `i8x16.splat` on wasm.
		## ```roc
		## expect U8x16.splat(7).get_lane(15) == 7
		## ```
		splat : U8 -> U8x16
		splat = |value| U8x16.from_u128_bits(simd128_splat(value.to_u64(), 8))

		## Build a [U8x16] from exactly 16 lane values, lane 0 first.
		## Returns `Err(WrongLength)` if the list's length is not 16.
		from_list : List(U8) -> Try(U8x16, [WrongLength, ..])
		from_list = |lanes|
			if List.len(lanes) != 16 {
				Err(WrongLength)
			} else {
				var $bits = 0.U128
				var $i = 0.U64
				while $i < 16 {
					$bits = simd128_with_lane($bits, 8, $i, u8_list_get_unsafe(lanes, $i).to_u64())
					$i = $i + 1
				}
				Ok(U8x16.from_u128_bits($bits))
			}

		## The 16 lane values as a list, lane 0 first.
		## ```roc
		## expect U8x16.splat(9).to_list() == List.repeat(9.U8, 16)
		## ```
		to_list : U8x16 -> List(U8)
		to_list = |vector| {
			var $out = List.with_capacity(16)
			var $i = 0.U64
			while $i < 16 {
				$out = u8_list_append_unsafe($out, U8x16.get_lane(vector, $i))
				$i = $i + 1
			}
			$out
		}

		## Returns `Bool.True` if all 128 bits of the two vectors are equal.
		## (For a per-lane comparison producing a mask, see [U8x16.eq_lanes].)
		##
		## Lowers to `pxor` + `ptest` on x86-64, `cmeq` + `uminv` on AArch64
		## NEON, and `v128.xor` + `v128.any_true` on wasm.
		is_eq : U8x16, U8x16 -> Bool
		is_eq = |a, b| a.to_u128_bits() == b.to_u128_bits()

		## Feed a [U8x16] into a [Hasher].
		to_hash : U8x16, Hasher -> Hasher
		to_hash = |vector, hasher| Hasher.write_u128(hasher, vector.to_u128_bits())

		## Render the lanes for debugging, e.g. `U8x16(1, 2, 3, ...)`.
		to_inspect : U8x16 -> Str
		to_inspect = |vector| simd128_inspect("U8x16", List.map(U8x16.to_list(vector), U8.to_str))

		## The vector's 128 bits as a [U128]. Lane `i` occupies bits
		## `[i * 8, (i + 1) * 8)`. Free at runtime — no instructions.
		to_u128_bits : U8x16 -> U128
		to_u128_bits = |vector| vector.bits

		## Build a [U8x16] from 128 raw bits. Free at runtime — no
		## instructions.
		from_u128_bits : U128 -> U8x16
		from_u128_bits = |bits| U8x16.{ bits }

		## Reinterpret the same 128 bits as an [I8x16]. Free at runtime.
		to_i8x16_bits : U8x16 -> I8x16
		to_i8x16_bits = |vector| I8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U16x8]. Free at runtime.
		to_u16x8_bits : U8x16 -> U16x8
		to_u16x8_bits = |vector| U16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I16x8]. Free at runtime.
		to_i16x8_bits : U8x16 -> I16x8
		to_i16x8_bits = |vector| I16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U32x4]. Free at runtime.
		to_u32x4_bits : U8x16 -> U32x4
		to_u32x4_bits = |vector| U32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I32x4]. Free at runtime.
		to_i32x4_bits : U8x16 -> I32x4
		to_i32x4_bits = |vector| I32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U64x2]. Free at runtime.
		to_u64x2_bits : U8x16 -> U64x2
		to_u64x2_bits = |vector| U64x2.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I64x2]. Free at runtime.
		to_i64x2_bits : U8x16 -> I64x2
		to_i64x2_bits = |vector| I64x2.from_u128_bits(vector.to_u128_bits())

		## Add lane-wise, each lane wrapping mod 256.
		##
		## Lowers to `paddb` on x86-64, `add` (16×8-bit) on AArch64 NEON,
		## and `i8x16.add` on wasm.
		## ```roc
		## expect U8x16.splat(200).plus_wrap(U8x16.splat(100)).get_lane(0) == 44
		## ```
		plus_wrap : U8x16, U8x16 -> U8x16
		plus_wrap = |a, b| U8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| x + y))

		## Subtract lane-wise, each lane wrapping mod 256.
		##
		## Lowers to `psubb` on x86-64, `sub` (16×8-bit) on AArch64 NEON,
		## and `i8x16.sub` on wasm.
		minus_wrap : U8x16, U8x16 -> U8x16
		minus_wrap = |a, b| U8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| (x + 256) - y))

		## Add lane-wise, each lane saturating at 255 instead of wrapping.
		##
		## Lowers to `paddusb` on x86-64, `uqadd` on AArch64 NEON, and
		## `i8x16.add_sat_u` on wasm.
		## ```roc
		## expect U8x16.splat(200).plus_saturated(U8x16.splat(100)).get_lane(0) == 255
		## ```
		plus_saturated : U8x16, U8x16 -> U8x16
		plus_saturated = |a, b| U8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| U64.min(x + y, 255)))

		## Subtract lane-wise, each lane saturating at 0 instead of wrapping.
		##
		## Lowers to `psubusb` on x86-64, `uqsub` on AArch64 NEON, and
		## `i8x16.sub_sat_u` on wasm.
		minus_saturated : U8x16, U8x16 -> U8x16
		minus_saturated = |a, b| U8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, U64.minus_saturated))

		## The smaller of each pair of lanes.
		##
		## Lowers to `pminub` on x86-64, `umin` on AArch64 NEON, and
		## `i8x16.min_u` on wasm.
		min : U8x16, U8x16 -> U8x16
		min = |a, b| U8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, U64.min))

		## The larger of each pair of lanes.
		##
		## Lowers to `pmaxub` on x86-64, `umax` on AArch64 NEON, and
		## `i8x16.max_u` on wasm.
		max : U8x16, U8x16 -> U8x16
		max = |a, b| U8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, U64.max))

		## The absolute difference of each pair of lanes: `max - min`.
		## A core building block of PNG's Paeth filter and video codec
		## loop filters.
		##
		## Lowers to `psubusb` twice + `por` on x86-64 (no single
		## instruction), `uabd` on AArch64 NEON, and `i8x16.max_u` +
		## `i8x16.min_u` + `i8x16.sub` on wasm.
		abs_diff : U8x16, U8x16 -> U8x16
		abs_diff = |a, b| U8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, U64.abs_diff))

		## The rounding average of each pair of lanes: `(a + b + 1) >> 1`.
		## Used by chroma upsampling and intra prediction in image codecs.
		##
		## Lowers to `pavgb` on x86-64, `urhadd` on AArch64 NEON, and
		## `i8x16.avgr_u` on wasm.
		## ```roc
		## expect U8x16.splat(1).avg_rounded(U8x16.splat(2)).get_lane(0) == 2
		## ```
		avg_rounded : U8x16, U8x16 -> U8x16
		avg_rounded = |a, b| U8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| U64.shr_zf_wrap(x + y + 1, 1)))

		## Returns the bitwise AND of the two vectors' 128 bits.
		##
		## Lowers to `pand` on x86-64, `and` on AArch64 NEON, and
		## `v128.and` on wasm.
		bitwise_and : U8x16, U8x16 -> U8x16
		bitwise_and = |a, b| U8x16.from_u128_bits(U128.bitwise_and(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise OR of the two vectors' 128 bits.
		##
		## Lowers to `por` on x86-64, `orr` on AArch64 NEON, and `v128.or`
		## on wasm.
		bitwise_or : U8x16, U8x16 -> U8x16
		bitwise_or = |a, b| U8x16.from_u128_bits(U128.bitwise_or(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise XOR of the two vectors' 128 bits.
		##
		## Lowers to `pxor` on x86-64, `eor` on AArch64 NEON, and
		## `v128.xor` on wasm.
		bitwise_xor : U8x16, U8x16 -> U8x16
		bitwise_xor = |a, b| U8x16.from_u128_bits(U128.bitwise_xor(a.to_u128_bits(), b.to_u128_bits()))

		## Flips every one of the vector's 128 bits.
		##
		## Lowers to `pxor` with all-ones on x86-64, `mvn` on AArch64 NEON,
		## and `v128.not` on wasm.
		bitwise_not : U8x16 -> U8x16
		bitwise_not = |vector| U8x16.from_u128_bits(U128.bitwise_not(vector.to_u128_bits()))

		## Bitwise select: for each of the 128 bits, take the bit from
		## `if_set` where this mask vector has a 1, and from `if_clear`
		## where it has a 0. Combined with the `_lanes` comparisons, this
		## is the branchless lane-wise `if`.
		##
		## Lowers to `pand`/`pandn`/`por` on x86-64, `bsl` on AArch64 NEON,
		## and `v128.bitselect` on wasm.
		bit_select : U8x16, U8x16, U8x16 -> U8x16
		bit_select = |mask, if_set, if_clear| {
			mask_bits = mask.to_u128_bits()
			kept = U128.bitwise_and(mask_bits, if_set.to_u128_bits())
			cleared = U128.bitwise_and(U128.bitwise_not(mask_bits), if_clear.to_u128_bits())
			U8x16.from_u128_bits(U128.bitwise_or(kept, cleared))
		}

		## Compare lane-wise for equality: each result lane is 255 where
		## the lanes are equal and 0 where they differ.
		##
		## Lowers to `pcmpeqb` on x86-64, `cmeq` on AArch64 NEON, and
		## `i8x16.eq` on wasm.
		eq_lanes : U8x16, U8x16 -> U8x16
		eq_lanes = |a, b| U8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| if (x == y) U64.highest else 0))

		## Compare lane-wise: each result lane is 255 where a's lane is
		## greater than b's (unsigned), else 0.
		##
		## x86-64 has no unsigned byte compare, so this lowers to
		## `pmaxub` + `pcmpeqb` + inversion (or a sign-bias + `pcmpgtb`);
		## AArch64 NEON `cmhi`; wasm `i8x16.gt_u`.
		gt_lanes : U8x16, U8x16 -> U8x16
		gt_lanes = |a, b| U8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| if (x > y) U64.highest else 0))

		## Compare lane-wise: each result lane is 255 where a's lane is
		## less than b's (unsigned), else 0. See [U8x16.gt_lanes] for the
		## per-target lowerings.
		lt_lanes : U8x16, U8x16 -> U8x16
		lt_lanes = |a, b| U8x16.gt_lanes(b, a)

		## Compare lane-wise: each result lane is 255 where a's lane is
		## greater than or equal to b's (unsigned), else 0.
		##
		## Lowers to `pmaxub` + `pcmpeqb` on x86-64, `cmhs` on AArch64
		## NEON, and `i8x16.ge_u` on wasm.
		gte_lanes : U8x16, U8x16 -> U8x16
		gte_lanes = |a, b| U8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| if (x >= y) U64.highest else 0))

		## Compare lane-wise: each result lane is 255 where a's lane is
		## less than or equal to b's (unsigned), else 0. See
		## [U8x16.gte_lanes] for the per-target lowerings.
		lte_lanes : U8x16, U8x16 -> U8x16
		lte_lanes = |a, b| U8x16.gte_lanes(b, a)

		## One bit per lane (lane 0 in bit 0): 1 where the lane's most
		## significant bit is set. On the all-0/all-1 masks produced by the
		## `_lanes` comparisons, this packs the comparison results into a
		## [U16] for scalar decision-making (find-first-match scans, etc.).
		##
		## Lowers to `pmovmskb` on x86-64, a short `ushr`/`usra` narrowing
		## sequence on AArch64 NEON (no single instruction), and
		## `i8x16.bitmask` on wasm.
		## ```roc
		## expect U8x16.splat(255).to_bitmask() == 65535
		## ```
		to_bitmask : U8x16 -> U16
		to_bitmask = |vector| simd128_bitmask(vector.to_u128_bits(), 8).to_u16_wrap()

		## Returns `Bool.True` if any lane's most significant bit is set.
		## On comparison masks: "did any lane match?"
		any_lanes_set : U8x16 -> Bool
		any_lanes_set = |vector| U8x16.to_bitmask(vector) != 0

		## Returns `Bool.True` if every lane's most significant bit is set.
		## On comparison masks: "did all lanes match?"
		all_lanes_set : U8x16 -> Bool
		all_lanes_set = |vector| U8x16.to_bitmask(vector) == 65535

		## Shift every lane's bits left by the same count. The count is
		## taken modulo 8: shifting by 8 leaves every lane unchanged and
		## shifting by 9 shifts by 1, matching [U8.shl_wrap].
		##
		## x86-64 has no 8-bit lane shift, so this lowers to a 16-bit
		## `psllw` + `pand` mask, with the count masked to the lane width
		## first; AArch64 NEON `shl` takes the pre-masked count; wasm
		## `i8x16.shl` masks the count natively.
		shl_wrap : U8x16, U8 -> U8x16
		shl_wrap = |vector, count| U8x16.from_u128_bits(simd128_shift_left(vector.to_u128_bits(), 8, count))

		## Shift every lane's bits right by the same count, filling with
		## zeros. The count is taken modulo 8. For unsigned lanes this
		## behaves the same as [U8x16.shr_zf_wrap].
		shr_wrap : U8x16, U8 -> U8x16
		shr_wrap = |vector, count| U8x16.shr_zf_wrap(vector, count)

		## Shift every lane's bits right by the same count, filling the
		## vacated high bits with zeros. The count is taken modulo 8:
		## shifting by 8 leaves every lane unchanged and shifting by 9
		## shifts by 1, matching [U8.shr_zf_wrap].
		##
		## Lowers to `psrlw` + `pand` on x86-64 (no 8-bit lane shift), with
		## the count masked to the lane width first; `ushr` on AArch64 NEON
		## takes the pre-masked count; `i8x16.shr_u` on wasm masks the count
		## natively.
		shr_zf_wrap : U8x16, U8 -> U8x16
		shr_zf_wrap = |vector, count| U8x16.from_u128_bits(simd128_shift_right_zf(vector.to_u128_bits(), 8, count))

		## The value of the lane at the given index. Crashes if the index
		## is 16 or greater. (Lane indices are expected to be compile-time
		## constants in practice.)
		##
		## Lowers to `pextrb` on x86-64, `umov` on AArch64 NEON, and
		## `i8x16.extract_lane_u` on wasm.
		get_lane : U8x16, U64 -> U8
		get_lane = |vector, index|
			if index >= 16 {
				crash "U8x16.get_lane: lane index out of range"
			} else {
				simd128_get_lane(vector.to_u128_bits(), 8, index).to_u8_wrap()
			}

		## Returns the vector with the lane at the given index replaced by
		## the given value. Crashes if the index is 16 or greater.
		##
		## Lowers to `pinsrb` on x86-64, `ins` on AArch64 NEON, and
		## `i8x16.replace_lane` on wasm.
		with_lane : U8x16, U64, U8 -> U8x16
		with_lane = |vector, index, value|
			if index >= 16 {
				crash "U8x16.with_lane: lane index out of range"
			} else {
				U8x16.from_u128_bits(simd128_with_lane(vector.to_u128_bits(), 8, index, value.to_u64()))
			}

		## Returns a vector with every lane set to the lane of this vector
		## at the given index. Crashes if the index is 16 or greater.
		##
		## Lowers to `pshufb` with a constant pattern on x86-64, `dup`
		## (lane form) on AArch64 NEON, and a constant `i8x16.shuffle` on
		## wasm.
		broadcast_lane : U8x16, U64 -> U8x16
		broadcast_lane = |vector, index| U8x16.splat(U8x16.get_lane(vector, index))

		## Interleave the low 8 lanes of the two vectors: result lanes are
		## `a0, b0, a1, b1, ...` up through `a7, b7`. With
		## [U8x16.interleave_hi], this is the building block of matrix
		## transposes and of widening pixel data.
		##
		## Lowers to `punpcklbw` on x86-64, `zip1` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		interleave_lo : U8x16, U8x16 -> U8x16
		interleave_lo = |a, b| U8x16.from_u128_bits(simd128_interleave_lo(a.to_u128_bits(), b.to_u128_bits(), 8))

		## Interleave the high 8 lanes of the two vectors: result lanes are
		## `a8, b8, a9, b9, ...` up through `a15, b15`.
		##
		## Lowers to `punpckhbw` on x86-64, `zip2` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		interleave_hi : U8x16, U8x16 -> U8x16
		interleave_hi = |a, b| U8x16.from_u128_bits(simd128_interleave_hi(a.to_u128_bits(), b.to_u128_bits(), 8))

		## The even-indexed lanes of a followed by the even-indexed lanes
		## of b — the deinterleaving inverse of the interleave operations,
		## used to split interleaved channel data apart.
		##
		## Lowers to `pshufb`-based shuffles on x86-64, `uzp1` on AArch64
		## NEON, and a constant `i8x16.shuffle` on wasm.
		even_lanes : U8x16, U8x16 -> U8x16
		even_lanes = |a, b| U8x16.from_u128_bits(simd128_even_lanes(a.to_u128_bits(), b.to_u128_bits(), 8))

		## The odd-indexed lanes of a followed by the odd-indexed lanes of
		## b.
		##
		## Lowers to `pshufb`-based shuffles on x86-64, `uzp2` on AArch64
		## NEON, and a constant `i8x16.shuffle` on wasm.
		odd_lanes : U8x16, U8x16 -> U8x16
		odd_lanes = |a, b| U8x16.from_u128_bits(simd128_odd_lanes(a.to_u128_bits(), b.to_u128_bits(), 8))

		## Returns the vector with its 16 lanes in reverse order.
		##
		## Lowers to `pshufb` with a constant pattern on x86-64, `rev64` +
		## `ext` on AArch64 NEON, and a constant `i8x16.shuffle` on wasm.
		reverse_lanes : U8x16 -> U8x16
		reverse_lanes = |vector| U8x16.from_u128_bits(simd128_reverse_lanes(vector.to_u128_bits(), 8))

		## Treat `lo` and `hi` as one 32-byte sequence (lo's bytes first)
		## and return 16 consecutive bytes of it starting at byte `count`.
		## `count` 0 returns `lo`; 16 returns `hi`. Crashes if `count` is
		## greater than 16. This is the sliding-window operation behind
		## filter kernels and overlapped LZ77 copies.
		##
		## Lowers to `palignr` on x86-64, `ext` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		concat_shift_bytes : U8x16, U8x16, U8 -> U8x16
		concat_shift_bytes = |lo, hi, count|
			if count > 16 {
				crash "U8x16.concat_shift_bytes: count out of range"
			} else if count == 0 {
				lo
			} else if count == 16 {
				hi
			} else {
				low_part = U128.shr_zf_wrap(lo.to_u128_bits(), count * 8)
				high_part = U128.shl_wrap(hi.to_u128_bits(), (16 - count) * 8)
				U8x16.from_u128_bits(U128.bitwise_or(low_part, high_part))
			}

		## For each lane of `indices`: the lane of `table` it names, or 0
		## if the index is 16 or greater. This dynamic byte shuffle powers
		## palette lookups, nibble-table tricks, and byte rearrangement
		## with runtime patterns.
		##
		## Lowers to `pshufb` plus a one-instruction fixup on x86-64
		## (`pshufb` alone wraps indices 16-127), `tbl` on AArch64 NEON,
		## and `i8x16.swizzle` on wasm — the out-of-range-to-zero
		## semantics here matches `tbl` and `swizzle` exactly.
		## ```roc
		## expect U8x16.splat(42).table_lookup(U8x16.splat(20)).get_lane(0) == 0
		## ```
		table_lookup : U8x16, U8x16 -> U8x16
		table_lookup = |table, indices| {
			table_bits = table.to_u128_bits()
			U8x16.from_u128_bits(simd128_map1(indices.to_u128_bits(), 8, |index| if (index >= 16) 0 else simd128_get_lane(table_bits, 8, index)))
		}

		## Zero-extend the low 8 lanes into the 8 16-bit lanes of a
		## [U16x8]. With [U8x16.to_u16x8_hi], this is the widening step of
		## the widen-compute-narrow pattern most codec kernels use.
		##
		## Lowers to `pmovzxbw` on x86-64, `uxtl` on AArch64 NEON, and
		## `i16x8.extend_low_i8x16_u` on wasm.
		to_u16x8_lo : U8x16 -> U16x8
		to_u16x8_lo = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				$out = simd128_with_lane($out, 16, $i, simd128_get_lane(bits, 8, $i))
				$i = $i + 1
			}
			U16x8.from_u128_bits($out)
		}

		## Zero-extend the high 8 lanes into the 8 16-bit lanes of a
		## [U16x8].
		##
		## Lowers to `punpckhbw` with zero on x86-64, `uxtl2` on AArch64
		## NEON, and `i16x8.extend_high_i8x16_u` on wasm.
		to_u16x8_hi : U8x16 -> U16x8
		to_u16x8_hi = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				$out = simd128_with_lane($out, 16, $i, simd128_get_lane(bits, 8, 8 + $i))
				$i = $i + 1
			}
			U16x8.from_u128_bits($out)
		}

		## Add adjacent pairs of lanes into the 8 16-bit lanes of a
		## [U16x8]: result lane i is `lane(2i) + lane(2i+1)`. Used by
		## checksum and histogram accumulation.
		##
		## Lowers to `pmaddubsw` with a ones vector on x86-64, `uaddlp` on
		## AArch64 NEON, and `i16x8.extadd_pairwise_i8x16_u` on wasm.
		pairwise_plus_to_u16x8 : U8x16 -> U16x8
		pairwise_plus_to_u16x8 = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				sum = simd128_get_lane(bits, 8, $i * 2) + simd128_get_lane(bits, 8, $i * 2 + 1)
				$out = simd128_with_lane($out, 16, $i, sum)
				$i = $i + 1
			}
			U16x8.from_u128_bits($out)
		}

		## Multiply the low 8 lanes of the two vectors pairwise into the 8
		## 16-bit lanes of a [U16x8] (no overflow is possible).
		##
		## Lowers to `punpcklbw` + `pmullw` on x86-64 (no single
		## instruction), `umull` on AArch64 NEON, and
		## `i16x8.extmul_low_i8x16_u` on wasm.
		times_wide_lo : U8x16, U8x16 -> U16x8
		times_wide_lo = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				product = simd128_get_lane(a_bits, 8, $i) * simd128_get_lane(b_bits, 8, $i)
				$out = simd128_with_lane($out, 16, $i, product)
				$i = $i + 1
			}
			U16x8.from_u128_bits($out)
		}

		## Multiply the high 8 lanes of the two vectors pairwise into the
		## 8 16-bit lanes of a [U16x8].
		##
		## Lowers to `punpckhbw` + `pmullw` on x86-64, `umull2` on AArch64
		## NEON, and `i16x8.extmul_high_i8x16_u` on wasm.
		times_wide_hi : U8x16, U8x16 -> U16x8
		times_wide_hi = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				product = simd128_get_lane(a_bits, 8, 8 + $i) * simd128_get_lane(b_bits, 8, 8 + $i)
				$out = simd128_with_lane($out, 16, $i, product)
				$i = $i + 1
			}
			U16x8.from_u128_bits($out)
		}

		## Multiply each unsigned lane of this vector with the signed lane
		## of the [I8x16] at the same index, then add adjacent product
		## pairs into the 8 16-bit lanes of an [I16x8], saturating on
		## overflow: result lane i is
		## `saturate(u(2i) * s(2i) + u(2i+1) * s(2i+1))`. This is the
		## 8-tap-filter workhorse of convolution and color conversion
		## kernels.
		##
		## Lowers to `pmaddubsw` on x86-64 (whose saturation semantics
		## this operation pins), and to widening-multiply + saturating
		## pairwise-add sequences on AArch64 NEON and wasm (no single
		## instruction there).
		dot_pairs_saturated : U8x16, I8x16 -> I16x8
		dot_pairs_saturated = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				u0 = simd128_get_lane(a_bits, 8, $i * 2).to_i64_wrap()
				u1 = simd128_get_lane(a_bits, 8, $i * 2 + 1).to_i64_wrap()
				s0 = simd128_lane_to_signed(simd128_get_lane(b_bits, 8, $i * 2), 8)
				s1 = simd128_lane_to_signed(simd128_get_lane(b_bits, 8, $i * 2 + 1), 8)
				$out = simd128_with_lane($out, 16, $i, simd128_clamp_signed(u0 * s0 + u1 * s1, 16))
				$i = $i + 1
			}
			I16x8.from_u128_bits($out)
		}

		## Sums of absolute differences: result lane 0 of the [U64x2] is
		## the sum of `|a_i - b_i|` for lanes 0-7, and result lane 1 is
		## the same for lanes 8-15. This is the encoder-search workhorse
		## (SAD-based block matching, PNG filter selection).
		##
		## Lowers to `psadbw` on x86-64 (whose output layout this
		## operation pins), a `uabdl`/`uadalp` sequence on AArch64 NEON,
		## and an emulated sequence on wasm (no single instruction).
		## ```roc
		## expect U8x16.splat(9).sums_of_abs_diffs(U8x16.splat(6)).get_lane(0) == 24
		## ```
		sums_of_abs_diffs : U8x16, U8x16 -> U64x2
		sums_of_abs_diffs = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $half = 0.U64
			while $half < 2 {
				var $sum = 0.U64
				var $i = 0.U64
				while $i < 8 {
					lane = $half * 8 + $i
					$sum = $sum + U64.abs_diff(simd128_get_lane(a_bits, 8, lane), simd128_get_lane(b_bits, 8, lane))
					$i = $i + 1
				}
				$out = simd128_with_lane($out, 64, $half, $sum)
				$half = $half + 1
			}
			U64x2.from_u128_bits($out)
		}

		## The sum of all 16 lanes (at most 4080, so it always fits).
		##
		## Lowers to `psadbw` against zero + extract on x86-64, `uaddlv`
		## on AArch64 NEON, and pairwise-add chains on wasm.
		sum_lanes : U8x16 -> U32
		sum_lanes = |vector| {
			bits = vector.to_u128_bits()
			var $sum = 0.U64
			var $i = 0.U64
			while $i < 16 {
				$sum = $sum + simd128_get_lane(bits, 8, $i)
				$i = $i + 1
			}
			$sum.to_u32_wrap()
		}

		## Read 16 bytes starting at the given byte index, as lanes in
		## little-endian order. Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`. Any alignment is fine.
		##
		## Lowers to `movdqu` on x86-64, `ldr` (Q register) on AArch64,
		## and `v128.load` on wasm.
		load : List(U8), U64 -> Try(U8x16, [OutOfBounds, ..])
		load = |bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(U8x16.from_u128_bits(simd128_from_bytes_at(bytes, index)))
			}
		}

		## Write these 16 bytes into the list starting at the given byte
		## index (in place when the list is unique), little-endian.
		## Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`.
		##
		## Lowers to `movdqu` (store form) on x86-64, `str` (Q register)
		## on AArch64, and `v128.store` on wasm.
		store : U8x16, List(U8), U64 -> Try(List(U8), [OutOfBounds, ..])
		store = |vector, bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(simd128_write_bytes_at(vector.to_u128_bits(), bytes, index))
			}
		}

		## Append these 16 bytes (little-endian) to the end of the list.
		append_to : U8x16, List(U8) -> List(U8)
		append_to = |vector, bytes| simd128_append_bytes(vector.to_u128_bits(), bytes)

		## Iterate the list 16 bytes at a time: `chunks` yields one
		## [U8x16] per full 16 bytes, and `tail` is the fewer-than-16
		## leftover bytes. This is the streaming driver for chunked byte
		## processing.
		iter_list : List(U8) -> { chunks : Iter(U8x16), tail : List(U8) }
		iter_list = |bytes| {
			len = List.len(bytes)
			chunk_count = len / 16
			chunks = Iter.custom(
				0.U64,
				Known(chunk_count),
				|start|
					if len - start >= 16 {
						Ok((U8x16.from_u128_bits(simd128_from_bytes_at(bytes, start)), start + 16))
					} else {
						Err(NoMore)
					},
			)
			{ chunks, tail: List.drop_first(bytes, chunk_count * 16) }
		}
	}

	## A 128-bit SIMD vector of 16 signed 8-bit lanes (two's complement).
	##
	## Lane `i` occupies bits `[i * 8, (i + 1) * 8)` of the vector, and the
	## byte-serialized form (used by [I8x16.load], [I8x16.store], and
	## [I8x16.to_list]) is little-endian with lane 0 first. Every operation
	## has one pinned meaning that is bit-identical on every target; the
	## compiler lowers each operation to the best instruction sequence for
	## the target CPU (SSE/AVX on x86-64, NEON on AArch64, simd128 on wasm).
	I8x16 :: { bits : U128 }.{

		## Returns the [I8x16] with every lane `0`.
		## ```roc
		## expect I8x16.default() == I8x16.splat(0)
		## ```
		default : () -> I8x16
		default = || I8x16.splat(0)

		## Returns an [I8x16] with every lane set to the given [I8].
		##
		## Lowers to `vpbroadcastb` on x86-64, `dup` on AArch64 NEON, and
		## `i8x16.splat` on wasm.
		## ```roc
		## expect I8x16.splat(7).get_lane(15) == 7
		## ```
		splat : I8 -> I8x16
		splat = |value| I8x16.from_u128_bits(simd128_splat(value.to_u64_wrap(), 8))

		## Build an [I8x16] from exactly 16 lane values, lane 0 first.
		## Returns `Err(WrongLength)` if the list's length is not 16.
		from_list : List(I8) -> Try(I8x16, [WrongLength, ..])
		from_list = |lanes|
			if List.len(lanes) != 16 {
				Err(WrongLength)
			} else {
				var $bits = 0.U128
				var $i = 0.U64
				while $i < 16 {
					$bits = simd128_with_lane($bits, 8, $i, list_get_unsafe(lanes, $i).to_u64_wrap())
					$i = $i + 1
				}
				Ok(I8x16.from_u128_bits($bits))
			}

		## The 16 lane values as a list, lane 0 first.
		## ```roc
		## expect I8x16.splat(9).to_list() == List.repeat(9.I8, 16)
		## ```
		to_list : I8x16 -> List(I8)
		to_list = |vector| {
			var $out = List.with_capacity(16)
			var $i = 0.U64
			while $i < 16 {
				$out = list_append_unsafe($out, I8x16.get_lane(vector, $i))
				$i = $i + 1
			}
			$out
		}

		## Returns `Bool.True` if all 128 bits of the two vectors are equal.
		## (For a per-lane comparison producing a mask, see [I8x16.eq_lanes].)
		##
		## Lowers to `pxor` + `ptest` on x86-64, `cmeq` + `uminv` on AArch64
		## NEON, and `v128.xor` + `v128.any_true` on wasm.
		is_eq : I8x16, I8x16 -> Bool
		is_eq = |a, b| a.to_u128_bits() == b.to_u128_bits()

		## Feed an [I8x16] into a [Hasher].
		to_hash : I8x16, Hasher -> Hasher
		to_hash = |vector, hasher| Hasher.write_u128(hasher, vector.to_u128_bits())

		## Render the lanes for debugging, e.g. `I8x16(1, 2, 3, ...)`.
		to_inspect : I8x16 -> Str
		to_inspect = |vector| simd128_inspect("I8x16", List.map(I8x16.to_list(vector), I8.to_str))

		## The vector's 128 bits as a [U128]. Lane `i` occupies bits
		## `[i * 8, (i + 1) * 8)`. Free at runtime — no instructions.
		to_u128_bits : I8x16 -> U128
		to_u128_bits = |vector| vector.bits

		## Build an [I8x16] from 128 raw bits. Free at runtime — no
		## instructions.
		from_u128_bits : U128 -> I8x16
		from_u128_bits = |bits| I8x16.{ bits }

		## Reinterpret the same 128 bits as a [U8x16]. Free at runtime.
		to_u8x16_bits : I8x16 -> U8x16
		to_u8x16_bits = |vector| U8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U16x8]. Free at runtime.
		to_u16x8_bits : I8x16 -> U16x8
		to_u16x8_bits = |vector| U16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I16x8]. Free at runtime.
		to_i16x8_bits : I8x16 -> I16x8
		to_i16x8_bits = |vector| I16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U32x4]. Free at runtime.
		to_u32x4_bits : I8x16 -> U32x4
		to_u32x4_bits = |vector| U32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I32x4]. Free at runtime.
		to_i32x4_bits : I8x16 -> I32x4
		to_i32x4_bits = |vector| I32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U64x2]. Free at runtime.
		to_u64x2_bits : I8x16 -> U64x2
		to_u64x2_bits = |vector| U64x2.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I64x2]. Free at runtime.
		to_i64x2_bits : I8x16 -> I64x2
		to_i64x2_bits = |vector| I64x2.from_u128_bits(vector.to_u128_bits())

		## Add lane-wise, each lane wrapping mod 256 (two's complement).
		##
		## Lowers to `paddb` on x86-64, `add` (16×8-bit) on AArch64 NEON,
		## and `i8x16.add` on wasm.
		## ```roc
		## expect I8x16.splat(127).plus_wrap(I8x16.splat(1)).get_lane(0) == -128
		## ```
		plus_wrap : I8x16, I8x16 -> I8x16
		plus_wrap = |a, b| I8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| x + y))

		## Subtract lane-wise, each lane wrapping mod 256 (two's complement).
		##
		## Lowers to `psubb` on x86-64, `sub` (16×8-bit) on AArch64 NEON,
		## and `i8x16.sub` on wasm.
		minus_wrap : I8x16, I8x16 -> I8x16
		minus_wrap = |a, b| I8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| (x + 256) - y))

		## Add lane-wise, each lane saturating within the signed range
		## -128 to 127 instead of wrapping.
		##
		## Lowers to `paddsb` on x86-64, `sqadd` on AArch64 NEON, and
		## `i8x16.add_sat_s` on wasm.
		## ```roc
		## expect I8x16.splat(100).plus_saturated(I8x16.splat(100)).get_lane(0) == 127
		## ```
		plus_saturated : I8x16, I8x16 -> I8x16
		plus_saturated = |a, b| I8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| simd128_clamp_signed(simd128_lane_to_signed(x, 8) + simd128_lane_to_signed(y, 8), 8)))

		## Subtract lane-wise, each lane saturating within the signed range
		## -128 to 127 instead of wrapping.
		##
		## Lowers to `psubsb` on x86-64, `sqsub` on AArch64 NEON, and
		## `i8x16.sub_sat_s` on wasm.
		minus_saturated : I8x16, I8x16 -> I8x16
		minus_saturated = |a, b| I8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| simd128_clamp_signed(simd128_lane_to_signed(x, 8) - simd128_lane_to_signed(y, 8), 8)))

		## Negate each lane, wrapping mod 256. The lane holding -128 negates
		## to itself (-128), since +128 is not representable.
		##
		## Lowers to `psubb` from zero on x86-64, `neg` on AArch64 NEON, and
		## `i8x16.neg` on wasm.
		## ```roc
		## expect I8x16.splat(5).negate_wrap().get_lane(0) == -5
		## ```
		negate_wrap : I8x16 -> I8x16
		negate_wrap = |vector| I8x16.from_u128_bits(simd128_map1(vector.to_u128_bits(), 8, |x| 256 - x))

		## The absolute value of each lane. The lane holding -128 has no
		## representable positive absolute value, so it wraps back to -128.
		##
		## Lowers to `pabsb` on x86-64, `abs` on AArch64 NEON, and
		## `i8x16.abs` on wasm.
		## ```roc
		## expect I8x16.splat(-5).abs_wrap().get_lane(0) == 5
		## ```
		abs_wrap : I8x16 -> I8x16
		abs_wrap = |vector| I8x16.from_u128_bits(simd128_map1(vector.to_u128_bits(), 8, |x| if (simd128_lane_to_signed(x, 8) < 0) (256 - x) else x))

		## The smaller of each pair of lanes (signed).
		##
		## Lowers to `pminsb` (SSE4.1) on x86-64, `smin` on AArch64 NEON,
		## and `i8x16.min_s` on wasm.
		## ```roc
		## expect I8x16.splat(-3).min(I8x16.splat(2)).get_lane(0) == -3
		## ```
		min : I8x16, I8x16 -> I8x16
		min = |a, b| I8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| I64.min(simd128_lane_to_signed(x, 8), simd128_lane_to_signed(y, 8)).to_u64_wrap()))

		## The larger of each pair of lanes (signed).
		##
		## Lowers to `pmaxsb` (SSE4.1) on x86-64, `smax` on AArch64 NEON,
		## and `i8x16.max_s` on wasm.
		max : I8x16, I8x16 -> I8x16
		max = |a, b| I8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| I64.max(simd128_lane_to_signed(x, 8), simd128_lane_to_signed(y, 8)).to_u64_wrap()))

		## Returns the bitwise AND of the two vectors' 128 bits.
		##
		## Lowers to `pand` on x86-64, `and` on AArch64 NEON, and
		## `v128.and` on wasm.
		bitwise_and : I8x16, I8x16 -> I8x16
		bitwise_and = |a, b| I8x16.from_u128_bits(U128.bitwise_and(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise OR of the two vectors' 128 bits.
		##
		## Lowers to `por` on x86-64, `orr` on AArch64 NEON, and `v128.or`
		## on wasm.
		bitwise_or : I8x16, I8x16 -> I8x16
		bitwise_or = |a, b| I8x16.from_u128_bits(U128.bitwise_or(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise XOR of the two vectors' 128 bits.
		##
		## Lowers to `pxor` on x86-64, `eor` on AArch64 NEON, and
		## `v128.xor` on wasm.
		bitwise_xor : I8x16, I8x16 -> I8x16
		bitwise_xor = |a, b| I8x16.from_u128_bits(U128.bitwise_xor(a.to_u128_bits(), b.to_u128_bits()))

		## Flips every one of the vector's 128 bits.
		##
		## Lowers to `pxor` with all-ones on x86-64, `mvn` on AArch64 NEON,
		## and `v128.not` on wasm.
		bitwise_not : I8x16 -> I8x16
		bitwise_not = |vector| I8x16.from_u128_bits(U128.bitwise_not(vector.to_u128_bits()))

		## Bitwise select: for each of the 128 bits, take the bit from
		## `if_set` where this mask vector has a 1, and from `if_clear`
		## where it has a 0. Combined with the `_lanes` comparisons, this
		## is the branchless lane-wise `if`.
		##
		## Lowers to `pand`/`pandn`/`por` on x86-64, `bsl` on AArch64 NEON,
		## and `v128.bitselect` on wasm.
		bit_select : I8x16, I8x16, I8x16 -> I8x16
		bit_select = |mask, if_set, if_clear| {
			mask_bits = mask.to_u128_bits()
			kept = U128.bitwise_and(mask_bits, if_set.to_u128_bits())
			cleared = U128.bitwise_and(U128.bitwise_not(mask_bits), if_clear.to_u128_bits())
			I8x16.from_u128_bits(U128.bitwise_or(kept, cleared))
		}

		## Compare lane-wise for equality: each result lane is -1 (all bits
		## set) where the lanes are equal and 0 where they differ.
		##
		## Lowers to `pcmpeqb` on x86-64, `cmeq` on AArch64 NEON, and
		## `i8x16.eq` on wasm.
		eq_lanes : I8x16, I8x16 -> I8x16
		eq_lanes = |a, b| I8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| if (x == y) U64.highest else 0))

		## Compare lane-wise: each result lane is -1 (all bits set) where a's
		## lane is greater than b's (signed), else 0.
		##
		## Lowers to `pcmpgtb` on x86-64, `cmgt` on AArch64 NEON, and
		## `i8x16.gt_s` on wasm.
		## ```roc
		## expect I8x16.splat(1).gt_lanes(I8x16.splat(-1)).get_lane(0) == -1
		## ```
		gt_lanes : I8x16, I8x16 -> I8x16
		gt_lanes = |a, b| I8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| if (simd128_lane_to_signed(x, 8) > simd128_lane_to_signed(y, 8)) U64.highest else 0))

		## Compare lane-wise: each result lane is -1 (all bits set) where a's
		## lane is less than b's (signed), else 0. See [I8x16.gt_lanes] for
		## the per-target lowerings.
		lt_lanes : I8x16, I8x16 -> I8x16
		lt_lanes = |a, b| I8x16.gt_lanes(b, a)

		## Compare lane-wise: each result lane is -1 (all bits set) where a's
		## lane is greater than or equal to b's (signed), else 0.
		##
		## Lowers to `pcmpgtb` + `pcmpeqb` + `por` on x86-64, `cmge` on
		## AArch64 NEON, and `i8x16.ge_s` on wasm.
		gte_lanes : I8x16, I8x16 -> I8x16
		gte_lanes = |a, b| I8x16.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 8, |x, y| if (simd128_lane_to_signed(x, 8) >= simd128_lane_to_signed(y, 8)) U64.highest else 0))

		## Compare lane-wise: each result lane is -1 (all bits set) where a's
		## lane is less than or equal to b's (signed), else 0. See
		## [I8x16.gte_lanes] for the per-target lowerings.
		lte_lanes : I8x16, I8x16 -> I8x16
		lte_lanes = |a, b| I8x16.gte_lanes(b, a)

		## One bit per lane (lane 0 in bit 0): 1 where the lane's most
		## significant bit is set. On the all-0/all-1 masks produced by the
		## `_lanes` comparisons, this packs the comparison results into a
		## [U16] for scalar decision-making (find-first-match scans, etc.).
		##
		## Lowers to `pmovmskb` on x86-64, a short `ushr`/`usra` narrowing
		## sequence on AArch64 NEON (no single instruction), and
		## `i8x16.bitmask` on wasm.
		## ```roc
		## expect I8x16.splat(-1).to_bitmask() == 65535
		## ```
		to_bitmask : I8x16 -> U16
		to_bitmask = |vector| simd128_bitmask(vector.to_u128_bits(), 8).to_u16_wrap()

		## Returns `Bool.True` if any lane's most significant bit is set.
		## On comparison masks: "did any lane match?"
		any_lanes_set : I8x16 -> Bool
		any_lanes_set = |vector| I8x16.to_bitmask(vector) != 0

		## Returns `Bool.True` if every lane's most significant bit is set.
		## On comparison masks: "did all lanes match?"
		all_lanes_set : I8x16 -> Bool
		all_lanes_set = |vector| I8x16.to_bitmask(vector) == 65535

		## Shift every lane's bits left by the same count. The count is
		## taken modulo 8: shifting by 8 leaves every lane unchanged and
		## shifting by 9 shifts by 1, matching [I8.shl_wrap].
		##
		## x86-64 has no 8-bit lane shift, so this lowers to a 16-bit
		## `psllw` + `pand` mask, with the count masked to the lane width
		## first; AArch64 NEON `shl` takes the pre-masked count; wasm
		## `i8x16.shl` masks the count natively.
		shl_wrap : I8x16, U8 -> I8x16
		shl_wrap = |vector, count| I8x16.from_u128_bits(simd128_shift_left(vector.to_u128_bits(), 8, count))

		## Shift every lane's bits right by the same count, filling the
		## vacated high bits with zeros. The count is taken modulo 8:
		## shifting by 8 leaves every lane unchanged and shifting by 9
		## shifts by 1, matching [I8.shr_zf_wrap].
		##
		## Lowers to `psrlw` + `pand` on x86-64 (no 8-bit lane shift), with
		## the count masked to the lane width first; `ushr` on AArch64 NEON
		## takes the pre-masked count; `i8x16.shr_u` on wasm masks the count
		## natively.
		shr_zf_wrap : I8x16, U8 -> I8x16
		shr_zf_wrap = |vector, count| I8x16.from_u128_bits(simd128_shift_right_zf(vector.to_u128_bits(), 8, count))

		## Shift every lane's bits right by the same count, replicating the
		## sign bit into the vacated high bits. The count is taken modulo 8:
		## shifting by 8 leaves every lane unchanged and shifting by 9
		## shifts by 1, matching [I8.shr_wrap].
		##
		## Lowers to `psraw` + a mask sequence on x86-64 (no 8-bit lane
		## shift), with the count masked to the lane width first; `sshr` on
		## AArch64 NEON takes the pre-masked count; `i8x16.shr_s` on wasm
		## masks the count natively.
		## ```roc
		## expect I8x16.splat(-8).shr_wrap(1).get_lane(0) == -4
		## ```
		shr_wrap : I8x16, U8 -> I8x16
		shr_wrap = |vector, count| I8x16.from_u128_bits(simd128_shift_right_arith(vector.to_u128_bits(), 8, count))

		## The value of the lane at the given index. Crashes if the index
		## is 16 or greater. (Lane indices are expected to be compile-time
		## constants in practice.)
		##
		## Lowers to `pextrb` on x86-64, `smov` on AArch64 NEON, and
		## `i8x16.extract_lane_s` on wasm.
		get_lane : I8x16, U64 -> I8
		get_lane = |vector, index|
			if index >= 16 {
				crash "I8x16.get_lane: lane index out of range"
			} else {
				simd128_lane_to_signed(simd128_get_lane(vector.to_u128_bits(), 8, index), 8).to_i8_wrap()
			}

		## Returns the vector with the lane at the given index replaced by
		## the given value. Crashes if the index is 16 or greater.
		##
		## Lowers to `pinsrb` on x86-64, `ins` on AArch64 NEON, and
		## `i8x16.replace_lane` on wasm.
		with_lane : I8x16, U64, I8 -> I8x16
		with_lane = |vector, index, value|
			if index >= 16 {
				crash "I8x16.with_lane: lane index out of range"
			} else {
				I8x16.from_u128_bits(simd128_with_lane(vector.to_u128_bits(), 8, index, value.to_u64_wrap()))
			}

		## Returns a vector with every lane set to the lane of this vector
		## at the given index. Crashes if the index is 16 or greater.
		##
		## Lowers to `pshufb` with a constant pattern on x86-64, `dup`
		## (lane form) on AArch64 NEON, and a constant `i8x16.shuffle` on
		## wasm.
		broadcast_lane : I8x16, U64 -> I8x16
		broadcast_lane = |vector, index| I8x16.splat(I8x16.get_lane(vector, index))

		## Interleave the low 8 lanes of the two vectors: result lanes are
		## `a0, b0, a1, b1, ...` up through `a7, b7`. With
		## [I8x16.interleave_hi], this is the building block of matrix
		## transposes and of widening pixel data.
		##
		## Lowers to `punpcklbw` on x86-64, `zip1` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		interleave_lo : I8x16, I8x16 -> I8x16
		interleave_lo = |a, b| I8x16.from_u128_bits(simd128_interleave_lo(a.to_u128_bits(), b.to_u128_bits(), 8))

		## Interleave the high 8 lanes of the two vectors: result lanes are
		## `a8, b8, a9, b9, ...` up through `a15, b15`.
		##
		## Lowers to `punpckhbw` on x86-64, `zip2` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		interleave_hi : I8x16, I8x16 -> I8x16
		interleave_hi = |a, b| I8x16.from_u128_bits(simd128_interleave_hi(a.to_u128_bits(), b.to_u128_bits(), 8))

		## The even-indexed lanes of a followed by the even-indexed lanes
		## of b — the deinterleaving inverse of the interleave operations,
		## used to split interleaved channel data apart.
		##
		## Lowers to `pshufb`-based shuffles on x86-64, `uzp1` on AArch64
		## NEON, and a constant `i8x16.shuffle` on wasm.
		even_lanes : I8x16, I8x16 -> I8x16
		even_lanes = |a, b| I8x16.from_u128_bits(simd128_even_lanes(a.to_u128_bits(), b.to_u128_bits(), 8))

		## The odd-indexed lanes of a followed by the odd-indexed lanes of
		## b.
		##
		## Lowers to `pshufb`-based shuffles on x86-64, `uzp2` on AArch64
		## NEON, and a constant `i8x16.shuffle` on wasm.
		odd_lanes : I8x16, I8x16 -> I8x16
		odd_lanes = |a, b| I8x16.from_u128_bits(simd128_odd_lanes(a.to_u128_bits(), b.to_u128_bits(), 8))

		## Returns the vector with its 16 lanes in reverse order.
		##
		## Lowers to `pshufb` with a constant pattern on x86-64, `rev64` +
		## `ext` on AArch64 NEON, and a constant `i8x16.shuffle` on wasm.
		reverse_lanes : I8x16 -> I8x16
		reverse_lanes = |vector| I8x16.from_u128_bits(simd128_reverse_lanes(vector.to_u128_bits(), 8))

		## Sign-extend the low 8 lanes into the 8 16-bit lanes of an
		## [I16x8]. With [I8x16.to_i16x8_hi], this is the widening step of
		## the widen-compute-narrow pattern most codec kernels use.
		##
		## Lowers to `pmovsxbw` (SSE4.1) on x86-64, `sxtl` on AArch64 NEON,
		## and `i16x8.extend_low_i8x16_s` on wasm.
		to_i16x8_lo : I8x16 -> I16x8
		to_i16x8_lo = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				$out = simd128_with_lane($out, 16, $i, simd128_lane_to_signed(simd128_get_lane(bits, 8, $i), 8).to_u64_wrap())
				$i = $i + 1
			}
			I16x8.from_u128_bits($out)
		}

		## Sign-extend the high 8 lanes into the 8 16-bit lanes of an
		## [I16x8].
		##
		## Lowers to `pmovsxbw` of the high half on x86-64, `sxtl2` on
		## AArch64 NEON, and `i16x8.extend_high_i8x16_s` on wasm.
		to_i16x8_hi : I8x16 -> I16x8
		to_i16x8_hi = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				$out = simd128_with_lane($out, 16, $i, simd128_lane_to_signed(simd128_get_lane(bits, 8, 8 + $i), 8).to_u64_wrap())
				$i = $i + 1
			}
			I16x8.from_u128_bits($out)
		}

		## Add adjacent pairs of lanes into the 8 16-bit lanes of an
		## [I16x8]: result lane i is `lane(2i) + lane(2i+1)` (signed, always
		## in range). Used by checksum and histogram accumulation.
		##
		## Lowers to an emulated sequence on x86-64 (no single instruction),
		## `saddlp` on AArch64 NEON, and `i16x8.extadd_pairwise_i8x16_s` on
		## wasm.
		pairwise_plus_to_i16x8 : I8x16 -> I16x8
		pairwise_plus_to_i16x8 = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				sum = simd128_lane_to_signed(simd128_get_lane(bits, 8, $i * 2), 8) + simd128_lane_to_signed(simd128_get_lane(bits, 8, $i * 2 + 1), 8)
				$out = simd128_with_lane($out, 16, $i, sum.to_u64_wrap())
				$i = $i + 1
			}
			I16x8.from_u128_bits($out)
		}

		## Multiply the low 8 lanes of the two vectors pairwise (signed)
		## into the 8 16-bit lanes of an [I16x8] (no overflow is possible).
		##
		## Lowers to `pmovsxbw` + `pmullw` on x86-64 (no single
		## instruction), `smull` on AArch64 NEON, and
		## `i16x8.extmul_low_i8x16_s` on wasm.
		times_wide_lo : I8x16, I8x16 -> I16x8
		times_wide_lo = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				product = simd128_lane_to_signed(simd128_get_lane(a_bits, 8, $i), 8) * simd128_lane_to_signed(simd128_get_lane(b_bits, 8, $i), 8)
				$out = simd128_with_lane($out, 16, $i, product.to_u64_wrap())
				$i = $i + 1
			}
			I16x8.from_u128_bits($out)
		}

		## Multiply the high 8 lanes of the two vectors pairwise (signed)
		## into the 8 16-bit lanes of an [I16x8].
		##
		## Lowers to `pmovsxbw` + `pmullw` on x86-64 (no single
		## instruction), `smull2` on AArch64 NEON, and
		## `i16x8.extmul_high_i8x16_s` on wasm.
		times_wide_hi : I8x16, I8x16 -> I16x8
		times_wide_hi = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				product = simd128_lane_to_signed(simd128_get_lane(a_bits, 8, 8 + $i), 8) * simd128_lane_to_signed(simd128_get_lane(b_bits, 8, 8 + $i), 8)
				$out = simd128_with_lane($out, 16, $i, product.to_u64_wrap())
				$i = $i + 1
			}
			I16x8.from_u128_bits($out)
		}

		## The sum of all 16 lanes (signed, in the range -2048 to 2032, so
		## it always fits).
		##
		## Lowers to an emulated sequence on x86-64, `saddlv` on AArch64
		## NEON, and pairwise-add chains on wasm.
		## ```roc
		## expect I8x16.splat(1).sum_lanes() == 16
		## ```
		sum_lanes : I8x16 -> I32
		sum_lanes = |vector| {
			bits = vector.to_u128_bits()
			var $sum = 0.I64
			var $i = 0.U64
			while $i < 16 {
				$sum = $sum + simd128_lane_to_signed(simd128_get_lane(bits, 8, $i), 8)
				$i = $i + 1
			}
			$sum.to_i32_wrap()
		}

		## Read 16 bytes starting at the given byte index, as lanes in
		## little-endian order. Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`. Any alignment is fine.
		##
		## Lowers to `movdqu` on x86-64, `ldr` (Q register) on AArch64,
		## and `v128.load` on wasm.
		load : List(U8), U64 -> Try(I8x16, [OutOfBounds, ..])
		load = |bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(I8x16.from_u128_bits(simd128_from_bytes_at(bytes, index)))
			}
		}

		## Write these 16 bytes into the list starting at the given byte
		## index (in place when the list is unique), little-endian.
		## Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`.
		##
		## Lowers to `movdqu` (store form) on x86-64, `str` (Q register)
		## on AArch64, and `v128.store` on wasm.
		store : I8x16, List(U8), U64 -> Try(List(U8), [OutOfBounds, ..])
		store = |vector, bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(simd128_write_bytes_at(vector.to_u128_bits(), bytes, index))
			}
		}

		## Append these 16 bytes (little-endian) to the end of the list.
		append_to : I8x16, List(U8) -> List(U8)
		append_to = |vector, bytes| simd128_append_bytes(vector.to_u128_bits(), bytes)
	}

	## A 128-bit SIMD vector of 8 unsigned 16-bit lanes.
	##
	## Lane `i` occupies bits `[i * 16, (i + 1) * 16)` of the vector, and
	## the byte-serialized form (used by [U16x8.load] and [U16x8.store]) is
	## little-endian: lane 0 first, each lane's two bytes least-significant
	## first. Every operation has one pinned meaning that is bit-identical
	## on every target; the compiler lowers each operation to the best
	## instruction sequence for the target CPU (SSE/AVX on x86-64, NEON on
	## AArch64, simd128 on wasm).
	U16x8 :: { bits : U128 }.{

		## Returns the [U16x8] with every lane `0`.
		## ```roc
		## expect U16x8.default() == U16x8.splat(0)
		## ```
		default : () -> U16x8
		default = || U16x8.splat(0)

		## Returns a [U16x8] with every lane set to the given [U16].
		##
		## Lowers to `vpbroadcastw` on x86-64, `dup` on AArch64 NEON, and
		## `i16x8.splat` on wasm.
		## ```roc
		## expect U16x8.splat(7).get_lane(7) == 7
		## ```
		splat : U16 -> U16x8
		splat = |value| U16x8.from_u128_bits(simd128_splat(value.to_u64(), 16))

		## Build a [U16x8] from exactly 8 lane values, lane 0 first.
		## Returns `Err(WrongLength)` if the list's length is not 8.
		from_list : List(U16) -> Try(U16x8, [WrongLength, ..])
		from_list = |lanes|
			if List.len(lanes) != 8 {
				Err(WrongLength)
			} else {
				var $bits = 0.U128
				var $i = 0.U64
				while $i < 8 {
					$bits = simd128_with_lane($bits, 16, $i, list_get_unsafe(lanes, $i).to_u64())
					$i = $i + 1
				}
				Ok(U16x8.from_u128_bits($bits))
			}

		## The 8 lane values as a list, lane 0 first.
		## ```roc
		## expect U16x8.splat(9).to_list() == List.repeat(9.U16, 8)
		## ```
		to_list : U16x8 -> List(U16)
		to_list = |vector| {
			var $out = List.with_capacity(8)
			var $i = 0.U64
			while $i < 8 {
				$out = list_append_unsafe($out, U16x8.get_lane(vector, $i))
				$i = $i + 1
			}
			$out
		}

		## Returns `Bool.True` if all 128 bits of the two vectors are equal.
		## (For a per-lane comparison producing a mask, see [U16x8.eq_lanes].)
		##
		## Lowers to `pxor` + `ptest` on x86-64, `cmeq` + `uminv` on AArch64
		## NEON, and `v128.xor` + `v128.any_true` on wasm.
		is_eq : U16x8, U16x8 -> Bool
		is_eq = |a, b| a.to_u128_bits() == b.to_u128_bits()

		## Feed a [U16x8] into a [Hasher].
		to_hash : U16x8, Hasher -> Hasher
		to_hash = |vector, hasher| Hasher.write_u128(hasher, vector.to_u128_bits())

		## Render the lanes for debugging, e.g. `U16x8(1, 2, 3, ...)`.
		to_inspect : U16x8 -> Str
		to_inspect = |vector| simd128_inspect("U16x8", List.map(U16x8.to_list(vector), U16.to_str))

		## The vector's 128 bits as a [U128]. Lane `i` occupies bits
		## `[i * 16, (i + 1) * 16)`. Free at runtime — no instructions.
		to_u128_bits : U16x8 -> U128
		to_u128_bits = |vector| vector.bits

		## Build a [U16x8] from 128 raw bits. Free at runtime — no
		## instructions.
		from_u128_bits : U128 -> U16x8
		from_u128_bits = |bits| U16x8.{ bits }

		## Reinterpret the same 128 bits as a [U8x16]. Free at runtime.
		to_u8x16_bits : U16x8 -> U8x16
		to_u8x16_bits = |vector| U8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I8x16]. Free at runtime.
		to_i8x16_bits : U16x8 -> I8x16
		to_i8x16_bits = |vector| I8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I16x8]. Free at runtime.
		to_i16x8_bits : U16x8 -> I16x8
		to_i16x8_bits = |vector| I16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U32x4]. Free at runtime.
		to_u32x4_bits : U16x8 -> U32x4
		to_u32x4_bits = |vector| U32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I32x4]. Free at runtime.
		to_i32x4_bits : U16x8 -> I32x4
		to_i32x4_bits = |vector| I32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U64x2]. Free at runtime.
		to_u64x2_bits : U16x8 -> U64x2
		to_u64x2_bits = |vector| U64x2.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I64x2]. Free at runtime.
		to_i64x2_bits : U16x8 -> I64x2
		to_i64x2_bits = |vector| I64x2.from_u128_bits(vector.to_u128_bits())

		## Add lane-wise, each lane wrapping mod 65536.
		##
		## Lowers to `paddw` on x86-64, `add` (8×16-bit) on AArch64 NEON,
		## and `i16x8.add` on wasm.
		## ```roc
		## expect U16x8.splat(60000).plus_wrap(U16x8.splat(10000)).get_lane(0) == 4464
		## ```
		plus_wrap : U16x8, U16x8 -> U16x8
		plus_wrap = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| x + y))

		## Subtract lane-wise, each lane wrapping mod 65536.
		##
		## Lowers to `psubw` on x86-64, `sub` (8×16-bit) on AArch64 NEON,
		## and `i16x8.sub` on wasm.
		minus_wrap : U16x8, U16x8 -> U16x8
		minus_wrap = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| (x + 65536) - y))

		## Add lane-wise, each lane saturating at 65535 instead of wrapping.
		##
		## Lowers to `paddusw` on x86-64, `uqadd` on AArch64 NEON, and
		## `i16x8.add_sat_u` on wasm.
		## ```roc
		## expect U16x8.splat(60000).plus_saturated(U16x8.splat(10000)).get_lane(0) == 65535
		## ```
		plus_saturated : U16x8, U16x8 -> U16x8
		plus_saturated = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| U64.min(x + y, 65535)))

		## Subtract lane-wise, each lane saturating at 0 instead of wrapping.
		##
		## Lowers to `psubusw` on x86-64, `uqsub` on AArch64 NEON, and
		## `i16x8.sub_sat_u` on wasm.
		minus_saturated : U16x8, U16x8 -> U16x8
		minus_saturated = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, U64.minus_saturated))

		## The smaller of each pair of lanes.
		##
		## Lowers to `pminuw` (SSE4.1) on x86-64, `umin` on AArch64 NEON,
		## and `i16x8.min_u` on wasm.
		min : U16x8, U16x8 -> U16x8
		min = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, U64.min))

		## The larger of each pair of lanes.
		##
		## Lowers to `pmaxuw` (SSE4.1) on x86-64, `umax` on AArch64 NEON,
		## and `i16x8.max_u` on wasm.
		max : U16x8, U16x8 -> U16x8
		max = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, U64.max))

		## The absolute difference of each pair of lanes: `max - min`.
		##
		## Lowers to `psubusw` twice + `por` on x86-64 (no single
		## instruction), `uabd` on AArch64 NEON, and `i16x8.max_u` +
		## `i16x8.min_u` + `i16x8.sub` on wasm.
		abs_diff : U16x8, U16x8 -> U16x8
		abs_diff = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, U64.abs_diff))

		## The rounding average of each pair of lanes: `(a + b + 1) >> 1`.
		##
		## Lowers to `pavgw` on x86-64, `urhadd` on AArch64 NEON, and
		## `i16x8.avgr_u` on wasm.
		## ```roc
		## expect U16x8.splat(10).avg_rounded(U16x8.splat(15)).get_lane(0) == 13
		## ```
		avg_rounded : U16x8, U16x8 -> U16x8
		avg_rounded = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| U64.shr_zf_wrap(x + y + 1, 1)))

		## Multiply lane-wise, each lane wrapping mod 65536.
		##
		## Lowers to `pmullw` on x86-64, `mul` on AArch64 NEON, and
		## `i16x8.mul` on wasm.
		## ```roc
		## expect U16x8.splat(1000).times_wrap(U16x8.splat(1000)).get_lane(0) == 16960
		## ```
		times_wrap : U16x8, U16x8 -> U16x8
		times_wrap = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| x * y))

		## The high 16 bits of each lane-wise product: `(a * b) >> 16`.
		##
		## Lowers to `pmulhuw` on x86-64, a `umull` + `shrn` sequence on
		## AArch64 NEON (no single instruction), and an emulated sequence on
		## wasm (no single instruction).
		## ```roc
		## expect U16x8.splat(1000).times_high(U16x8.splat(1000)).get_lane(0) == 15
		## ```
		times_high : U16x8, U16x8 -> U16x8
		times_high = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| U64.shr_zf_wrap(x * y, 16)))

		## Returns the bitwise AND of the two vectors' 128 bits.
		##
		## Lowers to `pand` on x86-64, `and` on AArch64 NEON, and
		## `v128.and` on wasm.
		bitwise_and : U16x8, U16x8 -> U16x8
		bitwise_and = |a, b| U16x8.from_u128_bits(U128.bitwise_and(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise OR of the two vectors' 128 bits.
		##
		## Lowers to `por` on x86-64, `orr` on AArch64 NEON, and `v128.or`
		## on wasm.
		bitwise_or : U16x8, U16x8 -> U16x8
		bitwise_or = |a, b| U16x8.from_u128_bits(U128.bitwise_or(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise XOR of the two vectors' 128 bits.
		##
		## Lowers to `pxor` on x86-64, `eor` on AArch64 NEON, and
		## `v128.xor` on wasm.
		bitwise_xor : U16x8, U16x8 -> U16x8
		bitwise_xor = |a, b| U16x8.from_u128_bits(U128.bitwise_xor(a.to_u128_bits(), b.to_u128_bits()))

		## Flips every one of the vector's 128 bits.
		##
		## Lowers to `pxor` with all-ones on x86-64, `mvn` on AArch64 NEON,
		## and `v128.not` on wasm.
		bitwise_not : U16x8 -> U16x8
		bitwise_not = |vector| U16x8.from_u128_bits(U128.bitwise_not(vector.to_u128_bits()))

		## Bitwise select: for each of the 128 bits, take the bit from
		## `if_set` where this mask vector has a 1, and from `if_clear`
		## where it has a 0. Combined with the `_lanes` comparisons, this
		## is the branchless lane-wise `if`.
		##
		## Lowers to `pand`/`pandn`/`por` on x86-64, `bsl` on AArch64 NEON,
		## and `v128.bitselect` on wasm.
		bit_select : U16x8, U16x8, U16x8 -> U16x8
		bit_select = |mask, if_set, if_clear| {
			mask_bits = mask.to_u128_bits()
			kept = U128.bitwise_and(mask_bits, if_set.to_u128_bits())
			cleared = U128.bitwise_and(U128.bitwise_not(mask_bits), if_clear.to_u128_bits())
			U16x8.from_u128_bits(U128.bitwise_or(kept, cleared))
		}

		## Compare lane-wise for equality: each result lane is 65535 where
		## the lanes are equal and 0 where they differ.
		##
		## Lowers to `pcmpeqw` on x86-64, `cmeq` on AArch64 NEON, and
		## `i16x8.eq` on wasm.
		eq_lanes : U16x8, U16x8 -> U16x8
		eq_lanes = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| if (x == y) U64.highest else 0))

		## Compare lane-wise: each result lane is 65535 where a's lane is
		## greater than b's (unsigned), else 0.
		##
		## x86-64 has no unsigned 16-bit compare, so this lowers to
		## `pmaxuw` + `pcmpeqw` + inversion (or a sign-bias + `pcmpgtw`);
		## AArch64 NEON `cmhi`; wasm `i16x8.gt_u`.
		gt_lanes : U16x8, U16x8 -> U16x8
		gt_lanes = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| if (x > y) U64.highest else 0))

		## Compare lane-wise: each result lane is 65535 where a's lane is
		## less than b's (unsigned), else 0. See [U16x8.gt_lanes] for the
		## per-target lowerings.
		lt_lanes : U16x8, U16x8 -> U16x8
		lt_lanes = |a, b| U16x8.gt_lanes(b, a)

		## Compare lane-wise: each result lane is 65535 where a's lane is
		## greater than or equal to b's (unsigned), else 0.
		##
		## Lowers to `pmaxuw` + `pcmpeqw` on x86-64, `cmhs` on AArch64
		## NEON, and `i16x8.ge_u` on wasm.
		gte_lanes : U16x8, U16x8 -> U16x8
		gte_lanes = |a, b| U16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| if (x >= y) U64.highest else 0))

		## Compare lane-wise: each result lane is 65535 where a's lane is
		## less than or equal to b's (unsigned), else 0. See
		## [U16x8.gte_lanes] for the per-target lowerings.
		lte_lanes : U16x8, U16x8 -> U16x8
		lte_lanes = |a, b| U16x8.gte_lanes(b, a)

		## One bit per lane (lane 0 in bit 0): 1 where the lane's most
		## significant bit is set. On the all-0/all-1 masks produced by the
		## `_lanes` comparisons, this packs the comparison results into a
		## [U8] for scalar decision-making (find-first-match scans, etc.).
		##
		## Lowers to a `packsswb` + `pmovmskb` sequence on x86-64, a short
		## narrowing sequence on AArch64 NEON (no single instruction), and
		## `i16x8.bitmask` on wasm.
		## ```roc
		## expect U16x8.splat(65535).to_bitmask() == 255
		## ```
		to_bitmask : U16x8 -> U8
		to_bitmask = |vector| simd128_bitmask(vector.to_u128_bits(), 16).to_u8_wrap()

		## Returns `Bool.True` if any lane's most significant bit is set.
		## On comparison masks: "did any lane match?"
		any_lanes_set : U16x8 -> Bool
		any_lanes_set = |vector| U16x8.to_bitmask(vector) != 0

		## Returns `Bool.True` if every lane's most significant bit is set.
		## On comparison masks: "did all lanes match?"
		all_lanes_set : U16x8 -> Bool
		all_lanes_set = |vector| U16x8.to_bitmask(vector) == 255

		## Shift every lane's bits left by the same count. The count is
		## taken modulo 16: shifting by 16 leaves every lane unchanged and
		## shifting by 17 shifts by 1, matching [U16.shl_wrap].
		##
		## Lowers to `psllw` on x86-64 with the count masked to the lane
		## width first, `shl` on AArch64 NEON taking the pre-masked count,
		## and `i16x8.shl` on wasm, which masks the count natively.
		shl_wrap : U16x8, U8 -> U16x8
		shl_wrap = |vector, count| U16x8.from_u128_bits(simd128_shift_left(vector.to_u128_bits(), 16, count))

		## Shift every lane's bits right by the same count, filling with
		## zeros. The count is taken modulo 16. For unsigned lanes this
		## behaves the same as [U16x8.shr_zf_wrap].
		shr_wrap : U16x8, U8 -> U16x8
		shr_wrap = |vector, count| U16x8.shr_zf_wrap(vector, count)

		## Shift every lane's bits right by the same count, filling the
		## vacated high bits with zeros. The count is taken modulo 16:
		## shifting by 16 leaves every lane unchanged and shifting by 17
		## shifts by 1, matching [U16.shr_zf_wrap].
		##
		## Lowers to `psrlw` on x86-64 with the count masked to the lane
		## width first, `ushr` on AArch64 NEON taking the pre-masked count,
		## and `i16x8.shr_u` on wasm, which masks the count natively.
		shr_zf_wrap : U16x8, U8 -> U16x8
		shr_zf_wrap = |vector, count| U16x8.from_u128_bits(simd128_shift_right_zf(vector.to_u128_bits(), 16, count))

		## The value of the lane at the given index. Crashes if the index
		## is 8 or greater. (Lane indices are expected to be compile-time
		## constants in practice.)
		##
		## Lowers to `pextrw` on x86-64, `umov` on AArch64 NEON, and
		## `i16x8.extract_lane_u` on wasm.
		get_lane : U16x8, U64 -> U16
		get_lane = |vector, index|
			if index >= 8 {
				crash "U16x8.get_lane: lane index out of range"
			} else {
				simd128_get_lane(vector.to_u128_bits(), 16, index).to_u16_wrap()
			}

		## Returns the vector with the lane at the given index replaced by
		## the given value. Crashes if the index is 8 or greater.
		##
		## Lowers to `pinsrw` on x86-64, `ins` on AArch64 NEON, and
		## `i16x8.replace_lane` on wasm.
		with_lane : U16x8, U64, U16 -> U16x8
		with_lane = |vector, index, value|
			if index >= 8 {
				crash "U16x8.with_lane: lane index out of range"
			} else {
				U16x8.from_u128_bits(simd128_with_lane(vector.to_u128_bits(), 16, index, value.to_u64()))
			}

		## Returns a vector with every lane set to the lane of this vector
		## at the given index. Crashes if the index is 8 or greater.
		##
		## Lowers to `pshufb` with a constant pattern on x86-64, `dup`
		## (lane form) on AArch64 NEON, and a constant `i8x16.shuffle` on
		## wasm.
		broadcast_lane : U16x8, U64 -> U16x8
		broadcast_lane = |vector, index| U16x8.splat(U16x8.get_lane(vector, index))

		## Interleave the low 4 lanes of the two vectors: result lanes are
		## `a0, b0, a1, b1, ...` up through `a3, b3`. With
		## [U16x8.interleave_hi], this is the building block of matrix
		## transposes and of widening lane data.
		##
		## Lowers to `punpcklwd` on x86-64, `zip1` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		interleave_lo : U16x8, U16x8 -> U16x8
		interleave_lo = |a, b| U16x8.from_u128_bits(simd128_interleave_lo(a.to_u128_bits(), b.to_u128_bits(), 16))

		## Interleave the high 4 lanes of the two vectors: result lanes are
		## `a4, b4, a5, b5, ...` up through `a7, b7`.
		##
		## Lowers to `punpckhwd` on x86-64, `zip2` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		interleave_hi : U16x8, U16x8 -> U16x8
		interleave_hi = |a, b| U16x8.from_u128_bits(simd128_interleave_hi(a.to_u128_bits(), b.to_u128_bits(), 16))

		## The even-indexed lanes of a followed by the even-indexed lanes
		## of b — the deinterleaving inverse of the interleave operations,
		## used to split interleaved channel data apart.
		##
		## Lowers to `pshufb`-based shuffles on x86-64, `uzp1` on AArch64
		## NEON, and a constant `i8x16.shuffle` on wasm.
		even_lanes : U16x8, U16x8 -> U16x8
		even_lanes = |a, b| U16x8.from_u128_bits(simd128_even_lanes(a.to_u128_bits(), b.to_u128_bits(), 16))

		## The odd-indexed lanes of a followed by the odd-indexed lanes of
		## b.
		##
		## Lowers to `pshufb`-based shuffles on x86-64, `uzp2` on AArch64
		## NEON, and a constant `i8x16.shuffle` on wasm.
		odd_lanes : U16x8, U16x8 -> U16x8
		odd_lanes = |a, b| U16x8.from_u128_bits(simd128_odd_lanes(a.to_u128_bits(), b.to_u128_bits(), 16))

		## Returns the vector with its 8 lanes in reverse order.
		##
		## Lowers to `pshufb` with a constant pattern on x86-64, `rev64` +
		## `ext` on AArch64 NEON, and a constant `i8x16.shuffle` on wasm.
		reverse_lanes : U16x8 -> U16x8
		reverse_lanes = |vector| U16x8.from_u128_bits(simd128_reverse_lanes(vector.to_u128_bits(), 16))

		## Zero-extend the low 4 lanes into the 4 32-bit lanes of a
		## [U32x4]. With [U16x8.to_u32x4_hi], this is the widening step of
		## the widen-compute-narrow pattern most codec kernels use.
		##
		## Lowers to `pmovzxwd` (SSE4.1) on x86-64, `uxtl` on AArch64 NEON,
		## and `i32x4.extend_low_i16x8_u` on wasm.
		to_u32x4_lo : U16x8 -> U32x4
		to_u32x4_lo = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				$out = simd128_with_lane($out, 32, $i, simd128_get_lane(bits, 16, $i))
				$i = $i + 1
			}
			U32x4.from_u128_bits($out)
		}

		## Zero-extend the high 4 lanes into the 4 32-bit lanes of a
		## [U32x4].
		##
		## Lowers to `punpckhwd` with zero on x86-64, `uxtl2` on AArch64
		## NEON, and `i32x4.extend_high_i16x8_u` on wasm.
		to_u32x4_hi : U16x8 -> U32x4
		to_u32x4_hi = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				$out = simd128_with_lane($out, 32, $i, simd128_get_lane(bits, 16, 4 + $i))
				$i = $i + 1
			}
			U32x4.from_u128_bits($out)
		}

		## Add adjacent pairs of lanes into the 4 32-bit lanes of a
		## [U32x4]: result lane i is `lane(2i) + lane(2i+1)`. Used by
		## checksum and histogram accumulation.
		##
		## Lowers to an emulated sequence on x86-64 (no unsigned
		## pairwise-add instruction), `uaddlp` on AArch64 NEON, and
		## `i32x4.extadd_pairwise_i16x8_u` on wasm.
		pairwise_plus_to_u32x4 : U16x8 -> U32x4
		pairwise_plus_to_u32x4 = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				sum = simd128_get_lane(bits, 16, $i * 2) + simd128_get_lane(bits, 16, $i * 2 + 1)
				$out = simd128_with_lane($out, 32, $i, sum)
				$i = $i + 1
			}
			U32x4.from_u128_bits($out)
		}

		## Multiply the low 4 lanes of the two vectors pairwise into the 4
		## 32-bit lanes of a [U32x4] (no overflow is possible).
		##
		## Lowers to a `pmullw` + `pmulhuw` + `punpcklwd` sequence on
		## x86-64 (no single instruction), `umull` on AArch64 NEON, and
		## `i32x4.extmul_low_i16x8_u` on wasm.
		times_wide_lo : U16x8, U16x8 -> U32x4
		times_wide_lo = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				product = simd128_get_lane(a_bits, 16, $i) * simd128_get_lane(b_bits, 16, $i)
				$out = simd128_with_lane($out, 32, $i, product)
				$i = $i + 1
			}
			U32x4.from_u128_bits($out)
		}

		## Multiply the high 4 lanes of the two vectors pairwise into the 4
		## 32-bit lanes of a [U32x4].
		##
		## Lowers to a `pmullw` + `pmulhuw` + `punpckhwd` sequence on
		## x86-64, `umull2` on AArch64 NEON, and `i32x4.extmul_high_i16x8_u`
		## on wasm.
		times_wide_hi : U16x8, U16x8 -> U32x4
		times_wide_hi = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				product = simd128_get_lane(a_bits, 16, 4 + $i) * simd128_get_lane(b_bits, 16, 4 + $i)
				$out = simd128_with_lane($out, 32, $i, product)
				$i = $i + 1
			}
			U32x4.from_u128_bits($out)
		}

		## Truncate each 16-bit lane of the two vectors to its low 8 bits
		## and pack them into a [U8x16]: result lanes 0-7 come from `a`,
		## lanes 8-15 from `b`.
		##
		## Lowers to a `pand` + `packuswb` sequence on x86-64, `xtn` +
		## `xtn2` on AArch64 NEON, and an emulated mask + narrow sequence on
		## wasm.
		narrow_to_u8x16_wrap : U16x8, U16x8 -> U8x16
		narrow_to_u8x16_wrap = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				$out = simd128_with_lane($out, 8, $i, simd128_get_lane(a_bits, 16, $i))
				$out = simd128_with_lane($out, 8, 8 + $i, simd128_get_lane(b_bits, 16, $i))
				$i = $i + 1
			}
			U8x16.from_u128_bits($out)
		}

		## Clamp each 16-bit lane of the two vectors to at most 255 and
		## pack the results into a [U8x16]: result lanes 0-7 come from `a`,
		## lanes 8-15 from `b`.
		##
		## Lowers to a `pminuw` + `packuswb` sequence on x86-64 (no direct
		## unsigned-source pack), `uqxtn` + `uqxtn2` on AArch64 NEON, and an
		## emulated sequence on wasm.
		narrow_to_u8x16_saturated : U16x8, U16x8 -> U8x16
		narrow_to_u8x16_saturated = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				$out = simd128_with_lane($out, 8, $i, U64.min(simd128_get_lane(a_bits, 16, $i), 255))
				$out = simd128_with_lane($out, 8, 8 + $i, U64.min(simd128_get_lane(b_bits, 16, $i), 255))
				$i = $i + 1
			}
			U8x16.from_u128_bits($out)
		}

		## The sum of all 8 lanes (at most 524280, so it always fits).
		##
		## Lowers to an emulated sequence on x86-64, `uaddlv` on AArch64
		## NEON, and pairwise-add chains on wasm.
		sum_lanes : U16x8 -> U32
		sum_lanes = |vector| {
			bits = vector.to_u128_bits()
			var $sum = 0.U64
			var $i = 0.U64
			while $i < 8 {
				$sum = $sum + simd128_get_lane(bits, 16, $i)
				$i = $i + 1
			}
			$sum.to_u32_wrap()
		}

		## Read 16 bytes starting at the given byte index as 8 little-endian
		## 16-bit lanes. Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`. Any alignment is fine.
		##
		## Lowers to `movdqu` on x86-64, `ldr` (Q register) on AArch64,
		## and `v128.load` on wasm.
		load : List(U8), U64 -> Try(U16x8, [OutOfBounds, ..])
		load = |bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(U16x8.from_u128_bits(simd128_from_bytes_at(bytes, index)))
			}
		}

		## Write these 8 lanes as 16 bytes into the list starting at the
		## given byte index (in place when the list is unique),
		## little-endian. Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`.
		##
		## Lowers to `movdqu` (store form) on x86-64, `str` (Q register)
		## on AArch64, and `v128.store` on wasm.
		store : U16x8, List(U8), U64 -> Try(List(U8), [OutOfBounds, ..])
		store = |vector, bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(simd128_write_bytes_at(vector.to_u128_bits(), bytes, index))
			}
		}

		## Append these 8 lanes as 16 bytes (little-endian) to the end of
		## the list.
		append_to : U16x8, List(U8) -> List(U8)
		append_to = |vector, bytes| simd128_append_bytes(vector.to_u128_bits(), bytes)
	}

	## A 128-bit SIMD vector of 8 signed 16-bit lanes (two's complement).
	##
	## Lane `i` occupies bits `[i * 16, (i + 1) * 16)` of the vector, and
	## the byte-serialized form (used by [I16x8.load] and [I16x8.store]) is
	## little-endian: lane 0 first, each lane's two bytes least-significant
	## first. Every operation has one pinned meaning that is bit-identical
	## on every target; the compiler lowers each operation to the best
	## instruction sequence for the target CPU (SSE/AVX on x86-64, NEON on
	## AArch64, simd128 on wasm).
	I16x8 :: { bits : U128 }.{

		## Returns the [I16x8] with every lane `0`.
		## ```roc
		## expect I16x8.default() == I16x8.splat(0)
		## ```
		default : () -> I16x8
		default = || I16x8.splat(0)

		## Returns an [I16x8] with every lane set to the given [I16].
		##
		## Lowers to `vpbroadcastw` on x86-64, `dup` on AArch64 NEON, and
		## `i16x8.splat` on wasm.
		## ```roc
		## expect I16x8.splat(-7).get_lane(7) == -7
		## ```
		splat : I16 -> I16x8
		splat = |value| I16x8.from_u128_bits(simd128_splat(value.to_u64_wrap(), 16))

		## Build an [I16x8] from exactly 8 lane values, lane 0 first.
		## Returns `Err(WrongLength)` if the list's length is not 8.
		from_list : List(I16) -> Try(I16x8, [WrongLength, ..])
		from_list = |lanes|
			if List.len(lanes) != 8 {
				Err(WrongLength)
			} else {
				var $bits = 0.U128
				var $i = 0.U64
				while $i < 8 {
					$bits = simd128_with_lane($bits, 16, $i, list_get_unsafe(lanes, $i).to_u64_wrap())
					$i = $i + 1
				}
				Ok(I16x8.from_u128_bits($bits))
			}

		## The 8 lane values as a list, lane 0 first.
		## ```roc
		## expect I16x8.splat(9).to_list() == List.repeat(9.I16, 8)
		## ```
		to_list : I16x8 -> List(I16)
		to_list = |vector| {
			var $out = List.with_capacity(8)
			var $i = 0.U64
			while $i < 8 {
				$out = list_append_unsafe($out, I16x8.get_lane(vector, $i))
				$i = $i + 1
			}
			$out
		}

		## Returns `Bool.True` if all 128 bits of the two vectors are equal.
		## (For a per-lane comparison producing a mask, see [I16x8.eq_lanes].)
		##
		## Lowers to `pxor` + `ptest` on x86-64, `cmeq` + `uminv` on AArch64
		## NEON, and `v128.xor` + `v128.any_true` on wasm.
		is_eq : I16x8, I16x8 -> Bool
		is_eq = |a, b| a.to_u128_bits() == b.to_u128_bits()

		## Feed an [I16x8] into a [Hasher].
		to_hash : I16x8, Hasher -> Hasher
		to_hash = |vector, hasher| Hasher.write_u128(hasher, vector.to_u128_bits())

		## Render the lanes for debugging, e.g. `I16x8(1, 2, 3, ...)`.
		to_inspect : I16x8 -> Str
		to_inspect = |vector| simd128_inspect("I16x8", List.map(I16x8.to_list(vector), I16.to_str))

		## The vector's 128 bits as a [U128]. Lane `i` occupies bits
		## `[i * 16, (i + 1) * 16)`. Free at runtime — no instructions.
		to_u128_bits : I16x8 -> U128
		to_u128_bits = |vector| vector.bits

		## Build an [I16x8] from 128 raw bits. Free at runtime — no
		## instructions.
		from_u128_bits : U128 -> I16x8
		from_u128_bits = |bits| I16x8.{ bits }

		## Reinterpret the same 128 bits as a [U8x16]. Free at runtime.
		to_u8x16_bits : I16x8 -> U8x16
		to_u8x16_bits = |vector| U8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I8x16]. Free at runtime.
		to_i8x16_bits : I16x8 -> I8x16
		to_i8x16_bits = |vector| I8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U16x8]. Free at runtime.
		to_u16x8_bits : I16x8 -> U16x8
		to_u16x8_bits = |vector| U16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U32x4]. Free at runtime.
		to_u32x4_bits : I16x8 -> U32x4
		to_u32x4_bits = |vector| U32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I32x4]. Free at runtime.
		to_i32x4_bits : I16x8 -> I32x4
		to_i32x4_bits = |vector| I32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U64x2]. Free at runtime.
		to_u64x2_bits : I16x8 -> U64x2
		to_u64x2_bits = |vector| U64x2.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I64x2]. Free at runtime.
		to_i64x2_bits : I16x8 -> I64x2
		to_i64x2_bits = |vector| I64x2.from_u128_bits(vector.to_u128_bits())

		## Add lane-wise, each lane wrapping around mod 65536 (two's
		## complement).
		##
		## Lowers to `paddw` on x86-64, `add` (8×16-bit) on AArch64 NEON,
		## and `i16x8.add` on wasm.
		plus_wrap : I16x8, I16x8 -> I16x8
		plus_wrap = |a, b| I16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| x + y))

		## Subtract lane-wise, each lane wrapping around mod 65536 (two's
		## complement).
		##
		## Lowers to `psubw` on x86-64, `sub` (8×16-bit) on AArch64 NEON,
		## and `i16x8.sub` on wasm.
		minus_wrap : I16x8, I16x8 -> I16x8
		minus_wrap = |a, b| I16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| (x + 65536) - y))

		## Add lane-wise, each lane saturating at 32767 or -32768 instead
		## of wrapping.
		##
		## Lowers to `paddsw` on x86-64, `sqadd` on AArch64 NEON, and
		## `i16x8.add_sat_s` on wasm.
		## ```roc
		## expect I16x8.splat(30000).plus_saturated(I16x8.splat(30000)).get_lane(0) == 32767
		## ```
		plus_saturated : I16x8, I16x8 -> I16x8
		plus_saturated = |a, b| I16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| simd128_clamp_signed(simd128_lane_to_signed(x, 16) + simd128_lane_to_signed(y, 16), 16)))

		## Subtract lane-wise, each lane saturating at 32767 or -32768
		## instead of wrapping.
		##
		## Lowers to `psubsw` on x86-64, `sqsub` on AArch64 NEON, and
		## `i16x8.sub_sat_s` on wasm.
		minus_saturated : I16x8, I16x8 -> I16x8
		minus_saturated = |a, b| I16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| simd128_clamp_signed(simd128_lane_to_signed(x, 16) - simd128_lane_to_signed(y, 16), 16)))

		## Negate each lane, wrapping around mod 65536. `-32768` negates to
		## itself, since `32768` does not fit in a signed 16-bit lane.
		##
		## Lowers to `psubw` from zero on x86-64, `neg` on AArch64 NEON,
		## and `i16x8.neg` on wasm.
		## ```roc
		## expect I16x8.splat(5).negate_wrap().get_lane(0) == -5
		## ```
		negate_wrap : I16x8 -> I16x8
		negate_wrap = |vector| I16x8.from_u128_bits(simd128_map1(vector.to_u128_bits(), 16, |x| 65536 - x))

		## The absolute value of each lane, wrapping around mod 65536.
		## `-32768` maps to itself, since `32768` does not fit in a signed
		## 16-bit lane.
		##
		## Lowers to `pabsw` on x86-64, `abs` on AArch64 NEON, and
		## `i16x8.abs` on wasm.
		## ```roc
		## expect I16x8.splat(-5).abs_wrap().get_lane(0) == 5
		## ```
		abs_wrap : I16x8 -> I16x8
		abs_wrap = |vector| I16x8.from_u128_bits(simd128_map1(vector.to_u128_bits(), 16, |x| if (simd128_lane_to_signed(x, 16) < 0) (65536 - x) else x))

		## The smaller of each pair of lanes (signed).
		##
		## Lowers to `pminsw` on x86-64, `smin` on AArch64 NEON, and
		## `i16x8.min_s` on wasm.
		min : I16x8, I16x8 -> I16x8
		min = |a, b| I16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| I64.min(simd128_lane_to_signed(x, 16), simd128_lane_to_signed(y, 16)).to_u64_wrap()))

		## The larger of each pair of lanes (signed).
		##
		## Lowers to `pmaxsw` on x86-64, `smax` on AArch64 NEON, and
		## `i16x8.max_s` on wasm.
		max : I16x8, I16x8 -> I16x8
		max = |a, b| I16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| I64.max(simd128_lane_to_signed(x, 16), simd128_lane_to_signed(y, 16)).to_u64_wrap()))

		## Multiply lane-wise, each lane wrapping around mod 65536 (the low
		## 16 bits of the product are the same whether the lanes are read as
		## signed or unsigned).
		##
		## Lowers to `pmullw` on x86-64, `mul` on AArch64 NEON, and
		## `i16x8.mul` on wasm.
		times_wrap : I16x8, I16x8 -> I16x8
		times_wrap = |a, b| I16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| x * y))

		## The high 16 bits of each lane-wise signed product:
		## `(a * b) >> 16` with a sign-preserving (arithmetic) shift.
		##
		## Lowers to `pmulhw` on x86-64, a `smull` + `shrn` sequence on
		## AArch64 NEON (no single instruction), and an emulated sequence on
		## wasm (no single instruction).
		times_high : I16x8, I16x8 -> I16x8
		times_high = |a, b| I16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| I64.shr_wrap(simd128_lane_to_signed(x, 16) * simd128_lane_to_signed(y, 16), 16).to_u64_wrap()))

		## Fixed-point Q15 multiply with rounding and saturation: each lane
		## is `saturate((2 * a * b + 32768) >> 16)`, treating the lanes as
		## signed Q15 fractions. `(-32768, -32768)` saturates to `32767`,
		## since its exact product `+1.0` is not representable. This is the
		## fixed-point workhorse of color conversion and DCT kernels.
		##
		## Lowers to `pmulhrsw` plus a fixup for the `(-32768, -32768)`
		## input on x86-64, `sqrdmulh` on AArch64 NEON, and
		## `i16x8.q15mulr_sat_s` on wasm.
		## ```roc
		## expect I16x8.splat(-32768).times_fixed_q15_saturated(I16x8.splat(-32768)).get_lane(0) == 32767
		## ```
		times_fixed_q15_saturated : I16x8, I16x8 -> I16x8
		times_fixed_q15_saturated = |a, b| I16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| simd128_clamp_signed(I64.shr_wrap(2 * simd128_lane_to_signed(x, 16) * simd128_lane_to_signed(y, 16) + 32768, 16), 16)))

		## Multiply lanes pairwise and sum adjacent products into the 4
		## 32-bit lanes of an [I32x4]: result lane i is
		## `a(2i) * b(2i) + a(2i+1) * b(2i+1)`. The only input that wraps
		## the 32-bit result is all four lanes `-32768`, matching the
		## hardware. This is the DCT/IDCT/FIR workhorse — multiply-accumulate
		## over signed 16-bit taps.
		##
		## Lowers to `pmaddwd` on x86-64, a `smull` + `smull2` +
		## pairwise-add sequence on AArch64 NEON, and `i32x4.dot_i16x8_s`
		## on wasm.
		dot_pairs : I16x8, I16x8 -> I32x4
		dot_pairs = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				s0 = simd128_lane_to_signed(simd128_get_lane(a_bits, 16, $i * 2), 16)
				s1 = simd128_lane_to_signed(simd128_get_lane(a_bits, 16, $i * 2 + 1), 16)
				t0 = simd128_lane_to_signed(simd128_get_lane(b_bits, 16, $i * 2), 16)
				t1 = simd128_lane_to_signed(simd128_get_lane(b_bits, 16, $i * 2 + 1), 16)
				$out = simd128_with_lane($out, 32, $i, (s0 * t0 + s1 * t1).to_u64_wrap())
				$i = $i + 1
			}
			I32x4.from_u128_bits($out)
		}

		## Returns the bitwise AND of the two vectors' 128 bits.
		##
		## Lowers to `pand` on x86-64, `and` on AArch64 NEON, and
		## `v128.and` on wasm.
		bitwise_and : I16x8, I16x8 -> I16x8
		bitwise_and = |a, b| I16x8.from_u128_bits(U128.bitwise_and(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise OR of the two vectors' 128 bits.
		##
		## Lowers to `por` on x86-64, `orr` on AArch64 NEON, and `v128.or`
		## on wasm.
		bitwise_or : I16x8, I16x8 -> I16x8
		bitwise_or = |a, b| I16x8.from_u128_bits(U128.bitwise_or(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise XOR of the two vectors' 128 bits.
		##
		## Lowers to `pxor` on x86-64, `eor` on AArch64 NEON, and
		## `v128.xor` on wasm.
		bitwise_xor : I16x8, I16x8 -> I16x8
		bitwise_xor = |a, b| I16x8.from_u128_bits(U128.bitwise_xor(a.to_u128_bits(), b.to_u128_bits()))

		## Flips every one of the vector's 128 bits.
		##
		## Lowers to `pxor` with all-ones on x86-64, `mvn` on AArch64 NEON,
		## and `v128.not` on wasm.
		bitwise_not : I16x8 -> I16x8
		bitwise_not = |vector| I16x8.from_u128_bits(U128.bitwise_not(vector.to_u128_bits()))

		## Bitwise select: for each of the 128 bits, take the bit from
		## `if_set` where this mask vector has a 1, and from `if_clear`
		## where it has a 0. Combined with the `_lanes` comparisons, this
		## is the branchless lane-wise `if`.
		##
		## Lowers to `pand`/`pandn`/`por` on x86-64, `bsl` on AArch64 NEON,
		## and `v128.bitselect` on wasm.
		bit_select : I16x8, I16x8, I16x8 -> I16x8
		bit_select = |mask, if_set, if_clear| {
			mask_bits = mask.to_u128_bits()
			kept = U128.bitwise_and(mask_bits, if_set.to_u128_bits())
			cleared = U128.bitwise_and(U128.bitwise_not(mask_bits), if_clear.to_u128_bits())
			I16x8.from_u128_bits(U128.bitwise_or(kept, cleared))
		}

		## Compare lane-wise for equality: each result lane is all-ones
		## (`-1`) where the lanes are equal and 0 where they differ.
		##
		## Lowers to `pcmpeqw` on x86-64, `cmeq` on AArch64 NEON, and
		## `i16x8.eq` on wasm.
		eq_lanes : I16x8, I16x8 -> I16x8
		eq_lanes = |a, b| I16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| if (x == y) U64.highest else 0))

		## Compare lane-wise: each result lane is all-ones (`-1`) where a's
		## lane is greater than b's (signed), else 0.
		##
		## Lowers to `pcmpgtw` on x86-64, `cmgt` on AArch64 NEON, and
		## `i16x8.gt_s` on wasm.
		gt_lanes : I16x8, I16x8 -> I16x8
		gt_lanes = |a, b| I16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| if (simd128_lane_to_signed(x, 16) > simd128_lane_to_signed(y, 16)) U64.highest else 0))

		## Compare lane-wise: each result lane is all-ones (`-1`) where a's
		## lane is less than b's (signed), else 0. See [I16x8.gt_lanes] for
		## the per-target lowerings.
		lt_lanes : I16x8, I16x8 -> I16x8
		lt_lanes = |a, b| I16x8.gt_lanes(b, a)

		## Compare lane-wise: each result lane is all-ones (`-1`) where a's
		## lane is greater than or equal to b's (signed), else 0.
		##
		## Lowers to `pcmpgtw` + `pcmpeqw` + `por` on x86-64, `cmge` on
		## AArch64 NEON, and `i16x8.ge_s` on wasm.
		gte_lanes : I16x8, I16x8 -> I16x8
		gte_lanes = |a, b| I16x8.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 16, |x, y| if (simd128_lane_to_signed(x, 16) >= simd128_lane_to_signed(y, 16)) U64.highest else 0))

		## Compare lane-wise: each result lane is all-ones (`-1`) where a's
		## lane is less than or equal to b's (signed), else 0. See
		## [I16x8.gte_lanes] for the per-target lowerings.
		lte_lanes : I16x8, I16x8 -> I16x8
		lte_lanes = |a, b| I16x8.gte_lanes(b, a)

		## One bit per lane (lane 0 in bit 0): 1 where the lane's most
		## significant (sign) bit is set. On the all-0/all-1 masks produced
		## by the `_lanes` comparisons, this packs the comparison results
		## into a [U8] for scalar decision-making (find-first-match scans,
		## etc.).
		##
		## Lowers to a `packsswb` + `pmovmskb` sequence on x86-64, a short
		## narrowing sequence on AArch64 NEON (no single instruction), and
		## `i16x8.bitmask` on wasm.
		to_bitmask : I16x8 -> U8
		to_bitmask = |vector| simd128_bitmask(vector.to_u128_bits(), 16).to_u8_wrap()

		## Returns `Bool.True` if any lane's sign bit is set (any lane is
		## negative). On comparison masks: "did any lane match?"
		any_lanes_set : I16x8 -> Bool
		any_lanes_set = |vector| I16x8.to_bitmask(vector) != 0

		## Returns `Bool.True` if every lane's sign bit is set (every lane
		## is negative). On comparison masks: "did all lanes match?"
		all_lanes_set : I16x8 -> Bool
		all_lanes_set = |vector| I16x8.to_bitmask(vector) == 255

		## Shift every lane's bits left by the same count. The count is
		## taken modulo 16: shifting by 16 leaves every lane unchanged and
		## shifting by 17 shifts by 1, matching [I16.shl_wrap].
		##
		## Lowers to `psllw` on x86-64 with the count masked to the lane
		## width first, `shl` on AArch64 NEON taking the pre-masked count,
		## and `i16x8.shl` on wasm, which masks the count natively.
		shl_wrap : I16x8, U8 -> I16x8
		shl_wrap = |vector, count| I16x8.from_u128_bits(simd128_shift_left(vector.to_u128_bits(), 16, count))

		## Shift every lane's bits right by the same count, preserving the
		## sign ("arithmetic shift"). The count is taken modulo 16: shifting
		## by 16 leaves every lane unchanged and shifting by 17 shifts by 1,
		## matching [I16.shr_wrap].
		##
		## Lowers to `psraw` on x86-64 with the count masked to the lane
		## width first, `sshr` on AArch64 NEON taking the pre-masked count,
		## and `i16x8.shr_s` on wasm, which masks the count natively.
		shr_wrap : I16x8, U8 -> I16x8
		shr_wrap = |vector, count| I16x8.from_u128_bits(simd128_shift_right_arith(vector.to_u128_bits(), 16, count))

		## Shift every lane's bits right by the same count, filling the
		## vacated high bits with zeros ("zero-fill"). The count is taken
		## modulo 16: shifting by 16 leaves every lane unchanged and shifting
		## by 17 shifts by 1, matching [I16.shr_zf_wrap].
		##
		## Lowers to `psrlw` on x86-64 with the count masked to the lane
		## width first, `ushr` on AArch64 NEON taking the pre-masked count,
		## and `i16x8.shr_u` on wasm, which masks the count natively.
		shr_zf_wrap : I16x8, U8 -> I16x8
		shr_zf_wrap = |vector, count| I16x8.from_u128_bits(simd128_shift_right_zf(vector.to_u128_bits(), 16, count))

		## Shift every lane right by the same count, rounding to nearest
		## (round half up): each lane becomes
		## `(a + (1 << (count - 1))) >> count` with a sign-preserving shift.
		## A count of 0 returns the vector unchanged; counts of 16 or more
		## produce 0 in every lane. This is the rounding-shift idiom of
		## every transform stage.
		##
		## Lowers to a `paddw` + `psraw` sequence on x86-64 (no single
		## instruction), `srshr` on AArch64 NEON, and an emulated sequence
		## on wasm (no single instruction).
		## ```roc
		## expect I16x8.splat(5).shift_right_rounded_by(1).get_lane(0) == 3
		## ```
		shift_right_rounded_by : I16x8, U8 -> I16x8
		shift_right_rounded_by = |vector, count|
			if count == 0 {
				vector
			} else if count >= 16 {
				I16x8.splat(0)
			} else {
				bias = I64.shl_wrap(1, count - 1)
				I16x8.from_u128_bits(simd128_map1(vector.to_u128_bits(), 16, |x| I64.shr_wrap(simd128_lane_to_signed(x, 16) + bias, count).to_u64_wrap()))
			}

		## Clamp each 16-bit lane of the two vectors to the signed 8-bit
		## range `-128` to `127` and pack the results into an [I8x16]:
		## result lanes 0-7 come from `a`, lanes 8-15 from `b`.
		##
		## Lowers to `packsswb` on x86-64, `sqxtn` + `sqxtn2` on AArch64
		## NEON, and `i8x16.narrow_i16x8_s` on wasm.
		narrow_to_i8x16_saturated : I16x8, I16x8 -> I8x16
		narrow_to_i8x16_saturated = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				$out = simd128_with_lane($out, 8, $i, simd128_clamp_signed(simd128_lane_to_signed(simd128_get_lane(a_bits, 16, $i), 16), 8))
				$out = simd128_with_lane($out, 8, 8 + $i, simd128_clamp_signed(simd128_lane_to_signed(simd128_get_lane(b_bits, 16, $i), 16), 8))
				$i = $i + 1
			}
			I8x16.from_u128_bits($out)
		}

		## Clamp each 16-bit lane of the two vectors to the unsigned 8-bit
		## range `0` to `255` and pack the results into a [U8x16]: result
		## lanes 0-7 come from `a`, lanes 8-15 from `b`. This is the final
		## clamp-to-pixel step of image reconstruction.
		##
		## Lowers to `packuswb` on x86-64, `sqxtun` + `sqxtun2` on AArch64
		## NEON, and `i8x16.narrow_i16x8_u` on wasm.
		narrow_to_u8x16_saturated : I16x8, I16x8 -> U8x16
		narrow_to_u8x16_saturated = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 8 {
				$out = simd128_with_lane($out, 8, $i, simd128_clamp_to_unsigned(simd128_lane_to_signed(simd128_get_lane(a_bits, 16, $i), 16), 8))
				$out = simd128_with_lane($out, 8, 8 + $i, simd128_clamp_to_unsigned(simd128_lane_to_signed(simd128_get_lane(b_bits, 16, $i), 16), 8))
				$i = $i + 1
			}
			U8x16.from_u128_bits($out)
		}

		## Sign-extend the low 4 lanes into the 4 32-bit lanes of an
		## [I32x4]. With [I16x8.to_i32x4_hi], this is the widening step of
		## the widen-compute-narrow pattern most codec kernels use.
		##
		## Lowers to `pmovsxwd` on x86-64, `sxtl` on AArch64 NEON, and
		## `i32x4.extend_low_i16x8_s` on wasm.
		to_i32x4_lo : I16x8 -> I32x4
		to_i32x4_lo = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				$out = simd128_with_lane($out, 32, $i, simd128_lane_to_signed(simd128_get_lane(bits, 16, $i), 16).to_u64_wrap())
				$i = $i + 1
			}
			I32x4.from_u128_bits($out)
		}

		## Sign-extend the high 4 lanes into the 4 32-bit lanes of an
		## [I32x4].
		##
		## Lowers to a `psrldq` + `pmovsxwd` sequence on x86-64, `sxtl2` on
		## AArch64 NEON, and `i32x4.extend_high_i16x8_s` on wasm.
		to_i32x4_hi : I16x8 -> I32x4
		to_i32x4_hi = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				$out = simd128_with_lane($out, 32, $i, simd128_lane_to_signed(simd128_get_lane(bits, 16, 4 + $i), 16).to_u64_wrap())
				$i = $i + 1
			}
			I32x4.from_u128_bits($out)
		}

		## Add adjacent pairs of lanes into the 4 32-bit lanes of an
		## [I32x4]: result lane i is `a(2i) + a(2i+1)` (signed). Used by
		## transform column sums and histogram accumulation.
		##
		## Lowers to `pmaddwd` with a ones vector on x86-64, `saddlp` on
		## AArch64 NEON, and `i32x4.extadd_pairwise_i16x8_s` on wasm.
		pairwise_plus_to_i32x4 : I16x8 -> I32x4
		pairwise_plus_to_i32x4 = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				sum = simd128_lane_to_signed(simd128_get_lane(bits, 16, $i * 2), 16) + simd128_lane_to_signed(simd128_get_lane(bits, 16, $i * 2 + 1), 16)
				$out = simd128_with_lane($out, 32, $i, sum.to_u64_wrap())
				$i = $i + 1
			}
			I32x4.from_u128_bits($out)
		}

		## Multiply the low 4 lanes of the two vectors pairwise into the 4
		## 32-bit lanes of an [I32x4] (no overflow is possible).
		##
		## Lowers to a `pmullw` + `pmulhw` + `punpcklwd` sequence on x86-64
		## (no single instruction), `smull` on AArch64 NEON, and
		## `i32x4.extmul_low_i16x8_s` on wasm.
		times_wide_lo : I16x8, I16x8 -> I32x4
		times_wide_lo = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				product = simd128_lane_to_signed(simd128_get_lane(a_bits, 16, $i), 16) * simd128_lane_to_signed(simd128_get_lane(b_bits, 16, $i), 16)
				$out = simd128_with_lane($out, 32, $i, product.to_u64_wrap())
				$i = $i + 1
			}
			I32x4.from_u128_bits($out)
		}

		## Multiply the high 4 lanes of the two vectors pairwise into the 4
		## 32-bit lanes of an [I32x4].
		##
		## Lowers to a `pmullw` + `pmulhw` + `punpckhwd` sequence on
		## x86-64, `smull2` on AArch64 NEON, and `i32x4.extmul_high_i16x8_s`
		## on wasm.
		times_wide_hi : I16x8, I16x8 -> I32x4
		times_wide_hi = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				product = simd128_lane_to_signed(simd128_get_lane(a_bits, 16, 4 + $i), 16) * simd128_lane_to_signed(simd128_get_lane(b_bits, 16, 4 + $i), 16)
				$out = simd128_with_lane($out, 32, $i, product.to_u64_wrap())
				$i = $i + 1
			}
			I32x4.from_u128_bits($out)
		}

		## The sum of all 8 lanes (signed; ranges from -262144 to 262136,
		## so it always fits in an [I32]).
		##
		## Lowers to an emulated sequence on x86-64, `saddlv` on AArch64
		## NEON, and pairwise-add chains on wasm.
		sum_lanes : I16x8 -> I32
		sum_lanes = |vector| {
			bits = vector.to_u128_bits()
			var $sum = 0.I64
			var $i = 0.U64
			while $i < 8 {
				$sum = $sum + simd128_lane_to_signed(simd128_get_lane(bits, 16, $i), 16)
				$i = $i + 1
			}
			$sum.to_i32_wrap()
		}

		## The value of the lane at the given index. Crashes if the index
		## is 8 or greater. (Lane indices are expected to be compile-time
		## constants in practice.)
		##
		## Lowers to `pextrw` on x86-64, `smov` on AArch64 NEON, and
		## `i16x8.extract_lane_s` on wasm.
		get_lane : I16x8, U64 -> I16
		get_lane = |vector, index|
			if index >= 8 {
				crash "I16x8.get_lane: lane index out of range"
			} else {
				simd128_lane_to_signed(simd128_get_lane(vector.to_u128_bits(), 16, index), 16).to_i16_wrap()
			}

		## Returns the vector with the lane at the given index replaced by
		## the given value. Crashes if the index is 8 or greater.
		##
		## Lowers to `pinsrw` on x86-64, `ins` on AArch64 NEON, and
		## `i16x8.replace_lane` on wasm.
		with_lane : I16x8, U64, I16 -> I16x8
		with_lane = |vector, index, value|
			if index >= 8 {
				crash "I16x8.with_lane: lane index out of range"
			} else {
				I16x8.from_u128_bits(simd128_with_lane(vector.to_u128_bits(), 16, index, value.to_u64_wrap()))
			}

		## Returns a vector with every lane set to the lane of this vector
		## at the given index. Crashes if the index is 8 or greater.
		##
		## Lowers to `pshufb` with a constant pattern on x86-64, `dup`
		## (lane form) on AArch64 NEON, and a constant `i8x16.shuffle` on
		## wasm.
		broadcast_lane : I16x8, U64 -> I16x8
		broadcast_lane = |vector, index| I16x8.splat(I16x8.get_lane(vector, index))

		## Interleave the low 4 lanes of the two vectors: result lanes are
		## `a0, b0, a1, b1, ...` up through `a3, b3`. With
		## [I16x8.interleave_hi], this is the building block of matrix
		## transposes and of widening lane data.
		##
		## Lowers to `punpcklwd` on x86-64, `zip1` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		interleave_lo : I16x8, I16x8 -> I16x8
		interleave_lo = |a, b| I16x8.from_u128_bits(simd128_interleave_lo(a.to_u128_bits(), b.to_u128_bits(), 16))

		## Interleave the high 4 lanes of the two vectors: result lanes are
		## `a4, b4, a5, b5, ...` up through `a7, b7`.
		##
		## Lowers to `punpckhwd` on x86-64, `zip2` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		interleave_hi : I16x8, I16x8 -> I16x8
		interleave_hi = |a, b| I16x8.from_u128_bits(simd128_interleave_hi(a.to_u128_bits(), b.to_u128_bits(), 16))

		## The even-indexed lanes of a followed by the even-indexed lanes
		## of b — the deinterleaving inverse of the interleave operations,
		## used to split interleaved channel data apart.
		##
		## Lowers to `pshufb`-based shuffles on x86-64, `uzp1` on AArch64
		## NEON, and a constant `i8x16.shuffle` on wasm.
		even_lanes : I16x8, I16x8 -> I16x8
		even_lanes = |a, b| I16x8.from_u128_bits(simd128_even_lanes(a.to_u128_bits(), b.to_u128_bits(), 16))

		## The odd-indexed lanes of a followed by the odd-indexed lanes of
		## b.
		##
		## Lowers to `pshufb`-based shuffles on x86-64, `uzp2` on AArch64
		## NEON, and a constant `i8x16.shuffle` on wasm.
		odd_lanes : I16x8, I16x8 -> I16x8
		odd_lanes = |a, b| I16x8.from_u128_bits(simd128_odd_lanes(a.to_u128_bits(), b.to_u128_bits(), 16))

		## Returns the vector with its 8 lanes in reverse order.
		##
		## Lowers to `pshufb` with a constant pattern on x86-64, `rev64` +
		## `ext` on AArch64 NEON, and a constant `i8x16.shuffle` on wasm.
		reverse_lanes : I16x8 -> I16x8
		reverse_lanes = |vector| I16x8.from_u128_bits(simd128_reverse_lanes(vector.to_u128_bits(), 16))

		## Read 16 bytes starting at the given byte index as 8 little-endian
		## 16-bit lanes. Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`. Any alignment is fine.
		##
		## Lowers to `movdqu` on x86-64, `ldr` (Q register) on AArch64,
		## and `v128.load` on wasm.
		load : List(U8), U64 -> Try(I16x8, [OutOfBounds, ..])
		load = |bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(I16x8.from_u128_bits(simd128_from_bytes_at(bytes, index)))
			}
		}

		## Write these 8 lanes as 16 bytes into the list starting at the
		## given byte index (in place when the list is unique),
		## little-endian. Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`.
		##
		## Lowers to `movdqu` (store form) on x86-64, `str` (Q register)
		## on AArch64, and `v128.store` on wasm.
		store : I16x8, List(U8), U64 -> Try(List(U8), [OutOfBounds, ..])
		store = |vector, bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(simd128_write_bytes_at(vector.to_u128_bits(), bytes, index))
			}
		}

		## Append these 8 lanes as 16 bytes (little-endian) to the end of
		## the list.
		append_to : I16x8, List(U8) -> List(U8)
		append_to = |vector, bytes| simd128_append_bytes(vector.to_u128_bits(), bytes)
	}

	## A 128-bit SIMD vector of 4 unsigned 32-bit lanes.
	##
	## Lane `i` occupies bits `[i * 32, (i + 1) * 32)` of the vector, and the
	## byte-serialized form (used by [U32x4.load], [U32x4.store], and
	## [U32x4.to_list]) is little-endian with lane 0 first. Every operation
	## has one pinned meaning that is bit-identical on every target; the
	## compiler lowers each operation to the best instruction sequence for
	## the target CPU (SSE/AVX on x86-64, NEON on AArch64, simd128 on wasm).
	U32x4 :: { bits : U128 }.{

		## Returns the [U32x4] with every lane `0`.
		## ```roc
		## expect U32x4.default() == U32x4.splat(0)
		## ```
		default : () -> U32x4
		default = || U32x4.splat(0)

		## Returns a [U32x4] with every lane set to the given [U32].
		##
		## Lowers to a scalar-register move + `pshufd` on x86-64, `dup` on
		## AArch64 NEON, and `i32x4.splat` on wasm.
		## ```roc
		## expect U32x4.splat(7).get_lane(3) == 7
		## ```
		splat : U32 -> U32x4
		splat = |value| U32x4.from_u128_bits(simd128_splat(value.to_u64(), 32))

		## Build a [U32x4] from exactly 4 lane values, lane 0 first.
		## Returns `Err(WrongLength)` if the list's length is not 4.
		from_list : List(U32) -> Try(U32x4, [WrongLength, ..])
		from_list = |lanes|
			if List.len(lanes) != 4 {
				Err(WrongLength)
			} else {
				var $bits = 0.U128
				var $i = 0.U64
				while $i < 4 {
					$bits = simd128_with_lane($bits, 32, $i, list_get_unsafe(lanes, $i).to_u64())
					$i = $i + 1
				}
				Ok(U32x4.from_u128_bits($bits))
			}

		## The 4 lane values as a list, lane 0 first.
		## ```roc
		## expect U32x4.splat(9).to_list() == List.repeat(9.U32, 4)
		## ```
		to_list : U32x4 -> List(U32)
		to_list = |vector| {
			var $out = List.with_capacity(4)
			var $i = 0.U64
			while $i < 4 {
				$out = list_append_unsafe($out, U32x4.get_lane(vector, $i))
				$i = $i + 1
			}
			$out
		}

		## Returns `Bool.True` if all 128 bits of the two vectors are equal.
		## (For a per-lane comparison producing a mask, see [U32x4.eq_lanes].)
		##
		## Lowers to `pxor` + `ptest` on x86-64, `cmeq` + `uminv` on AArch64
		## NEON, and `v128.xor` + `v128.any_true` on wasm.
		is_eq : U32x4, U32x4 -> Bool
		is_eq = |a, b| a.to_u128_bits() == b.to_u128_bits()

		## Feed a [U32x4] into a [Hasher].
		to_hash : U32x4, Hasher -> Hasher
		to_hash = |vector, hasher| Hasher.write_u128(hasher, vector.to_u128_bits())

		## Render the lanes for debugging, e.g. `U32x4(1, 2, 3, 4)`.
		to_inspect : U32x4 -> Str
		to_inspect = |vector| simd128_inspect("U32x4", List.map(U32x4.to_list(vector), U32.to_str))

		## The vector's 128 bits as a [U128]. Lane `i` occupies bits
		## `[i * 32, (i + 1) * 32)`. Free at runtime — no instructions.
		to_u128_bits : U32x4 -> U128
		to_u128_bits = |vector| vector.bits

		## Build a [U32x4] from 128 raw bits. Free at runtime — no
		## instructions.
		from_u128_bits : U128 -> U32x4
		from_u128_bits = |bits| U32x4.{ bits }

		## Reinterpret the same 128 bits as a [U8x16]. Free at runtime.
		to_u8x16_bits : U32x4 -> U8x16
		to_u8x16_bits = |vector| U8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I8x16]. Free at runtime.
		to_i8x16_bits : U32x4 -> I8x16
		to_i8x16_bits = |vector| I8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U16x8]. Free at runtime.
		to_u16x8_bits : U32x4 -> U16x8
		to_u16x8_bits = |vector| U16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I16x8]. Free at runtime.
		to_i16x8_bits : U32x4 -> I16x8
		to_i16x8_bits = |vector| I16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I32x4]. Free at runtime.
		to_i32x4_bits : U32x4 -> I32x4
		to_i32x4_bits = |vector| I32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U64x2]. Free at runtime.
		to_u64x2_bits : U32x4 -> U64x2
		to_u64x2_bits = |vector| U64x2.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I64x2]. Free at runtime.
		to_i64x2_bits : U32x4 -> I64x2
		to_i64x2_bits = |vector| I64x2.from_u128_bits(vector.to_u128_bits())

		## Add lane-wise, each lane wrapping mod 4294967296.
		##
		## Lowers to `paddd` on x86-64, `add` (4×32-bit) on AArch64 NEON,
		## and `i32x4.add` on wasm.
		## ```roc
		## expect U32x4.splat(4294967295).plus_wrap(U32x4.splat(1)).get_lane(0) == 0
		## ```
		plus_wrap : U32x4, U32x4 -> U32x4
		plus_wrap = |a, b| U32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| x + y))

		## Subtract lane-wise, each lane wrapping mod 4294967296.
		##
		## Lowers to `psubd` on x86-64, `sub` (4×32-bit) on AArch64 NEON,
		## and `i32x4.sub` on wasm.
		## ```roc
		## expect U32x4.splat(0).minus_wrap(U32x4.splat(1)).get_lane(0) == 4294967295
		## ```
		minus_wrap : U32x4, U32x4 -> U32x4
		minus_wrap = |a, b| U32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| (x + 4294967296) - y))

		## The smaller of each pair of lanes (unsigned).
		##
		## Lowers to `pminud` (SSE4.1) on x86-64, `umin` on AArch64 NEON,
		## and `i32x4.min_u` on wasm.
		min : U32x4, U32x4 -> U32x4
		min = |a, b| U32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, U64.min))

		## The larger of each pair of lanes (unsigned).
		##
		## Lowers to `pmaxud` (SSE4.1) on x86-64, `umax` on AArch64 NEON,
		## and `i32x4.max_u` on wasm.
		max : U32x4, U32x4 -> U32x4
		max = |a, b| U32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, U64.max))

		## Multiply lane-wise, each lane wrapping mod 4294967296.
		##
		## Lowers to `pmulld` (SSE4.1) on x86-64, `mul` (4×32-bit) on
		## AArch64 NEON, and `i32x4.mul` on wasm.
		## ```roc
		## expect U32x4.splat(65536).times_wrap(U32x4.splat(65536)).get_lane(0) == 0
		## ```
		times_wrap : U32x4, U32x4 -> U32x4
		times_wrap = |a, b| U32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| x * y))

		## Multiply lanes 0 and 1 of the two vectors pairwise into the two
		## 64-bit lanes of a [U64x2] (no overflow is possible).
		##
		## Lowers to a `pmuludq`-based sequence on x86-64, `umull` on AArch64
		## NEON, and `i64x2.extmul_low_i32x4_u` on wasm.
		times_wide_lo : U32x4, U32x4 -> U64x2
		times_wide_lo = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 2 {
				product = simd128_get_lane(a_bits, 32, $i) * simd128_get_lane(b_bits, 32, $i)
				$out = simd128_with_lane($out, 64, $i, product)
				$i = $i + 1
			}
			U64x2.from_u128_bits($out)
		}

		## Multiply lanes 2 and 3 of the two vectors pairwise into the two
		## 64-bit lanes of a [U64x2] (no overflow is possible).
		##
		## Lowers to a `pmuludq`-based sequence on x86-64, `umull2` on
		## AArch64 NEON, and `i64x2.extmul_high_i32x4_u` on wasm.
		times_wide_hi : U32x4, U32x4 -> U64x2
		times_wide_hi = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 2 {
				product = simd128_get_lane(a_bits, 32, 2 + $i) * simd128_get_lane(b_bits, 32, 2 + $i)
				$out = simd128_with_lane($out, 64, $i, product)
				$i = $i + 1
			}
			U64x2.from_u128_bits($out)
		}

		## Returns the bitwise AND of the two vectors' 128 bits.
		##
		## Lowers to `pand` on x86-64, `and` on AArch64 NEON, and
		## `v128.and` on wasm.
		bitwise_and : U32x4, U32x4 -> U32x4
		bitwise_and = |a, b| U32x4.from_u128_bits(U128.bitwise_and(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise OR of the two vectors' 128 bits.
		##
		## Lowers to `por` on x86-64, `orr` on AArch64 NEON, and `v128.or`
		## on wasm.
		bitwise_or : U32x4, U32x4 -> U32x4
		bitwise_or = |a, b| U32x4.from_u128_bits(U128.bitwise_or(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise XOR of the two vectors' 128 bits.
		##
		## Lowers to `pxor` on x86-64, `eor` on AArch64 NEON, and
		## `v128.xor` on wasm.
		bitwise_xor : U32x4, U32x4 -> U32x4
		bitwise_xor = |a, b| U32x4.from_u128_bits(U128.bitwise_xor(a.to_u128_bits(), b.to_u128_bits()))

		## Flips every one of the vector's 128 bits.
		##
		## Lowers to `pxor` with all-ones on x86-64, `mvn` on AArch64 NEON,
		## and `v128.not` on wasm.
		bitwise_not : U32x4 -> U32x4
		bitwise_not = |vector| U32x4.from_u128_bits(U128.bitwise_not(vector.to_u128_bits()))

		## Bitwise select: for each of the 128 bits, take the bit from
		## `if_set` where this mask vector has a 1, and from `if_clear`
		## where it has a 0. Combined with the `_lanes` comparisons, this
		## is the branchless lane-wise `if`.
		##
		## Lowers to `pand`/`pandn`/`por` on x86-64, `bsl` on AArch64 NEON,
		## and `v128.bitselect` on wasm.
		bit_select : U32x4, U32x4, U32x4 -> U32x4
		bit_select = |mask, if_set, if_clear| {
			mask_bits = mask.to_u128_bits()
			kept = U128.bitwise_and(mask_bits, if_set.to_u128_bits())
			cleared = U128.bitwise_and(U128.bitwise_not(mask_bits), if_clear.to_u128_bits())
			U32x4.from_u128_bits(U128.bitwise_or(kept, cleared))
		}

		## Compare lane-wise for equality: each result lane is all-ones
		## (4294967295) where the lanes are equal and 0 where they differ.
		##
		## Lowers to `pcmpeqd` on x86-64, `cmeq` on AArch64 NEON, and
		## `i32x4.eq` on wasm.
		eq_lanes : U32x4, U32x4 -> U32x4
		eq_lanes = |a, b| U32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| if (x == y) U64.highest else 0))

		## Compare lane-wise: each result lane is all-ones where a's lane is
		## greater than b's (unsigned), else 0.
		##
		## x86-64 has no unsigned dword compare, so this lowers to a
		## sign-bias + `pcmpgtd`; AArch64 NEON `cmhi`; wasm `i32x4.gt_u`.
		gt_lanes : U32x4, U32x4 -> U32x4
		gt_lanes = |a, b| U32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| if (x > y) U64.highest else 0))

		## Compare lane-wise: each result lane is all-ones where a's lane is
		## less than b's (unsigned), else 0. See [U32x4.gt_lanes] for the
		## per-target lowerings.
		lt_lanes : U32x4, U32x4 -> U32x4
		lt_lanes = |a, b| U32x4.gt_lanes(b, a)

		## Compare lane-wise: each result lane is all-ones where a's lane is
		## greater than or equal to b's (unsigned), else 0.
		##
		## x86-64 has no unsigned dword compare, so this lowers to a
		## sign-bias + `pcmpgtd` + inversion; AArch64 NEON `cmhs`; wasm
		## `i32x4.ge_u`.
		gte_lanes : U32x4, U32x4 -> U32x4
		gte_lanes = |a, b| U32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| if (x >= y) U64.highest else 0))

		## Compare lane-wise: each result lane is all-ones where a's lane is
		## less than or equal to b's (unsigned), else 0. See
		## [U32x4.gte_lanes] for the per-target lowerings.
		lte_lanes : U32x4, U32x4 -> U32x4
		lte_lanes = |a, b| U32x4.gte_lanes(b, a)

		## One bit per lane (lane 0 in bit 0): 1 where the lane's most
		## significant bit is set. On the all-0/all-1 masks produced by the
		## `_lanes` comparisons, this packs the comparison results into a
		## [U8] for scalar decision-making.
		##
		## Lowers to `movmskps` on x86-64, a short emulated narrowing
		## sequence on AArch64 NEON (no single instruction), and
		## `i32x4.bitmask` on wasm.
		## ```roc
		## expect U32x4.splat(4294967295).to_bitmask() == 15
		## ```
		to_bitmask : U32x4 -> U8
		to_bitmask = |vector| simd128_bitmask(vector.to_u128_bits(), 32).to_u8_wrap()

		## Returns `Bool.True` if any lane's most significant bit is set.
		## On comparison masks: "did any lane match?"
		any_lanes_set : U32x4 -> Bool
		any_lanes_set = |vector| U32x4.to_bitmask(vector) != 0

		## Returns `Bool.True` if every lane's most significant bit is set.
		## On comparison masks: "did all lanes match?"
		all_lanes_set : U32x4 -> Bool
		all_lanes_set = |vector| U32x4.to_bitmask(vector) == 15

		## Shift every lane's bits left by the same count. The count is
		## taken modulo 32: shifting by 32 leaves every lane unchanged and
		## shifting by 33 shifts by 1, matching [U32.shl_wrap].
		##
		## Lowers to `pslld` on x86-64 with the count masked to the lane
		## width first, `shl` on AArch64 NEON taking the pre-masked count,
		## and `i32x4.shl` on wasm, which masks the count natively.
		shl_wrap : U32x4, U8 -> U32x4
		shl_wrap = |vector, count| U32x4.from_u128_bits(simd128_shift_left(vector.to_u128_bits(), 32, count))

		## Shift every lane's bits right by the same count, filling with
		## zeros. The count is taken modulo 32. For unsigned lanes this
		## behaves the same as [U32x4.shr_zf_wrap].
		shr_wrap : U32x4, U8 -> U32x4
		shr_wrap = |vector, count| U32x4.shr_zf_wrap(vector, count)

		## Shift every lane's bits right by the same count, filling the
		## vacated high bits with zeros. The count is taken modulo 32:
		## shifting by 32 leaves every lane unchanged and shifting by 33
		## shifts by 1, matching [U32.shr_zf_wrap].
		##
		## Lowers to `psrld` on x86-64 with the count masked to the lane
		## width first, `ushr` on AArch64 NEON taking the pre-masked count,
		## and `i32x4.shr_u` on wasm, which masks the count natively.
		shr_zf_wrap : U32x4, U8 -> U32x4
		shr_zf_wrap = |vector, count| U32x4.from_u128_bits(simd128_shift_right_zf(vector.to_u128_bits(), 32, count))

		## The value of the lane at the given index. Crashes if the index
		## is 4 or greater. (Lane indices are expected to be compile-time
		## constants in practice.)
		##
		## Lowers to `pextrd` (SSE4.1) on x86-64, `umov` on AArch64 NEON,
		## and `i32x4.extract_lane` on wasm.
		get_lane : U32x4, U64 -> U32
		get_lane = |vector, index|
			if index >= 4 {
				crash "U32x4.get_lane: lane index out of range"
			} else {
				simd128_get_lane(vector.to_u128_bits(), 32, index).to_u32_wrap()
			}

		## Returns the vector with the lane at the given index replaced by
		## the given value. Crashes if the index is 4 or greater.
		##
		## Lowers to `pinsrd` (SSE4.1) on x86-64, `ins` on AArch64 NEON, and
		## `i32x4.replace_lane` on wasm.
		with_lane : U32x4, U64, U32 -> U32x4
		with_lane = |vector, index, value|
			if index >= 4 {
				crash "U32x4.with_lane: lane index out of range"
			} else {
				U32x4.from_u128_bits(simd128_with_lane(vector.to_u128_bits(), 32, index, value.to_u64()))
			}

		## Returns a vector with every lane set to the lane of this vector
		## at the given index. Crashes if the index is 4 or greater.
		##
		## Lowers to `pshufd` with a constant pattern on x86-64, `dup`
		## (lane form) on AArch64 NEON, and a constant `i32x4.shuffle` on
		## wasm.
		broadcast_lane : U32x4, U64 -> U32x4
		broadcast_lane = |vector, index| U32x4.splat(U32x4.get_lane(vector, index))

		## Interleave the low 2 lanes of the two vectors: result lanes are
		## `a0, b0, a1, b1`. With [U32x4.interleave_hi], this is the building
		## block of matrix transposes and of widening data.
		##
		## Lowers to `punpckldq` on x86-64, `zip1` on AArch64 NEON, and a
		## constant `i32x4.shuffle` on wasm.
		interleave_lo : U32x4, U32x4 -> U32x4
		interleave_lo = |a, b| U32x4.from_u128_bits(simd128_interleave_lo(a.to_u128_bits(), b.to_u128_bits(), 32))

		## Interleave the high 2 lanes of the two vectors: result lanes are
		## `a2, b2, a3, b3`.
		##
		## Lowers to `punpckhdq` on x86-64, `zip2` on AArch64 NEON, and a
		## constant `i32x4.shuffle` on wasm.
		interleave_hi : U32x4, U32x4 -> U32x4
		interleave_hi = |a, b| U32x4.from_u128_bits(simd128_interleave_hi(a.to_u128_bits(), b.to_u128_bits(), 32))

		## The even-indexed lanes of a followed by the even-indexed lanes
		## of b — the deinterleaving inverse of the interleave operations,
		## used to split interleaved channel data apart.
		##
		## Lowers to `shufps`-class shuffles on x86-64, `uzp1` on AArch64
		## NEON, and a constant `i32x4.shuffle` on wasm.
		even_lanes : U32x4, U32x4 -> U32x4
		even_lanes = |a, b| U32x4.from_u128_bits(simd128_even_lanes(a.to_u128_bits(), b.to_u128_bits(), 32))

		## The odd-indexed lanes of a followed by the odd-indexed lanes of
		## b.
		##
		## Lowers to `shufps`-class shuffles on x86-64, `uzp2` on AArch64
		## NEON, and a constant `i32x4.shuffle` on wasm.
		odd_lanes : U32x4, U32x4 -> U32x4
		odd_lanes = |a, b| U32x4.from_u128_bits(simd128_odd_lanes(a.to_u128_bits(), b.to_u128_bits(), 32))

		## Returns the vector with its 4 lanes in reverse order.
		##
		## Lowers to `pshufd` with a constant pattern on x86-64, `rev64` +
		## `ext` on AArch64 NEON, and a constant `i32x4.shuffle` on wasm.
		reverse_lanes : U32x4 -> U32x4
		reverse_lanes = |vector| U32x4.from_u128_bits(simd128_reverse_lanes(vector.to_u128_bits(), 32))

		## Zero-extend lanes 0 and 1 into the two 64-bit lanes of a [U64x2].
		## With [U32x4.to_u64x2_hi], this is the widening step of the
		## widen-compute-narrow pattern.
		##
		## Lowers to `pmovzxdq` (SSE4.1) on x86-64, `uxtl` on AArch64 NEON,
		## and `i64x2.extend_low_i32x4_u` on wasm.
		to_u64x2_lo : U32x4 -> U64x2
		to_u64x2_lo = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 2 {
				$out = simd128_with_lane($out, 64, $i, simd128_get_lane(bits, 32, $i))
				$i = $i + 1
			}
			U64x2.from_u128_bits($out)
		}

		## Zero-extend lanes 2 and 3 into the two 64-bit lanes of a [U64x2].
		##
		## Lowers to `punpckhdq` with zero on x86-64, `uxtl2` on AArch64
		## NEON, and `i64x2.extend_high_i32x4_u` on wasm.
		to_u64x2_hi : U32x4 -> U64x2
		to_u64x2_hi = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 2 {
				$out = simd128_with_lane($out, 64, $i, simd128_get_lane(bits, 32, 2 + $i))
				$i = $i + 1
			}
			U64x2.from_u128_bits($out)
		}

		## Truncate each lane to its low 16 bits and pack into a [U16x8]:
		## result lanes 0-3 come from `a`, lanes 4-7 from `b`.
		##
		## Lowers to a `pand` + `packusdw` sequence on x86-64, `xtn` + `xtn2`
		## on AArch64 NEON, and an emulated sequence on wasm.
		narrow_to_u16x8_wrap : U32x4, U32x4 -> U16x8
		narrow_to_u16x8_wrap = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				$out = simd128_with_lane($out, 16, $i, simd128_get_lane(a_bits, 32, $i))
				$out = simd128_with_lane($out, 16, 4 + $i, simd128_get_lane(b_bits, 32, $i))
				$i = $i + 1
			}
			U16x8.from_u128_bits($out)
		}

		## Clamp each lane to `65535` and pack into a [U16x8]: result lanes
		## 0-3 come from `a`, lanes 4-7 from `b`.
		##
		## Lowers to `pminud` + `packusdw` (SSE4.1) on x86-64, `uqxtn` +
		## `uqxtn2` on AArch64 NEON, and an emulated sequence on wasm.
		narrow_to_u16x8_saturated : U32x4, U32x4 -> U16x8
		narrow_to_u16x8_saturated = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				$out = simd128_with_lane($out, 16, $i, U64.min(simd128_get_lane(a_bits, 32, $i), 65535))
				$out = simd128_with_lane($out, 16, 4 + $i, U64.min(simd128_get_lane(b_bits, 32, $i), 65535))
				$i = $i + 1
			}
			U16x8.from_u128_bits($out)
		}

		## The sum of all 4 lanes (at most 17179869180, so it always fits a
		## [U64]).
		##
		## Lowers to a `phaddd`-style shuffle-add sequence on x86-64,
		## `uaddlv` on AArch64 NEON, and a pairwise-add chain on wasm.
		sum_lanes : U32x4 -> U64
		sum_lanes = |vector| {
			bits = vector.to_u128_bits()
			var $sum = 0.U64
			var $i = 0.U64
			while $i < 4 {
				$sum = $sum + simd128_get_lane(bits, 32, $i)
				$i = $i + 1
			}
			$sum
		}

		## Read 16 bytes starting at the given byte index, as lanes in
		## little-endian order. Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`. Any alignment is fine.
		##
		## Lowers to `movdqu` on x86-64, `ldr` (Q register) on AArch64,
		## and `v128.load` on wasm.
		load : List(U8), U64 -> Try(U32x4, [OutOfBounds, ..])
		load = |bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(U32x4.from_u128_bits(simd128_from_bytes_at(bytes, index)))
			}
		}

		## Write these 16 bytes into the list starting at the given byte
		## index (in place when the list is unique), little-endian.
		## Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`.
		##
		## Lowers to `movdqu` (store form) on x86-64, `str` (Q register)
		## on AArch64, and `v128.store` on wasm.
		store : U32x4, List(U8), U64 -> Try(List(U8), [OutOfBounds, ..])
		store = |vector, bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(simd128_write_bytes_at(vector.to_u128_bits(), bytes, index))
			}
		}

		## Append these 16 bytes (little-endian) to the end of the list.
		append_to : U32x4, List(U8) -> List(U8)
		append_to = |vector, bytes| simd128_append_bytes(vector.to_u128_bits(), bytes)
	}

	## A 128-bit SIMD vector of 4 signed 32-bit lanes.
	##
	## Lane `i` occupies bits `[i * 32, (i + 1) * 32)` of the vector (two's
	## complement), and the byte-serialized form (used by [I32x4.load],
	## [I32x4.store], and [I32x4.to_list]) is little-endian with lane 0
	## first. Every operation has one pinned meaning that is bit-identical
	## on every target; the compiler lowers each operation to the best
	## instruction sequence for the target CPU (SSE/AVX on x86-64, NEON on
	## AArch64, simd128 on wasm).
	I32x4 :: { bits : U128 }.{

		## Returns the [I32x4] with every lane `0`.
		## ```roc
		## expect I32x4.default() == I32x4.splat(0)
		## ```
		default : () -> I32x4
		default = || I32x4.splat(0)

		## Returns an [I32x4] with every lane set to the given [I32].
		##
		## Lowers to a scalar-register move + `pshufd` on x86-64, `dup` on
		## AArch64 NEON, and `i32x4.splat` on wasm.
		## ```roc
		## expect I32x4.splat(-5).get_lane(2) == -5
		## ```
		splat : I32 -> I32x4
		splat = |value| I32x4.from_u128_bits(simd128_splat(value.to_u64_wrap(), 32))

		## Build an [I32x4] from exactly 4 lane values, lane 0 first.
		## Returns `Err(WrongLength)` if the list's length is not 4.
		from_list : List(I32) -> Try(I32x4, [WrongLength, ..])
		from_list = |lanes|
			if List.len(lanes) != 4 {
				Err(WrongLength)
			} else {
				var $bits = 0.U128
				var $i = 0.U64
				while $i < 4 {
					$bits = simd128_with_lane($bits, 32, $i, list_get_unsafe(lanes, $i).to_u64_wrap())
					$i = $i + 1
				}
				Ok(I32x4.from_u128_bits($bits))
			}

		## The 4 lane values as a list, lane 0 first.
		## ```roc
		## expect I32x4.splat(9).to_list() == List.repeat(9.I32, 4)
		## ```
		to_list : I32x4 -> List(I32)
		to_list = |vector| {
			var $out = List.with_capacity(4)
			var $i = 0.U64
			while $i < 4 {
				$out = list_append_unsafe($out, I32x4.get_lane(vector, $i))
				$i = $i + 1
			}
			$out
		}

		## Returns `Bool.True` if all 128 bits of the two vectors are equal.
		## (For a per-lane comparison producing a mask, see [I32x4.eq_lanes].)
		##
		## Lowers to `pxor` + `ptest` on x86-64, `cmeq` + `uminv` on AArch64
		## NEON, and `v128.xor` + `v128.any_true` on wasm.
		is_eq : I32x4, I32x4 -> Bool
		is_eq = |a, b| a.to_u128_bits() == b.to_u128_bits()

		## Feed an [I32x4] into a [Hasher].
		to_hash : I32x4, Hasher -> Hasher
		to_hash = |vector, hasher| Hasher.write_u128(hasher, vector.to_u128_bits())

		## Render the lanes for debugging, e.g. `I32x4(-1, 2, -3, 4)`.
		to_inspect : I32x4 -> Str
		to_inspect = |vector| simd128_inspect("I32x4", List.map(I32x4.to_list(vector), I32.to_str))

		## The vector's 128 bits as a [U128]. Lane `i` occupies bits
		## `[i * 32, (i + 1) * 32)`. Free at runtime — no instructions.
		to_u128_bits : I32x4 -> U128
		to_u128_bits = |vector| vector.bits

		## Build an [I32x4] from 128 raw bits. Free at runtime — no
		## instructions.
		from_u128_bits : U128 -> I32x4
		from_u128_bits = |bits| I32x4.{ bits }

		## Reinterpret the same 128 bits as a [U8x16]. Free at runtime.
		to_u8x16_bits : I32x4 -> U8x16
		to_u8x16_bits = |vector| U8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I8x16]. Free at runtime.
		to_i8x16_bits : I32x4 -> I8x16
		to_i8x16_bits = |vector| I8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U16x8]. Free at runtime.
		to_u16x8_bits : I32x4 -> U16x8
		to_u16x8_bits = |vector| U16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I16x8]. Free at runtime.
		to_i16x8_bits : I32x4 -> I16x8
		to_i16x8_bits = |vector| I16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U32x4]. Free at runtime.
		to_u32x4_bits : I32x4 -> U32x4
		to_u32x4_bits = |vector| U32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U64x2]. Free at runtime.
		to_u64x2_bits : I32x4 -> U64x2
		to_u64x2_bits = |vector| U64x2.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I64x2]. Free at runtime.
		to_i64x2_bits : I32x4 -> I64x2
		to_i64x2_bits = |vector| I64x2.from_u128_bits(vector.to_u128_bits())

		## Add lane-wise, each lane wrapping mod 4294967296.
		##
		## Lowers to `paddd` on x86-64, `add` (4×32-bit) on AArch64 NEON,
		## and `i32x4.add` on wasm.
		plus_wrap : I32x4, I32x4 -> I32x4
		plus_wrap = |a, b| I32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| x + y))

		## Subtract lane-wise, each lane wrapping mod 4294967296.
		##
		## Lowers to `psubd` on x86-64, `sub` (4×32-bit) on AArch64 NEON,
		## and `i32x4.sub` on wasm.
		minus_wrap : I32x4, I32x4 -> I32x4
		minus_wrap = |a, b| I32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| (x + 4294967296) - y))

		## Negate each lane, wrapping mod 4294967296. The most negative lane
		## (-2147483648) negates to itself.
		##
		## Lowers to `psubd` from zero on x86-64, `neg` on AArch64 NEON, and
		## `i32x4.neg` on wasm.
		## ```roc
		## expect I32x4.splat(5).negate_wrap().get_lane(0) == -5
		## ```
		negate_wrap : I32x4 -> I32x4
		negate_wrap = |vector| I32x4.from_u128_bits(simd128_map1(vector.to_u128_bits(), 32, |x| 4294967296 - x))

		## The absolute value of each lane, wrapping mod 4294967296. The most
		## negative lane (-2147483648) has no positive counterpart and stays
		## -2147483648.
		##
		## Lowers to `pabsd` (SSSE3) on x86-64, `abs` on AArch64 NEON, and
		## `i32x4.abs` on wasm.
		## ```roc
		## expect I32x4.splat(-5).abs_wrap().get_lane(0) == 5
		## ```
		abs_wrap : I32x4 -> I32x4
		abs_wrap = |vector| I32x4.from_u128_bits(simd128_map1(vector.to_u128_bits(), 32, |x| if (simd128_lane_to_signed(x, 32) < 0) (4294967296 - x) else x))

		## The smaller of each pair of lanes (signed).
		##
		## Lowers to `pminsd` (SSE4.1) on x86-64, `smin` on AArch64 NEON,
		## and `i32x4.min_s` on wasm.
		## ```roc
		## expect I32x4.splat(-5).min(I32x4.splat(3)).get_lane(0) == -5
		## ```
		min : I32x4, I32x4 -> I32x4
		min = |a, b| I32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| if (simd128_lane_to_signed(x, 32) < simd128_lane_to_signed(y, 32)) x else y))

		## The larger of each pair of lanes (signed).
		##
		## Lowers to `pmaxsd` (SSE4.1) on x86-64, `smax` on AArch64 NEON,
		## and `i32x4.max_s` on wasm.
		max : I32x4, I32x4 -> I32x4
		max = |a, b| I32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| if (simd128_lane_to_signed(x, 32) > simd128_lane_to_signed(y, 32)) x else y))

		## Multiply lane-wise, each lane wrapping mod 4294967296.
		##
		## Lowers to `pmulld` (SSE4.1) on x86-64, `mul` (4×32-bit) on
		## AArch64 NEON, and `i32x4.mul` on wasm.
		times_wrap : I32x4, I32x4 -> I32x4
		times_wrap = |a, b| I32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| x * y))

		## Multiply lanes 0 and 1 of the two vectors pairwise (signed) into
		## the two 64-bit lanes of an [I64x2] (no overflow is possible).
		##
		## Lowers to `pmuldq` (SSE4.1) on x86-64, `smull` on AArch64 NEON,
		## and `i64x2.extmul_low_i32x4_s` on wasm.
		times_wide_lo : I32x4, I32x4 -> I64x2
		times_wide_lo = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 2 {
				product = simd128_lane_to_signed(simd128_get_lane(a_bits, 32, $i), 32) * simd128_lane_to_signed(simd128_get_lane(b_bits, 32, $i), 32)
				$out = simd128_with_lane($out, 64, $i, product.to_u64_wrap())
				$i = $i + 1
			}
			I64x2.from_u128_bits($out)
		}

		## Multiply lanes 2 and 3 of the two vectors pairwise (signed) into
		## the two 64-bit lanes of an [I64x2] (no overflow is possible).
		##
		## Lowers to `pmuldq` (SSE4.1) on x86-64, `smull2` on AArch64 NEON,
		## and `i64x2.extmul_high_i32x4_s` on wasm.
		times_wide_hi : I32x4, I32x4 -> I64x2
		times_wide_hi = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 2 {
				product = simd128_lane_to_signed(simd128_get_lane(a_bits, 32, 2 + $i), 32) * simd128_lane_to_signed(simd128_get_lane(b_bits, 32, 2 + $i), 32)
				$out = simd128_with_lane($out, 64, $i, product.to_u64_wrap())
				$i = $i + 1
			}
			I64x2.from_u128_bits($out)
		}

		## Returns the bitwise AND of the two vectors' 128 bits.
		##
		## Lowers to `pand` on x86-64, `and` on AArch64 NEON, and
		## `v128.and` on wasm.
		bitwise_and : I32x4, I32x4 -> I32x4
		bitwise_and = |a, b| I32x4.from_u128_bits(U128.bitwise_and(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise OR of the two vectors' 128 bits.
		##
		## Lowers to `por` on x86-64, `orr` on AArch64 NEON, and `v128.or`
		## on wasm.
		bitwise_or : I32x4, I32x4 -> I32x4
		bitwise_or = |a, b| I32x4.from_u128_bits(U128.bitwise_or(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise XOR of the two vectors' 128 bits.
		##
		## Lowers to `pxor` on x86-64, `eor` on AArch64 NEON, and
		## `v128.xor` on wasm.
		bitwise_xor : I32x4, I32x4 -> I32x4
		bitwise_xor = |a, b| I32x4.from_u128_bits(U128.bitwise_xor(a.to_u128_bits(), b.to_u128_bits()))

		## Flips every one of the vector's 128 bits.
		##
		## Lowers to `pxor` with all-ones on x86-64, `mvn` on AArch64 NEON,
		## and `v128.not` on wasm.
		bitwise_not : I32x4 -> I32x4
		bitwise_not = |vector| I32x4.from_u128_bits(U128.bitwise_not(vector.to_u128_bits()))

		## Bitwise select: for each of the 128 bits, take the bit from
		## `if_set` where this mask vector has a 1, and from `if_clear`
		## where it has a 0. Combined with the `_lanes` comparisons, this
		## is the branchless lane-wise `if`.
		##
		## Lowers to `pand`/`pandn`/`por` on x86-64, `bsl` on AArch64 NEON,
		## and `v128.bitselect` on wasm.
		bit_select : I32x4, I32x4, I32x4 -> I32x4
		bit_select = |mask, if_set, if_clear| {
			mask_bits = mask.to_u128_bits()
			kept = U128.bitwise_and(mask_bits, if_set.to_u128_bits())
			cleared = U128.bitwise_and(U128.bitwise_not(mask_bits), if_clear.to_u128_bits())
			I32x4.from_u128_bits(U128.bitwise_or(kept, cleared))
		}

		## Compare lane-wise for equality: each result lane is all-ones
		## where the lanes are equal and 0 where they differ.
		##
		## Lowers to `pcmpeqd` on x86-64, `cmeq` on AArch64 NEON, and
		## `i32x4.eq` on wasm.
		eq_lanes : I32x4, I32x4 -> I32x4
		eq_lanes = |a, b| I32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| if (x == y) U64.highest else 0))

		## Compare lane-wise: each result lane is all-ones where a's lane is
		## greater than b's (signed), else 0.
		##
		## Lowers to `pcmpgtd` on x86-64, `cmgt` on AArch64 NEON, and
		## `i32x4.gt_s` on wasm.
		gt_lanes : I32x4, I32x4 -> I32x4
		gt_lanes = |a, b| I32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| if (simd128_lane_to_signed(x, 32) > simd128_lane_to_signed(y, 32)) U64.highest else 0))

		## Compare lane-wise: each result lane is all-ones where a's lane is
		## less than b's (signed), else 0. See [I32x4.gt_lanes] for the
		## per-target lowerings.
		lt_lanes : I32x4, I32x4 -> I32x4
		lt_lanes = |a, b| I32x4.gt_lanes(b, a)

		## Compare lane-wise: each result lane is all-ones where a's lane is
		## greater than or equal to b's (signed), else 0.
		##
		## Lowers to `pcmpgtd` + `pcmpeqd` + `por` on x86-64, `cmge` on
		## AArch64 NEON, and `i32x4.ge_s` on wasm.
		gte_lanes : I32x4, I32x4 -> I32x4
		gte_lanes = |a, b| I32x4.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 32, |x, y| if (simd128_lane_to_signed(x, 32) >= simd128_lane_to_signed(y, 32)) U64.highest else 0))

		## Compare lane-wise: each result lane is all-ones where a's lane is
		## less than or equal to b's (signed), else 0. See [I32x4.gte_lanes]
		## for the per-target lowerings.
		lte_lanes : I32x4, I32x4 -> I32x4
		lte_lanes = |a, b| I32x4.gte_lanes(b, a)

		## One bit per lane (lane 0 in bit 0): 1 where the lane's most
		## significant (sign) bit is set. On the all-0/all-1 masks produced
		## by the `_lanes` comparisons, this packs the comparison results
		## into a [U8] for scalar decision-making.
		##
		## Lowers to `movmskps` on x86-64, a short emulated narrowing
		## sequence on AArch64 NEON (no single instruction), and
		## `i32x4.bitmask` on wasm.
		to_bitmask : I32x4 -> U8
		to_bitmask = |vector| simd128_bitmask(vector.to_u128_bits(), 32).to_u8_wrap()

		## Returns `Bool.True` if any lane's sign bit is set (any lane is
		## negative, or on a comparison mask, "did any lane match?").
		any_lanes_set : I32x4 -> Bool
		any_lanes_set = |vector| I32x4.to_bitmask(vector) != 0

		## Returns `Bool.True` if every lane's sign bit is set (every lane
		## is negative, or on a comparison mask, "did all lanes match?").
		all_lanes_set : I32x4 -> Bool
		all_lanes_set = |vector| I32x4.to_bitmask(vector) == 15

		## Shift every lane's bits left by the same count. The count is
		## taken modulo 32: shifting by 32 leaves every lane unchanged and
		## shifting by 33 shifts by 1, matching [I32.shl_wrap].
		##
		## Lowers to `pslld` on x86-64 with the count masked to the lane
		## width first, `shl` on AArch64 NEON taking the pre-masked count,
		## and `i32x4.shl` on wasm, which masks the count natively.
		shl_wrap : I32x4, U8 -> I32x4
		shl_wrap = |vector, count| I32x4.from_u128_bits(simd128_shift_left(vector.to_u128_bits(), 32, count))

		## Shift every lane's bits right by the same count, replicating the
		## sign bit into the vacated high bits (arithmetic shift). The count
		## is taken modulo 32: shifting by 32 leaves every lane unchanged and
		## shifting by 33 shifts by 1, matching [I32.shr_wrap].
		##
		## Lowers to `psrad` on x86-64 with the count masked to the lane
		## width first, `sshr` on AArch64 NEON taking the pre-masked count,
		## and `i32x4.shr_s` on wasm, which masks the count natively.
		shr_wrap : I32x4, U8 -> I32x4
		shr_wrap = |vector, count| I32x4.from_u128_bits(simd128_shift_right_arith(vector.to_u128_bits(), 32, count))

		## Shift every lane's bits right by the same count, filling the
		## vacated high bits with zeros. The count is taken modulo 32:
		## shifting by 32 leaves every lane unchanged and shifting by 33
		## shifts by 1, matching [I32.shr_zf_wrap].
		##
		## Lowers to `psrld` on x86-64 with the count masked to the lane
		## width first, `ushr` on AArch64 NEON taking the pre-masked count,
		## and `i32x4.shr_u` on wasm, which masks the count natively.
		shr_zf_wrap : I32x4, U8 -> I32x4
		shr_zf_wrap = |vector, count| I32x4.from_u128_bits(simd128_shift_right_zf(vector.to_u128_bits(), 32, count))

		## Arithmetic right shift that rounds to nearest by adding a
		## half-ULP bias first: each lane becomes
		## `(lane + 2^(count-1)) >> count`, so halves round toward positive
		## infinity. A count of `0` leaves the lane unchanged; counts of 32
		## or more produce 0. This is the rounding step of fixed-point
		## transform kernels.
		##
		## Lowers to a `paddd` + `psrad` sequence on x86-64, `srshr` on
		## AArch64 NEON, and an emulated sequence on wasm.
		shift_right_rounded_by : I32x4, U8 -> I32x4
		shift_right_rounded_by = |vector, count|
			if count == 0 {
				vector
			} else if count >= 32 {
				I32x4.splat(0)
			} else {
				rounding = I64.shl_wrap(1, count - 1)
				I32x4.from_u128_bits(simd128_map1(vector.to_u128_bits(), 32, |lane| I64.shr_wrap(simd128_lane_to_signed(lane, 32) + rounding, count).to_u64_wrap()))
			}

		## Clamp each signed lane to the [I16] range and pack into an
		## [I16x8]: result lanes 0-3 come from `a`, lanes 4-7 from `b`.
		##
		## Lowers to `packssdw` on x86-64, `sqxtn` + `sqxtn2` on AArch64
		## NEON, and `i16x8.narrow_i32x4_s` on wasm.
		narrow_to_i16x8_saturated : I32x4, I32x4 -> I16x8
		narrow_to_i16x8_saturated = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				$out = simd128_with_lane($out, 16, $i, simd128_clamp_signed(simd128_lane_to_signed(simd128_get_lane(a_bits, 32, $i), 32), 16))
				$out = simd128_with_lane($out, 16, 4 + $i, simd128_clamp_signed(simd128_lane_to_signed(simd128_get_lane(b_bits, 32, $i), 32), 16))
				$i = $i + 1
			}
			I16x8.from_u128_bits($out)
		}

		## Clamp each signed lane to the [U16] range (negatives become 0)
		## and pack into a [U16x8]: result lanes 0-3 come from `a`, lanes
		## 4-7 from `b`.
		##
		## Lowers to `packusdw` (SSE4.1) on x86-64, `sqxtun` + `sqxtun2` on
		## AArch64 NEON, and `i16x8.narrow_i32x4_u` on wasm.
		narrow_to_u16x8_saturated : I32x4, I32x4 -> U16x8
		narrow_to_u16x8_saturated = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 4 {
				$out = simd128_with_lane($out, 16, $i, simd128_clamp_to_unsigned(simd128_lane_to_signed(simd128_get_lane(a_bits, 32, $i), 32), 16))
				$out = simd128_with_lane($out, 16, 4 + $i, simd128_clamp_to_unsigned(simd128_lane_to_signed(simd128_get_lane(b_bits, 32, $i), 32), 16))
				$i = $i + 1
			}
			U16x8.from_u128_bits($out)
		}

		## Sign-extend lanes 0 and 1 into the two 64-bit lanes of an [I64x2].
		## With [I32x4.to_i64x2_hi], this is the widening step of the
		## widen-compute-narrow pattern.
		##
		## Lowers to `pmovsxdq` (SSE4.1) on x86-64, `sxtl` on AArch64 NEON,
		## and `i64x2.extend_low_i32x4_s` on wasm.
		to_i64x2_lo : I32x4 -> I64x2
		to_i64x2_lo = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 2 {
				$out = simd128_with_lane($out, 64, $i, simd128_lane_to_signed(simd128_get_lane(bits, 32, $i), 32).to_u64_wrap())
				$i = $i + 1
			}
			I64x2.from_u128_bits($out)
		}

		## Sign-extend lanes 2 and 3 into the two 64-bit lanes of an [I64x2].
		##
		## Lowers to `pmovsxdq` (SSE4.1) on the high two lanes on x86-64,
		## `sxtl2` on AArch64 NEON, and `i64x2.extend_high_i32x4_s` on wasm.
		to_i64x2_hi : I32x4 -> I64x2
		to_i64x2_hi = |vector| {
			bits = vector.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 2 {
				$out = simd128_with_lane($out, 64, $i, simd128_lane_to_signed(simd128_get_lane(bits, 32, 2 + $i), 32).to_u64_wrap())
				$i = $i + 1
			}
			I64x2.from_u128_bits($out)
		}

		## The sum of all 4 lanes (signed), as an [I64] (the sum always
		## fits).
		##
		## Lowers to a `phaddd`-style shuffle-add sequence on x86-64,
		## `saddlv` on AArch64 NEON, and a pairwise-add chain on wasm.
		sum_lanes : I32x4 -> I64
		sum_lanes = |vector| {
			bits = vector.to_u128_bits()
			var $sum = 0.I64
			var $i = 0.U64
			while $i < 4 {
				$sum = $sum + simd128_lane_to_signed(simd128_get_lane(bits, 32, $i), 32)
				$i = $i + 1
			}
			$sum
		}

		## The value of the lane at the given index. Crashes if the index
		## is 4 or greater. (Lane indices are expected to be compile-time
		## constants in practice.)
		##
		## Lowers to `pextrd` (SSE4.1) on x86-64, `smov` on AArch64 NEON,
		## and `i32x4.extract_lane` on wasm.
		get_lane : I32x4, U64 -> I32
		get_lane = |vector, index|
			if index >= 4 {
				crash "I32x4.get_lane: lane index out of range"
			} else {
				simd128_lane_to_signed(simd128_get_lane(vector.to_u128_bits(), 32, index), 32).to_i32_wrap()
			}

		## Returns the vector with the lane at the given index replaced by
		## the given value. Crashes if the index is 4 or greater.
		##
		## Lowers to `pinsrd` (SSE4.1) on x86-64, `ins` on AArch64 NEON, and
		## `i32x4.replace_lane` on wasm.
		with_lane : I32x4, U64, I32 -> I32x4
		with_lane = |vector, index, value|
			if index >= 4 {
				crash "I32x4.with_lane: lane index out of range"
			} else {
				I32x4.from_u128_bits(simd128_with_lane(vector.to_u128_bits(), 32, index, value.to_u64_wrap()))
			}

		## Returns a vector with every lane set to the lane of this vector
		## at the given index. Crashes if the index is 4 or greater.
		##
		## Lowers to `pshufd` with a constant pattern on x86-64, `dup`
		## (lane form) on AArch64 NEON, and a constant `i32x4.shuffle` on
		## wasm.
		broadcast_lane : I32x4, U64 -> I32x4
		broadcast_lane = |vector, index| I32x4.splat(I32x4.get_lane(vector, index))

		## Interleave the low 2 lanes of the two vectors: result lanes are
		## `a0, b0, a1, b1`. With [I32x4.interleave_hi], this is the building
		## block of matrix transposes and of widening data.
		##
		## Lowers to `punpckldq` on x86-64, `zip1` on AArch64 NEON, and a
		## constant `i32x4.shuffle` on wasm.
		interleave_lo : I32x4, I32x4 -> I32x4
		interleave_lo = |a, b| I32x4.from_u128_bits(simd128_interleave_lo(a.to_u128_bits(), b.to_u128_bits(), 32))

		## Interleave the high 2 lanes of the two vectors: result lanes are
		## `a2, b2, a3, b3`.
		##
		## Lowers to `punpckhdq` on x86-64, `zip2` on AArch64 NEON, and a
		## constant `i32x4.shuffle` on wasm.
		interleave_hi : I32x4, I32x4 -> I32x4
		interleave_hi = |a, b| I32x4.from_u128_bits(simd128_interleave_hi(a.to_u128_bits(), b.to_u128_bits(), 32))

		## The even-indexed lanes of a followed by the even-indexed lanes
		## of b — the deinterleaving inverse of the interleave operations,
		## used to split interleaved channel data apart.
		##
		## Lowers to `shufps`-class shuffles on x86-64, `uzp1` on AArch64
		## NEON, and a constant `i32x4.shuffle` on wasm.
		even_lanes : I32x4, I32x4 -> I32x4
		even_lanes = |a, b| I32x4.from_u128_bits(simd128_even_lanes(a.to_u128_bits(), b.to_u128_bits(), 32))

		## The odd-indexed lanes of a followed by the odd-indexed lanes of
		## b.
		##
		## Lowers to `shufps`-class shuffles on x86-64, `uzp2` on AArch64
		## NEON, and a constant `i32x4.shuffle` on wasm.
		odd_lanes : I32x4, I32x4 -> I32x4
		odd_lanes = |a, b| I32x4.from_u128_bits(simd128_odd_lanes(a.to_u128_bits(), b.to_u128_bits(), 32))

		## Returns the vector with its 4 lanes in reverse order.
		##
		## Lowers to `pshufd` with a constant pattern on x86-64, `rev64` +
		## `ext` on AArch64 NEON, and a constant `i32x4.shuffle` on wasm.
		reverse_lanes : I32x4 -> I32x4
		reverse_lanes = |vector| I32x4.from_u128_bits(simd128_reverse_lanes(vector.to_u128_bits(), 32))

		## Read 16 bytes starting at the given byte index, as lanes in
		## little-endian order. Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`. Any alignment is fine.
		##
		## Lowers to `movdqu` on x86-64, `ldr` (Q register) on AArch64,
		## and `v128.load` on wasm.
		load : List(U8), U64 -> Try(I32x4, [OutOfBounds, ..])
		load = |bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(I32x4.from_u128_bits(simd128_from_bytes_at(bytes, index)))
			}
		}

		## Write these 16 bytes into the list starting at the given byte
		## index (in place when the list is unique), little-endian.
		## Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`.
		##
		## Lowers to `movdqu` (store form) on x86-64, `str` (Q register)
		## on AArch64, and `v128.store` on wasm.
		store : I32x4, List(U8), U64 -> Try(List(U8), [OutOfBounds, ..])
		store = |vector, bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(simd128_write_bytes_at(vector.to_u128_bits(), bytes, index))
			}
		}

		## Append these 16 bytes (little-endian) to the end of the list.
		append_to : I32x4, List(U8) -> List(U8)
		append_to = |vector, bytes| simd128_append_bytes(vector.to_u128_bits(), bytes)
	}

	## A 128-bit SIMD vector of 2 unsigned 64-bit lanes.
	##
	## Lane `i` occupies bits `[i * 64, (i + 1) * 64)` of the vector, and
	## the byte-serialized form (used by [U64x2.load], [U64x2.store], and
	## [U64x2.append_to]) is little-endian with lane 0 first. Every operation
	## has one pinned meaning that is bit-identical on every target; the
	## compiler lowers each operation to the best instruction sequence for
	## the target CPU (SSE/AVX on x86-64, NEON on AArch64, simd128 on wasm).
	U64x2 :: { bits : U128 }.{

		## Returns the [U64x2] with every lane `0`.
		## ```roc
		## expect U64x2.default() == U64x2.splat(0)
		## ```
		default : () -> U64x2
		default = || U64x2.splat(0)

		## Returns a [U64x2] with every lane set to the given [U64].
		##
		## Lowers to `pshufd` or `movddup` on x86-64, `dup` on AArch64 NEON,
		## and `i64x2.splat` on wasm.
		## ```roc
		## expect U64x2.splat(7).get_lane(1) == 7
		## ```
		splat : U64 -> U64x2
		splat = |value| U64x2.from_u128_bits(simd128_splat(value, 64))

		## Build a [U64x2] from exactly 2 lane values, lane 0 first.
		## Returns `Err(WrongLength)` if the list's length is not 2.
		from_list : List(U64) -> Try(U64x2, [WrongLength, ..])
		from_list = |lanes|
			if List.len(lanes) != 2 {
				Err(WrongLength)
			} else {
				var $bits = 0.U128
				var $i = 0.U64
				while $i < 2 {
					$bits = simd128_with_lane($bits, 64, $i, list_get_unsafe(lanes, $i))
					$i = $i + 1
				}
				Ok(U64x2.from_u128_bits($bits))
			}

		## The 2 lane values as a list, lane 0 first.
		## ```roc
		## expect U64x2.splat(9).to_list() == List.repeat(9.U64, 2)
		## ```
		to_list : U64x2 -> List(U64)
		to_list = |vector| {
			var $out = List.with_capacity(2)
			var $i = 0.U64
			while $i < 2 {
				$out = list_append_unsafe($out, U64x2.get_lane(vector, $i))
				$i = $i + 1
			}
			$out
		}

		## Returns `Bool.True` if all 128 bits of the two vectors are equal.
		## (For a per-lane comparison producing a mask, see [U64x2.eq_lanes].)
		##
		## Lowers to `pxor` + `ptest` on x86-64, `cmeq` + `uminv` on AArch64
		## NEON, and `v128.xor` + `v128.any_true` on wasm.
		is_eq : U64x2, U64x2 -> Bool
		is_eq = |a, b| a.to_u128_bits() == b.to_u128_bits()

		## Feed a [U64x2] into a [Hasher].
		to_hash : U64x2, Hasher -> Hasher
		to_hash = |vector, hasher| Hasher.write_u128(hasher, vector.to_u128_bits())

		## Render the lanes for debugging, e.g. `U64x2(1, 2)`.
		to_inspect : U64x2 -> Str
		to_inspect = |vector| simd128_inspect("U64x2", List.map(U64x2.to_list(vector), U64.to_str))

		## The vector's 128 bits as a [U128]. Lane `i` occupies bits
		## `[i * 64, (i + 1) * 64)`. Free at runtime — no instructions.
		to_u128_bits : U64x2 -> U128
		to_u128_bits = |vector| vector.bits

		## Build a [U64x2] from 128 raw bits. Free at runtime — no
		## instructions.
		from_u128_bits : U128 -> U64x2
		from_u128_bits = |bits| U64x2.{ bits }

		## Reinterpret the same 128 bits as a [U8x16]. Free at runtime.
		to_u8x16_bits : U64x2 -> U8x16
		to_u8x16_bits = |vector| U8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I8x16]. Free at runtime.
		to_i8x16_bits : U64x2 -> I8x16
		to_i8x16_bits = |vector| I8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U16x8]. Free at runtime.
		to_u16x8_bits : U64x2 -> U16x8
		to_u16x8_bits = |vector| U16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I16x8]. Free at runtime.
		to_i16x8_bits : U64x2 -> I16x8
		to_i16x8_bits = |vector| I16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U32x4]. Free at runtime.
		to_u32x4_bits : U64x2 -> U32x4
		to_u32x4_bits = |vector| U32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I32x4]. Free at runtime.
		to_i32x4_bits : U64x2 -> I32x4
		to_i32x4_bits = |vector| I32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I64x2]. Free at runtime.
		to_i64x2_bits : U64x2 -> I64x2
		to_i64x2_bits = |vector| I64x2.from_u128_bits(vector.to_u128_bits())

		## Add lane-wise, each lane wrapping mod 2^64. Each lane widens to a
		## [U128] for the add so the intermediate cannot overflow, then wraps
		## back to 64 bits.
		##
		## Lowers to `paddq` on x86-64, `add` (2×64-bit) on AArch64 NEON, and
		## `i64x2.add` on wasm.
		## ```roc
		## expect U64x2.splat(1).plus_wrap(U64x2.splat(2)).get_lane(0) == 3
		## ```
		plus_wrap : U64x2, U64x2 -> U64x2
		plus_wrap = |a, b| U64x2.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 64, |x, y| (x.to_u128() + y.to_u128()).to_u64_wrap()))

		## Subtract lane-wise, each lane wrapping mod 2^64. Each lane widens to
		## a [U128] and borrows 2^64 before subtracting so the intermediate
		## stays non-negative, then wraps back to 64 bits.
		##
		## Lowers to `psubq` on x86-64, `sub` (2×64-bit) on AArch64 NEON, and
		## `i64x2.sub` on wasm.
		minus_wrap : U64x2, U64x2 -> U64x2
		minus_wrap = |a, b| U64x2.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 64, |x, y| ((x.to_u128() + 18446744073709551616) - y.to_u128()).to_u64_wrap()))

		## Returns the bitwise AND of the two vectors' 128 bits.
		##
		## Lowers to `pand` on x86-64, `and` on AArch64 NEON, and
		## `v128.and` on wasm.
		bitwise_and : U64x2, U64x2 -> U64x2
		bitwise_and = |a, b| U64x2.from_u128_bits(U128.bitwise_and(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise OR of the two vectors' 128 bits.
		##
		## Lowers to `por` on x86-64, `orr` on AArch64 NEON, and `v128.or`
		## on wasm.
		bitwise_or : U64x2, U64x2 -> U64x2
		bitwise_or = |a, b| U64x2.from_u128_bits(U128.bitwise_or(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise XOR of the two vectors' 128 bits.
		##
		## Lowers to `pxor` on x86-64, `eor` on AArch64 NEON, and
		## `v128.xor` on wasm.
		bitwise_xor : U64x2, U64x2 -> U64x2
		bitwise_xor = |a, b| U64x2.from_u128_bits(U128.bitwise_xor(a.to_u128_bits(), b.to_u128_bits()))

		## Flips every one of the vector's 128 bits.
		##
		## Lowers to `pxor` with all-ones on x86-64, `mvn` on AArch64 NEON,
		## and `v128.not` on wasm.
		bitwise_not : U64x2 -> U64x2
		bitwise_not = |vector| U64x2.from_u128_bits(U128.bitwise_not(vector.to_u128_bits()))

		## Bitwise select: for each of the 128 bits, take the bit from
		## `if_set` where this mask vector has a 1, and from `if_clear`
		## where it has a 0. Combined with the `_lanes` comparisons, this
		## is the branchless lane-wise `if`.
		##
		## Lowers to `pand`/`pandn`/`por` on x86-64, `bsl` on AArch64 NEON,
		## and `v128.bitselect` on wasm.
		bit_select : U64x2, U64x2, U64x2 -> U64x2
		bit_select = |mask, if_set, if_clear| {
			mask_bits = mask.to_u128_bits()
			kept = U128.bitwise_and(mask_bits, if_set.to_u128_bits())
			cleared = U128.bitwise_and(U128.bitwise_not(mask_bits), if_clear.to_u128_bits())
			U64x2.from_u128_bits(U128.bitwise_or(kept, cleared))
		}

		## Compare lane-wise for equality: each result lane is all-ones where
		## the lanes are equal and 0 where they differ.
		##
		## Lowers to `pcmpeqq` (SSE4.1) on x86-64, `cmeq` on AArch64 NEON, and
		## `i64x2.eq` on wasm.
		eq_lanes : U64x2, U64x2 -> U64x2
		eq_lanes = |a, b| U64x2.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 64, |x, y| if (x == y) U64.highest else 0))

		## One bit per lane (lane 0 in bit 0): 1 where the lane's most
		## significant bit is set. On the all-0/all-1 masks produced by
		## [U64x2.eq_lanes], this packs the comparison results into a [U8] for
		## scalar decision-making.
		##
		## Lowers to `movmskpd` on x86-64, a short emulated sequence on AArch64
		## NEON (no single instruction), and `i64x2.bitmask` on wasm.
		## ```roc
		## expect U64x2.splat(18446744073709551615).to_bitmask() == 3
		## ```
		to_bitmask : U64x2 -> U8
		to_bitmask = |vector| simd128_bitmask(vector.to_u128_bits(), 64).to_u8_wrap()

		## Returns `Bool.True` if any lane's most significant bit is set.
		## On comparison masks: "did any lane match?"
		any_lanes_set : U64x2 -> Bool
		any_lanes_set = |vector| U64x2.to_bitmask(vector) != 0

		## Returns `Bool.True` if every lane's most significant bit is set.
		## On comparison masks: "did all lanes match?"
		all_lanes_set : U64x2 -> Bool
		all_lanes_set = |vector| U64x2.to_bitmask(vector) == 3

		## Shift every lane's bits left by the same count. The count is
		## taken modulo 64: shifting by 64 leaves every lane unchanged and
		## shifting by 65 shifts by 1, matching [U64.shl_wrap].
		##
		## Lowers to `psllq` on x86-64 with the count masked to the lane
		## width first, `shl` on AArch64 NEON taking the pre-masked count,
		## and `i64x2.shl` on wasm, which masks the count natively.
		shl_wrap : U64x2, U8 -> U64x2
		shl_wrap = |vector, count| U64x2.from_u128_bits(simd128_shift_left(vector.to_u128_bits(), 64, count))

		## Shift every lane's bits right by the same count, filling with
		## zeros. The count is taken modulo 64. For unsigned lanes this
		## behaves the same as [U64x2.shr_zf_wrap].
		shr_wrap : U64x2, U8 -> U64x2
		shr_wrap = |vector, count| U64x2.shr_zf_wrap(vector, count)

		## Shift every lane's bits right by the same count, filling the
		## vacated high bits with zeros. The count is taken modulo 64:
		## shifting by 64 leaves every lane unchanged and shifting by 65
		## shifts by 1, matching [U64.shr_zf_wrap].
		##
		## Lowers to `psrlq` on x86-64 with the count masked to the lane
		## width first, `ushr` on AArch64 NEON taking the pre-masked count,
		## and `i64x2.shr_u` on wasm, which masks the count natively.
		shr_zf_wrap : U64x2, U8 -> U64x2
		shr_zf_wrap = |vector, count| U64x2.from_u128_bits(simd128_shift_right_zf(vector.to_u128_bits(), 64, count))

		## The value of the lane at the given index. Crashes if the index
		## is 2 or greater. (Lane indices are expected to be compile-time
		## constants in practice.)
		##
		## Lowers to `pextrq` (SSE4.1) on x86-64, `umov` on AArch64 NEON, and
		## `i64x2.extract_lane` on wasm.
		get_lane : U64x2, U64 -> U64
		get_lane = |vector, index|
			if index >= 2 {
				crash "U64x2.get_lane: lane index out of range"
			} else {
				simd128_get_lane(vector.to_u128_bits(), 64, index)
			}

		## Returns the vector with the lane at the given index replaced by
		## the given value. Crashes if the index is 2 or greater.
		##
		## Lowers to `pinsrq` (SSE4.1) on x86-64, `ins` on AArch64 NEON, and
		## `i64x2.replace_lane` on wasm.
		with_lane : U64x2, U64, U64 -> U64x2
		with_lane = |vector, index, value|
			if index >= 2 {
				crash "U64x2.with_lane: lane index out of range"
			} else {
				U64x2.from_u128_bits(simd128_with_lane(vector.to_u128_bits(), 64, index, value))
			}

		## Returns a vector with every lane set to the lane of this vector
		## at the given index. Crashes if the index is 2 or greater.
		##
		## Lowers to `pshufd` with a constant pattern on x86-64, `dup` (lane
		## form) on AArch64 NEON, and a constant `i8x16.shuffle` on wasm.
		broadcast_lane : U64x2, U64 -> U64x2
		broadcast_lane = |vector, index| U64x2.splat(U64x2.get_lane(vector, index))

		## Interleave the low lanes of the two vectors: result lanes are
		## `a0, b0`. With [U64x2.interleave_hi], this is the building block of
		## matrix transposes and of widening data.
		##
		## Lowers to `punpcklqdq` on x86-64, `zip1` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		interleave_lo : U64x2, U64x2 -> U64x2
		interleave_lo = |a, b| U64x2.from_u128_bits(simd128_interleave_lo(a.to_u128_bits(), b.to_u128_bits(), 64))

		## Interleave the high lanes of the two vectors: result lanes are
		## `a1, b1`.
		##
		## Lowers to `punpckhqdq` on x86-64, `zip2` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		interleave_hi : U64x2, U64x2 -> U64x2
		interleave_hi = |a, b| U64x2.from_u128_bits(simd128_interleave_hi(a.to_u128_bits(), b.to_u128_bits(), 64))

		## Returns the vector with its 2 lanes in reverse order.
		##
		## Lowers to `pshufd` with a constant pattern on x86-64, `ext` on
		## AArch64 NEON, and a constant `i8x16.shuffle` on wasm.
		reverse_lanes : U64x2 -> U64x2
		reverse_lanes = |vector| U64x2.from_u128_bits(simd128_reverse_lanes(vector.to_u128_bits(), 64))

		## Truncate each 64-bit lane of both vectors to its low 32 bits,
		## packing the four results into the 4 lanes of a [U32x4]: result
		## lanes are `a0, a1, b0, b1`.
		##
		## Lowers to a `shufps`-class shuffle on x86-64, `xtn` + `xtn2` on
		## AArch64 NEON, and an emulated shuffle on wasm.
		narrow_to_u32x4_wrap : U64x2, U64x2 -> U32x4
		narrow_to_u32x4_wrap = |a, b| {
			a_bits = a.to_u128_bits()
			b_bits = b.to_u128_bits()
			var $out = 0.U128
			var $i = 0.U64
			while $i < 2 {
				$out = simd128_with_lane($out, 32, $i, simd128_get_lane(a_bits, 64, $i))
				$out = simd128_with_lane($out, 32, 2 + $i, simd128_get_lane(b_bits, 64, $i))
				$i = $i + 1
			}
			U32x4.from_u128_bits($out)
		}

		## The wrapping sum of both lanes (mod 2^64). Both lanes widen to a
		## [U128] for the add so the intermediate cannot overflow, then wrap
		## back to 64 bits.
		##
		## Lowers to `paddq` + a shuffle on x86-64, `addp` (scalar `d`-form)
		## on AArch64 NEON, and an emulated sequence on wasm.
		## ```roc
		## expect U64x2.splat(10).sum_lanes_wrap() == 20
		## ```
		sum_lanes_wrap : U64x2 -> U64
		sum_lanes_wrap = |vector| {
			bits = vector.to_u128_bits()
			lane0 = simd128_get_lane(bits, 64, 0)
			lane1 = simd128_get_lane(bits, 64, 1)
			(lane0.to_u128() + lane1.to_u128()).to_u64_wrap()
		}

		## The carryless (XOR-accumulate, no carries) 128-bit product of lane
		## 0 of each input. The result vector's 128 bits are that product:
		## result lane 0 is its low 64 bits and result lane 1 its high 64
		## bits. This is the engine of CRC-32/CRC-64 folding and GHASH.
		##
		## Lowers to `pclmulqdq` (immediate `0x00`) on x86-64, `pmull`
		## (polynomial, crypto extension) on AArch64 NEON, and a software
		## sequence on wasm (no instruction).
		## ```roc
		## expect U64x2.splat(3).carryless_times_lo(U64x2.splat(5)).get_lane(0) == 15
		## ```
		carryless_times_lo : U64x2, U64x2 -> U64x2
		carryless_times_lo = |a, b| {
			a128 = simd128_get_lane(a.to_u128_bits(), 64, 0).to_u128()
			b_lane = simd128_get_lane(b.to_u128_bits(), 64, 0)
			var $acc = 0.U128
			var $i = 0.U64
			while $i < 64 {
				shift = U64.to_u8_wrap($i)
				if U64.bitwise_and(U64.shr_zf_wrap(b_lane, shift), 1) != 0 {
					$acc = U128.bitwise_xor($acc, U128.shl_wrap(a128, shift))
				} else {}
				$i = $i + 1
			}
			U64x2.from_u128_bits($acc)
		}

		## The carryless (XOR-accumulate, no carries) 128-bit product of lane
		## 1 of each input. The result vector's 128 bits are that product:
		## result lane 0 is its low 64 bits and result lane 1 its high 64
		## bits. This is the engine of CRC-32/CRC-64 folding and GHASH.
		##
		## Lowers to `pclmulqdq` (immediate `0x11`) on x86-64, `pmull2`
		## (polynomial, crypto extension) on AArch64 NEON, and a software
		## sequence on wasm (no instruction).
		carryless_times_hi : U64x2, U64x2 -> U64x2
		carryless_times_hi = |a, b| {
			a128 = simd128_get_lane(a.to_u128_bits(), 64, 1).to_u128()
			b_lane = simd128_get_lane(b.to_u128_bits(), 64, 1)
			var $acc = 0.U128
			var $i = 0.U64
			while $i < 64 {
				shift = U64.to_u8_wrap($i)
				if U64.bitwise_and(U64.shr_zf_wrap(b_lane, shift), 1) != 0 {
					$acc = U128.bitwise_xor($acc, U128.shl_wrap(a128, shift))
				} else {}
				$i = $i + 1
			}
			U64x2.from_u128_bits($acc)
		}

		## Read 16 bytes starting at the given byte index, as lanes in
		## little-endian order. Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`. Any alignment is fine.
		##
		## Lowers to `movdqu` on x86-64, `ldr` (Q register) on AArch64,
		## and `v128.load` on wasm.
		load : List(U8), U64 -> Try(U64x2, [OutOfBounds, ..])
		load = |bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(U64x2.from_u128_bits(simd128_from_bytes_at(bytes, index)))
			}
		}

		## Write these 16 bytes into the list starting at the given byte
		## index (in place when the list is unique), little-endian.
		## Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`.
		##
		## Lowers to `movdqu` (store form) on x86-64, `str` (Q register)
		## on AArch64, and `v128.store` on wasm.
		store : U64x2, List(U8), U64 -> Try(List(U8), [OutOfBounds, ..])
		store = |vector, bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(simd128_write_bytes_at(vector.to_u128_bits(), bytes, index))
			}
		}

		## Append these 16 bytes (little-endian) to the end of the list.
		append_to : U64x2, List(U8) -> List(U8)
		append_to = |vector, bytes| simd128_append_bytes(vector.to_u128_bits(), bytes)
	}

	## A 128-bit SIMD vector of 2 signed 64-bit lanes.
	##
	## Lane `i` occupies bits `[i * 64, (i + 1) * 64)` of the vector, and
	## the byte-serialized form (used by [I64x2.load], [I64x2.store], and
	## [I64x2.append_to]) is little-endian with lane 0 first. Every operation
	## has one pinned meaning that is bit-identical on every target; the
	## compiler lowers each operation to the best instruction sequence for
	## the target CPU (SSE/AVX on x86-64, NEON on AArch64, simd128 on wasm).
	I64x2 :: { bits : U128 }.{

		## Returns the [I64x2] with every lane `0`.
		## ```roc
		## expect I64x2.default() == I64x2.splat(0)
		## ```
		default : () -> I64x2
		default = || I64x2.splat(0)

		## Returns an [I64x2] with every lane set to the given [I64].
		##
		## Lowers to `pshufd` or `movddup` on x86-64, `dup` on AArch64 NEON,
		## and `i64x2.splat` on wasm.
		## ```roc
		## expect I64x2.splat(-1).get_lane(0) == -1
		## ```
		splat : I64 -> I64x2
		splat = |value| I64x2.from_u128_bits(simd128_splat(value.to_u64_wrap(), 64))

		## Build an [I64x2] from exactly 2 lane values, lane 0 first.
		## Returns `Err(WrongLength)` if the list's length is not 2.
		from_list : List(I64) -> Try(I64x2, [WrongLength, ..])
		from_list = |lanes|
			if List.len(lanes) != 2 {
				Err(WrongLength)
			} else {
				var $bits = 0.U128
				var $i = 0.U64
				while $i < 2 {
					$bits = simd128_with_lane($bits, 64, $i, list_get_unsafe(lanes, $i).to_u64_wrap())
					$i = $i + 1
				}
				Ok(I64x2.from_u128_bits($bits))
			}

		## The 2 lane values as a list, lane 0 first.
		## ```roc
		## expect I64x2.splat(7).to_list() == List.repeat(7.I64, 2)
		## ```
		to_list : I64x2 -> List(I64)
		to_list = |vector| {
			var $out = List.with_capacity(2)
			var $i = 0.U64
			while $i < 2 {
				$out = list_append_unsafe($out, I64x2.get_lane(vector, $i))
				$i = $i + 1
			}
			$out
		}

		## Returns `Bool.True` if all 128 bits of the two vectors are equal.
		## (For a per-lane comparison producing a mask, see [I64x2.eq_lanes].)
		##
		## Lowers to `pxor` + `ptest` on x86-64, `cmeq` + `uminv` on AArch64
		## NEON, and `v128.xor` + `v128.any_true` on wasm.
		is_eq : I64x2, I64x2 -> Bool
		is_eq = |a, b| a.to_u128_bits() == b.to_u128_bits()

		## Feed an [I64x2] into a [Hasher].
		to_hash : I64x2, Hasher -> Hasher
		to_hash = |vector, hasher| Hasher.write_u128(hasher, vector.to_u128_bits())

		## Render the lanes for debugging, e.g. `I64x2(-1, 2)`.
		to_inspect : I64x2 -> Str
		to_inspect = |vector| simd128_inspect("I64x2", List.map(I64x2.to_list(vector), I64.to_str))

		## The vector's 128 bits as a [U128]. Lane `i` occupies bits
		## `[i * 64, (i + 1) * 64)`. Free at runtime — no instructions.
		to_u128_bits : I64x2 -> U128
		to_u128_bits = |vector| vector.bits

		## Build an [I64x2] from 128 raw bits. Free at runtime — no
		## instructions.
		from_u128_bits : U128 -> I64x2
		from_u128_bits = |bits| I64x2.{ bits }

		## Reinterpret the same 128 bits as a [U8x16]. Free at runtime.
		to_u8x16_bits : I64x2 -> U8x16
		to_u8x16_bits = |vector| U8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I8x16]. Free at runtime.
		to_i8x16_bits : I64x2 -> I8x16
		to_i8x16_bits = |vector| I8x16.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U16x8]. Free at runtime.
		to_u16x8_bits : I64x2 -> U16x8
		to_u16x8_bits = |vector| U16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I16x8]. Free at runtime.
		to_i16x8_bits : I64x2 -> I16x8
		to_i16x8_bits = |vector| I16x8.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U32x4]. Free at runtime.
		to_u32x4_bits : I64x2 -> U32x4
		to_u32x4_bits = |vector| U32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as an [I32x4]. Free at runtime.
		to_i32x4_bits : I64x2 -> I32x4
		to_i32x4_bits = |vector| I32x4.from_u128_bits(vector.to_u128_bits())

		## Reinterpret the same 128 bits as a [U64x2]. Free at runtime.
		to_u64x2_bits : I64x2 -> U64x2
		to_u64x2_bits = |vector| U64x2.from_u128_bits(vector.to_u128_bits())

		## Add lane-wise, each lane wrapping mod 2^64 (two's complement). Each
		## lane widens to a [U128] for the add so the intermediate cannot
		## overflow, then wraps back to 64 bits.
		##
		## Lowers to `paddq` on x86-64, `add` (2×64-bit) on AArch64 NEON, and
		## `i64x2.add` on wasm.
		plus_wrap : I64x2, I64x2 -> I64x2
		plus_wrap = |a, b| I64x2.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 64, |x, y| (x.to_u128() + y.to_u128()).to_u64_wrap()))

		## Subtract lane-wise, each lane wrapping mod 2^64 (two's complement).
		## Each lane widens to a [U128] and borrows 2^64 before subtracting so
		## the intermediate stays non-negative, then wraps back to 64 bits.
		##
		## Lowers to `psubq` on x86-64, `sub` (2×64-bit) on AArch64 NEON, and
		## `i64x2.sub` on wasm.
		minus_wrap : I64x2, I64x2 -> I64x2
		minus_wrap = |a, b| I64x2.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 64, |x, y| ((x.to_u128() + 18446744073709551616) - y.to_u128()).to_u64_wrap()))

		## Negate each lane, wrapping mod 2^64 (two's complement). Negating 0
		## yields 0, and negating [I64.lowest] yields itself (its magnitude is
		## not representable), matching the scalar wrapping negation.
		##
		## Lowers to `psubq` from a zero register on x86-64, `neg` on AArch64
		## NEON, and `i64x2.neg` on wasm.
		## ```roc
		## expect I64x2.splat(5).negate_wrap().get_lane(0) == -5
		## ```
		negate_wrap : I64x2 -> I64x2
		negate_wrap = |vector| I64x2.from_u128_bits(simd128_map1(vector.to_u128_bits(), 64, |x| (18446744073709551616 - x.to_u128()).to_u64_wrap()))

		## Returns the bitwise AND of the two vectors' 128 bits.
		##
		## Lowers to `pand` on x86-64, `and` on AArch64 NEON, and
		## `v128.and` on wasm.
		bitwise_and : I64x2, I64x2 -> I64x2
		bitwise_and = |a, b| I64x2.from_u128_bits(U128.bitwise_and(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise OR of the two vectors' 128 bits.
		##
		## Lowers to `por` on x86-64, `orr` on AArch64 NEON, and `v128.or`
		## on wasm.
		bitwise_or : I64x2, I64x2 -> I64x2
		bitwise_or = |a, b| I64x2.from_u128_bits(U128.bitwise_or(a.to_u128_bits(), b.to_u128_bits()))

		## Returns the bitwise XOR of the two vectors' 128 bits.
		##
		## Lowers to `pxor` on x86-64, `eor` on AArch64 NEON, and
		## `v128.xor` on wasm.
		bitwise_xor : I64x2, I64x2 -> I64x2
		bitwise_xor = |a, b| I64x2.from_u128_bits(U128.bitwise_xor(a.to_u128_bits(), b.to_u128_bits()))

		## Flips every one of the vector's 128 bits.
		##
		## Lowers to `pxor` with all-ones on x86-64, `mvn` on AArch64 NEON,
		## and `v128.not` on wasm.
		bitwise_not : I64x2 -> I64x2
		bitwise_not = |vector| I64x2.from_u128_bits(U128.bitwise_not(vector.to_u128_bits()))

		## Bitwise select: for each of the 128 bits, take the bit from
		## `if_set` where this mask vector has a 1, and from `if_clear`
		## where it has a 0. Combined with the `_lanes` comparisons, this
		## is the branchless lane-wise `if`.
		##
		## Lowers to `pand`/`pandn`/`por` on x86-64, `bsl` on AArch64 NEON,
		## and `v128.bitselect` on wasm.
		bit_select : I64x2, I64x2, I64x2 -> I64x2
		bit_select = |mask, if_set, if_clear| {
			mask_bits = mask.to_u128_bits()
			kept = U128.bitwise_and(mask_bits, if_set.to_u128_bits())
			cleared = U128.bitwise_and(U128.bitwise_not(mask_bits), if_clear.to_u128_bits())
			I64x2.from_u128_bits(U128.bitwise_or(kept, cleared))
		}

		## Compare lane-wise for equality: each result lane is all-ones where
		## the lanes are equal and 0 where they differ.
		##
		## Lowers to `pcmpeqq` (SSE4.1) on x86-64, `cmeq` on AArch64 NEON, and
		## `i64x2.eq` on wasm.
		eq_lanes : I64x2, I64x2 -> I64x2
		eq_lanes = |a, b| I64x2.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 64, |x, y| if (x == y) U64.highest else 0))

		## Compare lane-wise: each result lane is all-ones where a's lane is
		## greater than b's (signed), else 0.
		##
		## Lowers to `pcmpgtq` (SSE4.2) on x86-64, `cmgt` on AArch64 NEON, and
		## `i64x2.gt_s` on wasm.
		gt_lanes : I64x2, I64x2 -> I64x2
		gt_lanes = |a, b| I64x2.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 64, |x, y| if (simd128_lane_to_signed(x, 64) > simd128_lane_to_signed(y, 64)) U64.highest else 0))

		## Compare lane-wise: each result lane is all-ones where a's lane is
		## less than b's (signed), else 0. See [I64x2.gt_lanes] for the
		## per-target lowerings.
		lt_lanes : I64x2, I64x2 -> I64x2
		lt_lanes = |a, b| I64x2.gt_lanes(b, a)

		## Compare lane-wise: each result lane is all-ones where a's lane is
		## greater than or equal to b's (signed), else 0.
		##
		## Lowers to `pcmpgtq` + `pcmpeqq` + `por` on x86-64, `cmge` on AArch64
		## NEON, and `i64x2.ge_s` on wasm.
		gte_lanes : I64x2, I64x2 -> I64x2
		gte_lanes = |a, b| I64x2.from_u128_bits(simd128_map2(a.to_u128_bits(), b.to_u128_bits(), 64, |x, y| if (simd128_lane_to_signed(x, 64) >= simd128_lane_to_signed(y, 64)) U64.highest else 0))

		## Compare lane-wise: each result lane is all-ones where a's lane is
		## less than or equal to b's (signed), else 0. See [I64x2.gte_lanes]
		## for the per-target lowerings.
		lte_lanes : I64x2, I64x2 -> I64x2
		lte_lanes = |a, b| I64x2.gte_lanes(b, a)

		## One bit per lane (lane 0 in bit 0): 1 where the lane's most
		## significant (sign) bit is set. On the all-0/all-1 masks produced
		## by the `_lanes` comparisons, this packs the results into a [U8] for
		## scalar decision-making.
		##
		## Lowers to `movmskpd` on x86-64, a short emulated sequence on AArch64
		## NEON (no single instruction), and `i64x2.bitmask` on wasm.
		## ```roc
		## expect I64x2.splat(-1).to_bitmask() == 3
		## ```
		to_bitmask : I64x2 -> U8
		to_bitmask = |vector| simd128_bitmask(vector.to_u128_bits(), 64).to_u8_wrap()

		## Returns `Bool.True` if any lane's most significant bit is set.
		## On comparison masks: "did any lane match?"
		any_lanes_set : I64x2 -> Bool
		any_lanes_set = |vector| I64x2.to_bitmask(vector) != 0

		## Returns `Bool.True` if every lane's most significant bit is set.
		## On comparison masks: "did all lanes match?"
		all_lanes_set : I64x2 -> Bool
		all_lanes_set = |vector| I64x2.to_bitmask(vector) == 3

		## Shift every lane's bits left by the same count. The count is
		## taken modulo 64: shifting by 64 leaves every lane unchanged and
		## shifting by 65 shifts by 1, matching [I64.shl_wrap].
		##
		## Lowers to `psllq` on x86-64 with the count masked to the lane
		## width first, `shl` on AArch64 NEON taking the pre-masked count,
		## and `i64x2.shl` on wasm, which masks the count natively.
		shl_wrap : I64x2, U8 -> I64x2
		shl_wrap = |vector, count| I64x2.from_u128_bits(simd128_shift_left(vector.to_u128_bits(), 64, count))

		## Shift every lane's bits right by the same count, replicating the
		## sign bit into the vacated high bits (arithmetic shift). The count
		## is taken modulo 64: shifting by 64 leaves every lane unchanged and
		## shifting by 65 shifts by 1, matching [I64.shr_wrap].
		##
		## x86-64 has no signed 64-bit lane shift below AVX-512, so this lowers
		## to a `psrlq` + sign-fixup sequence, with the count masked to the
		## lane width first; AArch64 NEON `sshr` takes the pre-masked count;
		## wasm `i64x2.shr_s` masks the count natively.
		shr_wrap : I64x2, U8 -> I64x2
		shr_wrap = |vector, count| I64x2.from_u128_bits(simd128_shift_right_arith(vector.to_u128_bits(), 64, count))

		## Shift every lane's bits right by the same count, filling the
		## vacated high bits with zeros. The count is taken modulo 64:
		## shifting by 64 leaves every lane unchanged and shifting by 65
		## shifts by 1, matching [I64.shr_zf_wrap].
		##
		## Lowers to `psrlq` on x86-64 with the count masked to the lane
		## width first, `ushr` on AArch64 NEON taking the pre-masked count,
		## and `i64x2.shr_u` on wasm, which masks the count natively.
		shr_zf_wrap : I64x2, U8 -> I64x2
		shr_zf_wrap = |vector, count| I64x2.from_u128_bits(simd128_shift_right_zf(vector.to_u128_bits(), 64, count))

		## The value of the lane at the given index. Crashes if the index
		## is 2 or greater. (Lane indices are expected to be compile-time
		## constants in practice.)
		##
		## Lowers to `pextrq` (SSE4.1) on x86-64, `smov` on AArch64 NEON, and
		## `i64x2.extract_lane` on wasm.
		get_lane : I64x2, U64 -> I64
		get_lane = |vector, index|
			if index >= 2 {
				crash "I64x2.get_lane: lane index out of range"
			} else {
				simd128_lane_to_signed(simd128_get_lane(vector.to_u128_bits(), 64, index), 64)
			}

		## Returns the vector with the lane at the given index replaced by
		## the given value. Crashes if the index is 2 or greater.
		##
		## Lowers to `pinsrq` (SSE4.1) on x86-64, `ins` on AArch64 NEON, and
		## `i64x2.replace_lane` on wasm.
		with_lane : I64x2, U64, I64 -> I64x2
		with_lane = |vector, index, value|
			if index >= 2 {
				crash "I64x2.with_lane: lane index out of range"
			} else {
				I64x2.from_u128_bits(simd128_with_lane(vector.to_u128_bits(), 64, index, value.to_u64_wrap()))
			}

		## Returns a vector with every lane set to the lane of this vector
		## at the given index. Crashes if the index is 2 or greater.
		##
		## Lowers to `pshufd` with a constant pattern on x86-64, `dup` (lane
		## form) on AArch64 NEON, and a constant `i8x16.shuffle` on wasm.
		broadcast_lane : I64x2, U64 -> I64x2
		broadcast_lane = |vector, index| I64x2.splat(I64x2.get_lane(vector, index))

		## Interleave the low lanes of the two vectors: result lanes are
		## `a0, b0`. With [I64x2.interleave_hi], this is the building block of
		## matrix transposes and of widening data.
		##
		## Lowers to `punpcklqdq` on x86-64, `zip1` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		interleave_lo : I64x2, I64x2 -> I64x2
		interleave_lo = |a, b| I64x2.from_u128_bits(simd128_interleave_lo(a.to_u128_bits(), b.to_u128_bits(), 64))

		## Interleave the high lanes of the two vectors: result lanes are
		## `a1, b1`.
		##
		## Lowers to `punpckhqdq` on x86-64, `zip2` on AArch64 NEON, and a
		## constant `i8x16.shuffle` on wasm.
		interleave_hi : I64x2, I64x2 -> I64x2
		interleave_hi = |a, b| I64x2.from_u128_bits(simd128_interleave_hi(a.to_u128_bits(), b.to_u128_bits(), 64))

		## Returns the vector with its 2 lanes in reverse order.
		##
		## Lowers to `pshufd` with a constant pattern on x86-64, `ext` on
		## AArch64 NEON, and a constant `i8x16.shuffle` on wasm.
		reverse_lanes : I64x2 -> I64x2
		reverse_lanes = |vector| I64x2.from_u128_bits(simd128_reverse_lanes(vector.to_u128_bits(), 64))

		## The wrapping sum of both lanes (mod 2^64, two's complement). Both
		## lanes widen to a [U128] for the add so the intermediate cannot
		## overflow, then wrap back to 64 signed bits.
		##
		## Lowers to `paddq` + a shuffle on x86-64, `addp` (scalar `d`-form)
		## on AArch64 NEON, and an emulated sequence on wasm.
		## ```roc
		## expect I64x2.splat(-3).sum_lanes_wrap() == -6
		## ```
		sum_lanes_wrap : I64x2 -> I64
		sum_lanes_wrap = |vector| {
			bits = vector.to_u128_bits()
			lane0 = simd128_get_lane(bits, 64, 0)
			lane1 = simd128_get_lane(bits, 64, 1)
			(lane0.to_u128() + lane1.to_u128()).to_u64_wrap().to_i64_wrap()
		}

		## Read 16 bytes starting at the given byte index, as lanes in
		## little-endian order. Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`. Any alignment is fine.
		##
		## Lowers to `movdqu` on x86-64, `ldr` (Q register) on AArch64,
		## and `v128.load` on wasm.
		load : List(U8), U64 -> Try(I64x2, [OutOfBounds, ..])
		load = |bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(I64x2.from_u128_bits(simd128_from_bytes_at(bytes, index)))
			}
		}

		## Write these 16 bytes into the list starting at the given byte
		## index (in place when the list is unique), little-endian.
		## Returns `Err(OutOfBounds)` unless
		## `index + 16 <= List.len(bytes)`.
		##
		## Lowers to `movdqu` (store form) on x86-64, `str` (Q register)
		## on AArch64, and `v128.store` on wasm.
		store : I64x2, List(U8), U64 -> Try(List(U8), [OutOfBounds, ..])
		store = |vector, bytes, index| {
			len = List.len(bytes)
			if index > len {
				Err(OutOfBounds)
			} else if len - index < 16 {
				Err(OutOfBounds)
			} else {
				Ok(simd128_write_bytes_at(vector.to_u128_bits(), bytes, index))
			}
		}

		## Append these 16 bytes (little-endian) to the end of the list.
		append_to : I64x2, List(U8) -> List(U8)
		append_to = |vector, bytes| simd128_append_bytes(vector.to_u128_bits(), bytes)
	}
}

# SIMD reference-implementation helpers. Each 128-bit vector value is its
# `bits : U128` field; lane `i` of width `lane_bits` occupies bits
# [i * lane_bits, (i + 1) * lane_bits), i.e. little-endian lane order.
# Lane values travel through these helpers as raw unsigned U64 bits; helpers
# mask results to the lane width, so callers get wrapping semantics for free.

simd128_lane_mask : U8 -> U128
simd128_lane_mask = |lane_bits|
	if lane_bits == 128 {
		U128.highest
	} else {
		U128.shl_wrap(1, lane_bits) - 1
	}

simd128_get_lane : U128, U8, U64 -> U64
simd128_get_lane = |bits, lane_bits, index| {
	shift = U64.to_u8_wrap(index * lane_bits.to_u64())
	U128.bitwise_and(U128.shr_zf_wrap(bits, shift), simd128_lane_mask(lane_bits)).to_u64_wrap()
}

simd128_with_lane : U128, U8, U64, U64 -> U128
simd128_with_lane = |bits, lane_bits, index, value| {
	shift = U64.to_u8_wrap(index * lane_bits.to_u64())
	mask = simd128_lane_mask(lane_bits)
	cleared = U128.bitwise_and(bits, U128.bitwise_not(U128.shl_wrap(mask, shift)))
	masked_value = U128.bitwise_and(value.to_u128(), mask)
	U128.bitwise_or(cleared, U128.shl_wrap(masked_value, shift))
}

simd128_lane_count : U8 -> U64
simd128_lane_count = |lane_bits| 128 / lane_bits.to_u64()

simd128_splat : U64, U8 -> U128
simd128_splat = |value, lane_bits| {
	lane_count = simd128_lane_count(lane_bits)
	var $out = 0.U128
	var $i = 0.U64
	while $i < lane_count {
		$out = simd128_with_lane($out, lane_bits, $i, value)
		$i = $i + 1
	}
	$out
}

simd128_map1 : U128, U8, (U64 -> U64) -> U128
simd128_map1 = |a, lane_bits, transform| {
	lane_count = simd128_lane_count(lane_bits)
	var $out = 0.U128
	var $i = 0.U64
	while $i < lane_count {
		$out = simd128_with_lane($out, lane_bits, $i, transform(simd128_get_lane(a, lane_bits, $i)))
		$i = $i + 1
	}
	$out
}

simd128_map2 : U128, U128, U8, (U64, U64 -> U64) -> U128
simd128_map2 = |a, b, lane_bits, transform| {
	lane_count = simd128_lane_count(lane_bits)
	var $out = 0.U128
	var $i = 0.U64
	while $i < lane_count {
		lane_a = simd128_get_lane(a, lane_bits, $i)
		lane_b = simd128_get_lane(b, lane_bits, $i)
		$out = simd128_with_lane($out, lane_bits, $i, transform(lane_a, lane_b))
		$i = $i + 1
	}
	$out
}

# Sign-extend the low `lane_bits` bits of a raw lane value into an I64.
simd128_lane_to_signed : U64, U8 -> I64
simd128_lane_to_signed = |lane, lane_bits|
	if lane_bits == 64 {
		lane.to_i64_wrap()
	} else {
		sign_bit = U64.shl_wrap(1, lane_bits - 1)
		if U64.bitwise_and(lane, sign_bit) == 0 {
			lane.to_i64_wrap()
		} else {
			U64.bitwise_or(lane, U64.bitwise_not(U64.shl_wrap(1, lane_bits) - 1)).to_i64_wrap()
		}
	}

# The lowest value representable by a signed lane, as an I64.
simd128_signed_lowest : U8 -> I64
simd128_signed_lowest = |lane_bits|
	if lane_bits == 64 {
		I64.lowest
	} else {
		0 - I64.shl_wrap(1, lane_bits - 1)
	}

# The highest value representable by a signed lane, as an I64.
simd128_signed_highest : U8 -> I64
simd128_signed_highest = |lane_bits|
	if lane_bits == 64 {
		I64.highest
	} else {
		I64.shl_wrap(1, lane_bits - 1) - 1
	}

# Clamp a signed intermediate value into the signed lane range and return its
# raw lane bits. Only valid for lane widths up to 32 when the intermediate
# can exceed the I64 range is impossible (callers keep intermediates in I64).
simd128_clamp_signed : I64, U8 -> U64
simd128_clamp_signed = |value, lane_bits|
	I64.max(simd128_signed_lowest(lane_bits), I64.min(simd128_signed_highest(lane_bits), value)).to_u64_wrap()

# Clamp a signed intermediate value into [0, 2^lane_bits - 1] and return its
# raw lane bits. Only valid for lane widths up to 32.
simd128_clamp_to_unsigned : I64, U8 -> U64
simd128_clamp_to_unsigned = |value, lane_bits|
	if value < 0 {
		0
	} else {
		U64.min(value.to_u64_wrap(), U64.shl_wrap(1, lane_bits) - 1)
	}

# Interleave the low-half lanes of two vectors: result lane 2i is a's lane i,
# result lane 2i+1 is b's lane i, for i in [0, lane_count/2).
simd128_interleave_lo : U128, U128, U8 -> U128
simd128_interleave_lo = |a, b, lane_bits| {
	half = simd128_lane_count(lane_bits) / 2
	var $out = 0.U128
	var $i = 0.U64
	while $i < half {
		$out = simd128_with_lane($out, lane_bits, $i * 2, simd128_get_lane(a, lane_bits, $i))
		$out = simd128_with_lane($out, lane_bits, $i * 2 + 1, simd128_get_lane(b, lane_bits, $i))
		$i = $i + 1
	}
	$out
}

# Interleave the high-half lanes of two vectors: result lane 2i is a's lane
# (half + i), result lane 2i+1 is b's lane (half + i).
simd128_interleave_hi : U128, U128, U8 -> U128
simd128_interleave_hi = |a, b, lane_bits| {
	half = simd128_lane_count(lane_bits) / 2
	var $out = 0.U128
	var $i = 0.U64
	while $i < half {
		$out = simd128_with_lane($out, lane_bits, $i * 2, simd128_get_lane(a, lane_bits, half + $i))
		$out = simd128_with_lane($out, lane_bits, $i * 2 + 1, simd128_get_lane(b, lane_bits, half + $i))
		$i = $i + 1
	}
	$out
}

# The even-indexed lanes of a followed by the even-indexed lanes of b.
simd128_even_lanes : U128, U128, U8 -> U128
simd128_even_lanes = |a, b, lane_bits| {
	half = simd128_lane_count(lane_bits) / 2
	var $out = 0.U128
	var $i = 0.U64
	while $i < half {
		$out = simd128_with_lane($out, lane_bits, $i, simd128_get_lane(a, lane_bits, $i * 2))
		$out = simd128_with_lane($out, lane_bits, half + $i, simd128_get_lane(b, lane_bits, $i * 2))
		$i = $i + 1
	}
	$out
}

# The odd-indexed lanes of a followed by the odd-indexed lanes of b.
simd128_odd_lanes : U128, U128, U8 -> U128
simd128_odd_lanes = |a, b, lane_bits| {
	half = simd128_lane_count(lane_bits) / 2
	var $out = 0.U128
	var $i = 0.U64
	while $i < half {
		$out = simd128_with_lane($out, lane_bits, $i, simd128_get_lane(a, lane_bits, $i * 2 + 1))
		$out = simd128_with_lane($out, lane_bits, half + $i, simd128_get_lane(b, lane_bits, $i * 2 + 1))
		$i = $i + 1
	}
	$out
}

# Reverse the order of the lanes.
simd128_reverse_lanes : U128, U8 -> U128
simd128_reverse_lanes = |a, lane_bits| {
	lane_count = simd128_lane_count(lane_bits)
	var $out = 0.U128
	var $i = 0.U64
	while $i < lane_count {
		$out = simd128_with_lane($out, lane_bits, $i, simd128_get_lane(a, lane_bits, lane_count - 1 - $i))
		$i = $i + 1
	}
	$out
}

# One bit per lane (lane 0 in bit 0): 1 when the lane's most significant bit
# is set.
simd128_bitmask : U128, U8 -> U64
simd128_bitmask = |a, lane_bits| {
	lane_count = simd128_lane_count(lane_bits)
	sign_bit = U64.shl_wrap(1, lane_bits - 1)
	var $mask = 0.U64
	var $i = 0.U64
	while $i < lane_count {
		if U64.bitwise_and(simd128_get_lane(a, lane_bits, $i), sign_bit) != 0 {
			$mask = U64.bitwise_or($mask, U64.shl_wrap(1, U64.to_u8_wrap($i)))
		} else {}
		$i = $i + 1
	}
	$mask
}

# Uniform lane-wise shifts where the count is taken modulo the lane width, so a
# count equal to the lane width leaves every lane unchanged and larger counts
# wrap around. This matches the scalar shl_wrap family.
simd128_shift_left : U128, U8, U8 -> U128
simd128_shift_left = |a, lane_bits, count| {
	effective = count % lane_bits
	simd128_map1(a, lane_bits, |lane| U64.shl_wrap(lane, effective))
}

simd128_shift_right_zf : U128, U8, U8 -> U128
simd128_shift_right_zf = |a, lane_bits, count| {
	effective = count % lane_bits
	simd128_map1(a, lane_bits, |lane| U64.shr_zf_wrap(lane, effective))
}

simd128_shift_right_arith : U128, U8, U8 -> U128
simd128_shift_right_arith = |a, lane_bits, count| {
	effective = count % lane_bits
	simd128_map1(a, lane_bits, |lane| I64.shr_wrap(simd128_lane_to_signed(lane, lane_bits), effective).to_u64_wrap())
}

# Read 16 bytes starting at byte `index` as a little-endian 128-bit value.
# The caller must already have checked that index + 16 <= List.len(bytes).
simd128_from_bytes_at : List(U8), U64 -> U128
simd128_from_bytes_at = |bytes, index| {
	var $bits = 0.U128
	var $i = 0.U64
	while $i < 16 {
		byte = u8_list_get_unsafe(bytes, index + $i)
		$bits = U128.bitwise_or($bits, U128.shl_wrap(byte.to_u128(), U64.to_u8_wrap($i * 8)))
		$i = $i + 1
	}
	$bits
}

# Write 16 bytes (little-endian) starting at byte `index`. The caller must
# already have checked that index + 16 <= List.len(bytes).
simd128_write_bytes_at : U128, List(U8), U64 -> List(U8)
simd128_write_bytes_at = |bits, bytes, index| {
	var $out = bytes
	var $i = 0.U64
	while $i < 16 {
		byte = simd128_get_lane(bits, 8, $i).to_u8_wrap()
		$out = list_set_unsafe($out, index + $i, byte)
		$i = $i + 1
	}
	$out
}

# Append the 16 bytes (little-endian) to the end of the list.
simd128_append_bytes : U128, List(U8) -> List(U8)
simd128_append_bytes = |bits, bytes| {
	var $out = List.reserve(bytes, 16)
	var $i = 0.U64
	while $i < 16 {
		$out = u8_list_append_unsafe($out, simd128_get_lane(bits, 8, $i).to_u8_wrap())
		$i = $i + 1
	}
	$out
}

# Render lane values as e.g. "U8x16(1, 2, 3, ...)" for to_inspect.
simd128_inspect : Str, List(Str) -> Str
simd128_inspect = |type_name, lane_strs| {
	var $out = Str.concat(type_name, "(")
	var $first = Bool.True
	for lane_str in lane_strs {
		$out = if $first {
			Str.concat($out, lane_str)
		} else {
			Str.concat($out, Str.concat(", ", lane_str))
		}
		$first = Bool.False
	}
	Str.concat($out, ")")
}

# These names mirror the private Builtin helpers used by the original
# implementation. The oracle intentionally implements them with the public,
# bounds-checking List API so it does not share compiler low-levels with the
# implementation under test.
list_get_unsafe : List(a), U64 -> a
list_get_unsafe = |items, index|
	match List.get(items, index) {
		Ok(value) => value
		Err(OutOfBounds) => {
			crash "SIMD oracle list index invariant violated"
		}
	}

u8_list_get_unsafe : List(U8), U64 -> U8
u8_list_get_unsafe = |items, index| list_get_unsafe(items, index)

list_append_unsafe : List(a), a -> List(a)
list_append_unsafe = |items, value| List.append(items, value)

u8_list_append_unsafe : List(U8), U8 -> List(U8)
u8_list_append_unsafe = |items, value| List.append(items, value)

list_set_unsafe : List(a), U64, a -> List(a)
list_set_unsafe = |items, index, value|
	match List.set(items, index, value) {
		Ok(updated) => updated
		Err(OutOfBounds) => {
			crash "SIMD oracle list index invariant violated"
		}
	}
