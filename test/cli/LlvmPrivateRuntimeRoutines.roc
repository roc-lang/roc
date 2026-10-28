# Operations LLVM's code generator lowers to calls rather than instructions,
# each on a value only known at runtime so none is folded at compile time:
# large copies, moves and fills (`memcpy`, `memmove`, `memset`, and `bzero`
# for a zero fill on Darwin), float remainders (`fmod`, `fmodf`), rounding on a
# CPU without rounding instructions (`floor`, `ceil`, `trunc` and their F32
# forms), floating point on MSVC (`_fltused`), and on Windows a stack frame
# larger than a page, which is probed through `__chkstk` or `___chkstk_ms`.
# Wide and 64-bit integer arithmetic and conversions are here too, so a target
# that lowers them to compiler-rt helpers calls those as well.

Row8 : { a : F64, b : F64, c : F64, d : F64, e : F64, f : F64, g : F64, h : F64 }
Row64 : { a : Row8, b : Row8, c : Row8, d : Row8, e : Row8, f : Row8, g : Row8, h : Row8 }
Row512 : { a : Row64, b : Row64, c : Row64, d : Row64, e : Row64, f : Row64, g : Row64, h : Row64 }
Page2 : { a : Row512, b : Row512 }

row8 : F64 -> Row8
row8 = |x| { a: x, b: x + 1, c: x + 2, d: x + 3, e: x + 4, f: x + 5, g: x + 6, h: x + 7 }

row64 : F64 -> Row64
row64 = |x| { a: row8(x), b: row8(x + 8), c: row8(x + 16), d: row8(x + 24), e: row8(x + 32), f: row8(x + 40), g: row8(x + 48), h: row8(x + 56) }

row512 : F64 -> Row512
row512 = |x| { a: row64(x), b: row64(x + 64), c: row64(x + 128), d: row64(x + 192), e: row64(x + 256), f: row64(x + 320), g: row64(x + 384), h: row64(x + 448) }

page2 : F64 -> Page2
page2 = |x| { a: row512(x), b: row512(x + 512) }

main! = |args| {
	n = args.len()
	f64 = n.to_f64() * 1.75
	f32 = n.to_f32() * 1.75.F32
	i128 = n.to_i128() * 1000000000000000000000 - 7
	u128 = n.to_u128() * 1000000000000000000000 + 7
	i64 = n.to_i64_wrap() * 3000000000 - 5
	u64 = n.to_u64() * 3000000000 + 5
	dec = n.to_dec() * 1.5

	float_line = Str.join_with(
		[
			(f64 % 0.75).to_str(),
			(f32 % 0.75.F32).to_str(),
			(f64.floor_to_i64_try() ?? 0).to_str(),
			(f64.ceiling_to_i64_try() ?? 0).to_str(),
			(f64.round_to_i64_try() ?? 0).to_str(),
			(f32.floor_to_i32_try() ?? 0).to_str(),
			(f32.ceiling_to_i32_try() ?? 0).to_str(),
			(f32.round_to_i32_try() ?? 0).to_str(),
			f64.sqrt().to_str(),
			f32.sqrt().to_str(),
			f64.div_trunc_by(0.5).to_str(),
		],
		" ",
	)

	wide_line = Str.join_with(
		[
			(i128 * (n.to_i128() + 2)).to_str(),
			(i128 // (n.to_i128() + 3)).to_str(),
			(i128 % (n.to_i128() + 3)).to_str(),
			(u128 // (n.to_u128() + 3)).to_str(),
			(u128 % (n.to_u128() + 3)).to_str(),
			(i128.times_try(i128) ?? 0).to_str(),
			i128.shl_wrap(n.to_u8_wrap()).to_str(),
			u128.shr_zf_wrap(n.to_u8_wrap()).to_str(),
			i128.to_f64().to_str(),
			u128.to_f32().to_str(),
			(f64.to_i128_try() ?? 0).to_str(),
			(f32.to_u128_try() ?? 0).to_str(),
		],
		" ",
	)

	narrow_line = Str.join_with(
		[
			(i64 // (n.to_i64_wrap() + 3)).to_str(),
			(u64 % (n.to_u64() + 3)).to_str(),
			(i64.times_try(i64) ?? 0).to_str(),
			u64.to_f64().to_str(),
			i64.to_f32().to_str(),
			(f64.to_i64_try() ?? 0).to_str(),
			(f32.to_u64_try() ?? 0).to_str(),
		],
		" ",
	)

	dec_line = Str.join_with([(dec * dec).to_str(), (dec / (n.to_dec() + 3)).to_str(), dec.to_f64().to_str()], " ")

	pages = List.repeat(page2(f64), n)
	rows = List.repeat(row8(f64), n * 1000)
	moved = rows.drop_first(n).concat(rows)
	# Dropping from, prepending to, and dropping from the middle of a list
	# nothing else holds shift its items in place, an overlapping move.
	shifted = List.repeat(row8(f64), n * 1000 + 2).drop_first(n + 1)
	prepended = List.repeat(row8(f64), n * 1000 + 2).prepend(row8(f64 + 1))
	dropped = List.repeat(row8(f64), n * 1000 + 2).drop_at(n)
	zeroed = List.repeat(0.U8, n * 4096)
	sizes_line = Str.join_with([moved.len().to_str(), shifted.len().to_str(), prepended.len().to_str(), dropped.len().to_str(), zeroed.len().to_str(), (pages.first() ?? page2(0)).b.h.h.h.to_str()], " ")

	echo!(Str.join_with([float_line, wide_line, narrow_line, dec_line, sizes_line], "\n"))
	Ok({})
}
