app [main!] { pf: platform "../fx-open/platform/main.roc" }

show : a -> {}
show = |value| {
	dbg value
	{}
}

main! = |args| {
	zero = args.len() - args.len()
	dbg U8x16.default().with_lane(15, zero.to_u8_wrap() + 1)
	dbg I8x16.default().with_lane(15, zero.to_i8_wrap() + 1)
	dbg U16x8.default().with_lane(7, zero.to_u16_wrap() + 1)
	dbg I16x8.default().with_lane(7, zero.to_i16_wrap() + 1)
	dbg U32x4.default().with_lane(3, zero.to_u32_wrap() + 1)
	dbg I32x4.default().with_lane(3, zero.to_i32_wrap() + 1)
	dbg U64x2.default().with_lane(1, zero.to_u64_wrap() + 1)
	dbg I64x2.default().with_lane(1, zero.to_i64_wrap() + 1)
	show((U64x2.default().with_lane(1, zero + 1), I64x2.splat(-1)))
	Ok({})
}
