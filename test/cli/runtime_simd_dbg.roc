app [main!] { pf: platform "../fx-open/platform/main.roc" }

main! = |args| {
	lane = args.len().to_u8_wrap()
	dbg U8x16.splat(lane)
	dbg I8x16.splat(lane.to_i8_wrap())
	dbg U16x8.splat(lane.to_u16())
	dbg I16x8.splat(lane.to_i16())
	dbg U32x4.splat(lane.to_u32())
	dbg I32x4.splat(lane.to_i32())
	dbg U64x2.splat(lane.to_u64())
	dbg I64x2.splat(lane.to_i64())
	Ok({})
}
