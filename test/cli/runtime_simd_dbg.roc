app [main!] { pf: platform "../fx-open/platform/main.roc" }

main! = |args| {
	lane = args.len().to_u8_wrap()
	dbg U8x16.from_lanes(lane, lane, lane, lane, lane, lane, lane, lane, lane, lane, lane, lane, lane, lane, lane, lane)
	dbg I8x16.from_lanes(lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap(), lane.to_i8_wrap())
	dbg U16x8.from_lanes(lane.to_u16(), lane.to_u16(), lane.to_u16(), lane.to_u16(), lane.to_u16(), lane.to_u16(), lane.to_u16(), lane.to_u16())
	dbg I16x8.from_lanes(lane.to_i16(), lane.to_i16(), lane.to_i16(), lane.to_i16(), lane.to_i16(), lane.to_i16(), lane.to_i16(), lane.to_i16())
	dbg U32x4.from_lanes(lane.to_u32(), lane.to_u32(), lane.to_u32(), lane.to_u32())
	dbg I32x4.from_lanes(lane.to_i32(), lane.to_i32(), lane.to_i32(), lane.to_i32())
	dbg U64x2.from_lanes(lane.to_u64(), lane.to_u64())
	dbg I64x2.from_lanes(lane.to_i64(), lane.to_i64())
	Ok({})
}
