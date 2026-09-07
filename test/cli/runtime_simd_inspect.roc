app [main!] { pf: platform "../fx-open/platform/main.roc" }

Vec := U8x16
Outer := Vec

main! = |args| {
	lane = args.len().to_u8_wrap()
	v = U8x16.default().with_lane(1, lane)
	dbg Outer.(Vec.(v))
	dbg v
	dbg Str.inspect(v)
	inspect = |value| Str.inspect(value)
	if Str.inspect(v) != U8x16.to_inspect(v) {
		Err(IncorrectU8x16Inspection)
	} else if inspect(Outer.(Vec.(v))) != U8x16.to_inspect(v) {
		Err(IncorrectNominalInspection)
	} else if inspect({ vector: [v] }) != "{ vector: [${U8x16.to_inspect(v)}] }" {
		Err(IncorrectNestedInspection)
	} else if inspect({ vector: v, words: ["ok"] }) != "{ vector: ${U8x16.to_inspect(v)}, words: [\"ok\"] }" {
		Err(IncorrectRecordInspection)
	} else if inspect((v, ["ok"])) != "(${U8x16.to_inspect(v)}, [\"ok\"])" {
		Err(IncorrectTupleInspection)
	} else if inspect(Wrapped(v)) != "Wrapped(${U8x16.to_inspect(v)})" {
		Err(IncorrectTagInspection)
	} else if inspect(I8x16.splat(lane.to_i8_wrap())) != I8x16.to_inspect(I8x16.splat(lane.to_i8_wrap())) {
		Err(IncorrectI8x16Inspection)
	} else if inspect(U16x8.splat(lane.to_u16())) != U16x8.to_inspect(U16x8.splat(lane.to_u16())) {
		Err(IncorrectU16x8Inspection)
	} else if inspect(I16x8.splat(lane.to_i16())) != I16x8.to_inspect(I16x8.splat(lane.to_i16())) {
		Err(IncorrectI16x8Inspection)
	} else if inspect(U32x4.splat(lane.to_u32())) != U32x4.to_inspect(U32x4.splat(lane.to_u32())) {
		Err(IncorrectU32x4Inspection)
	} else if inspect(I32x4.splat(lane.to_i32())) != I32x4.to_inspect(I32x4.splat(lane.to_i32())) {
		Err(IncorrectI32x4Inspection)
	} else if inspect(U64x2.splat(lane.to_u64())) != U64x2.to_inspect(U64x2.splat(lane.to_u64())) {
		Err(IncorrectU64x2Inspection)
	} else if inspect(I64x2.splat(lane.to_i64())) != I64x2.to_inspect(I64x2.splat(lane.to_i64())) {
		Err(IncorrectI64x2Inspection)
	} else {
		Ok({})
	}
}

expect Str.inspect(U8x16.default()) == "U8x16(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)"
