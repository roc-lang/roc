app [main!] { pf: platform "../fx-open/platform/main.roc" }

render : a -> Str
render = |value| Str.inspect(value)

Vector := U64x2

main! = |args| {
	zero = args.len() - args.len()
	u8x16 = U8x16.default().with_lane(15, zero.to_u8_wrap() + 1)
	if render(u8x16) != "U8x16(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)" { crash "U8x16 generic inspect mismatch" }
	i8x16 = I8x16.default().with_lane(15, zero.to_i8_wrap() + 1)
	if render(i8x16) != "I8x16(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)" { crash "I8x16 generic inspect mismatch" }
	u16x8 = U16x8.default().with_lane(7, zero.to_u16_wrap() + 1)
	if render(u16x8) != "U16x8(0, 0, 0, 0, 0, 0, 0, 1)" { crash "U16x8 generic inspect mismatch" }
	i16x8 = I16x8.default().with_lane(7, zero.to_i16_wrap() + 1)
	if render(i16x8) != "I16x8(0, 0, 0, 0, 0, 0, 0, 1)" { crash "I16x8 generic inspect mismatch" }
	u32x4 = U32x4.default().with_lane(3, zero.to_u32_wrap() + 1)
	if render(u32x4) != "U32x4(0, 0, 0, 1)" { crash "U32x4 generic inspect mismatch" }
	i32x4 = I32x4.default().with_lane(3, zero.to_i32_wrap() + 1)
	if render(i32x4) != "I32x4(0, 0, 0, 1)" { crash "I32x4 generic inspect mismatch" }
	u64x2 = U64x2.default().with_lane(1, zero.to_u64_wrap() + 1)
	if render(u64x2) != "U64x2(0, 1)" { crash "U64x2 generic inspect mismatch" }
	i64x2 = I64x2.default().with_lane(1, zero.to_i64_wrap() + 1)
	if render(i64x2) != "I64x2(0, 1)" { crash "I64x2 generic inspect mismatch" }
	wrapped = Vector.(u64x2)
	if Str.inspect(wrapped) != "U64x2(0, 1)" { crash "nominal backing inspect mismatch" }
	if render({ vectors: [i64x2], boxed: Box.box(u64x2) }) != "{ boxed: Box(U64x2(0, 1)), vectors: [I64x2(0, 1)] }" { crash "nested inspect mismatch" }
	Ok({})
}
