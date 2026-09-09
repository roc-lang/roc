app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

run! : Str => Str
run! = |input| {
	seed = input.count_utf8_bytes()
	u8x16 = U8x16.splat(seed.to_u8_wrap())
	u8x16_before = Host.alloc_count!()
	u8x16_text = Str.inspect(u8x16)
	u8x16_allocs = Host.alloc_count!() - u8x16_before
	expect u8x16_allocs == 1
	expect u8x16_text == "U8x16(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16)"
	i8x16 = I8x16.splat(seed.to_i8_wrap())
	i8x16_before = Host.alloc_count!()
	i8x16_text = Str.inspect(i8x16)
	i8x16_allocs = Host.alloc_count!() - i8x16_before
	expect i8x16_allocs == 1
	expect i8x16_text == "I8x16(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16)"
	u16x8 = U16x8.splat(seed.to_u16_wrap())
	u16x8_before = Host.alloc_count!()
	u16x8_text = Str.inspect(u16x8)
	u16x8_allocs = Host.alloc_count!() - u16x8_before
	expect u16x8_allocs == 1
	expect u16x8_text == "U16x8(16, 16, 16, 16, 16, 16, 16, 16)"
	i16x8 = I16x8.splat(seed.to_i16_wrap())
	i16x8_before = Host.alloc_count!()
	i16x8_text = Str.inspect(i16x8)
	i16x8_allocs = Host.alloc_count!() - i16x8_before
	expect i16x8_allocs == 1
	expect i16x8_text == "I16x8(16, 16, 16, 16, 16, 16, 16, 16)"
	u32x4 = U32x4.splat(seed.to_u32_wrap())
	u32x4_before = Host.alloc_count!()
	u32x4_text = Str.inspect(u32x4)
	u32x4_allocs = Host.alloc_count!() - u32x4_before
	expect u32x4_allocs == 0
	expect u32x4_text == "U32x4(16, 16, 16, 16)"
	i32x4 = I32x4.splat(seed.to_i32_wrap())
	i32x4_before = Host.alloc_count!()
	i32x4_text = Str.inspect(i32x4)
	i32x4_allocs = Host.alloc_count!() - i32x4_before
	expect i32x4_allocs == 0
	expect i32x4_text == "I32x4(16, 16, 16, 16)"
	u64x2 = U64x2.splat(seed.to_u64_wrap())
	u64x2_before = Host.alloc_count!()
	u64x2_text = Str.inspect(u64x2)
	u64x2_allocs = Host.alloc_count!() - u64x2_before
	expect u64x2_allocs == 0
	expect u64x2_text == "U64x2(16, 16)"
	i64x2 = I64x2.splat(seed.to_i64_wrap())
	i64x2_before = Host.alloc_count!()
	i64x2_text = Str.inspect(i64x2)
	i64x2_allocs = Host.alloc_count!() - i64x2_before
	expect i64x2_allocs == 0
	expect i64x2_text == "I64x2(16, 16)"
	# Long results reserve their exact final size; short ones stay inline.
	u32x4_wide = U32x4.splat(U32.highest).with_lane(0, seed.to_u32_wrap())
	u32x4_wide_before = Host.alloc_count!()
	u32x4_wide_text = Str.inspect(u32x4_wide)
	u32x4_wide_allocs = Host.alloc_count!() - u32x4_wide_before
	expect u32x4_wide_allocs == 1
	expect u32x4_wide_text == "U32x4(16, 4294967295, 4294967295, 4294967295)"
	i32x4_wide = I32x4.splat(I32.lowest).with_lane(0, seed.to_i32_wrap())
	i32x4_wide_before = Host.alloc_count!()
	i32x4_wide_text = Str.inspect(i32x4_wide)
	i32x4_wide_allocs = Host.alloc_count!() - i32x4_wide_before
	expect i32x4_wide_allocs == 1
	expect i32x4_wide_text == "I32x4(16, -2147483648, -2147483648, -2147483648)"
	u64x2_wide = U64x2.splat(U64.highest).with_lane(0, seed.to_u64_wrap())
	u64x2_wide_before = Host.alloc_count!()
	u64x2_wide_text = Str.inspect(u64x2_wide)
	u64x2_wide_allocs = Host.alloc_count!() - u64x2_wide_before
	expect u64x2_wide_allocs == 1
	expect u64x2_wide_text == "U64x2(16, 18446744073709551615)"
	i64x2_wide = I64x2.splat(I64.lowest).with_lane(0, seed.to_i64_wrap())
	i64x2_wide_before = Host.alloc_count!()
	i64x2_wide_text = Str.inspect(i64x2_wide)
	i64x2_wide_allocs = Host.alloc_count!() - i64x2_wide_before
	expect i64x2_wide_allocs == 1
	expect i64x2_wide_text == "I64x2(16, -9223372036854775808)"
	"SIMD inspection allocations: ${u8x16_allocs.to_str()} ${i8x16_allocs.to_str()} ${u16x8_allocs.to_str()} ${i16x8_allocs.to_str()} ${u32x4_allocs.to_str()} ${i32x4_allocs.to_str()} ${u64x2_allocs.to_str()} ${i64x2_allocs.to_str()}"
}
