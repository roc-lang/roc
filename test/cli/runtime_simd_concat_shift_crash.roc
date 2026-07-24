app [main!] { pf: platform "../fx-open/platform/main.roc" }

main! = |args| {
	count = args.len().to_u8_wrap() + 17
	_ = U8x16.concat_shift_bytes(U8x16.splat(1), U8x16.splat(2), count).to_u128_bits()
	Ok({})
}
