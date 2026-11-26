app [main!] { pf: platform "./static-lib-platform/main.roc" }

# WebAssembly 1.0 has no vector instructions, so every operation here must
# lower to scalar code on wasm32v1. The host's seed is 0, which LLVM cannot
# know (Binaryen's size pipeline can, after linking).
main! = |seed| {
	bytes = U8x16.splat((seed + 200).to_u8_wrap()).with_lane(3, (seed + 1).to_u8_wrap())
	halves = U16x8.splat((seed + 300).to_u16_wrap())
	# Lane 0's index is out of range, so it reads 0; every other lane reads lane 3.
	indices = U8x16.splat((seed + 3).to_u8_wrap()).with_lane(0, (seed + 16).to_u8_wrap())
	setting = match Str.concat("width=", (seed + 16).to_str()) {
		"${key}=${value}" => "${key} is ${value}"
		_ => "no setting"
	}
	Str.inspect((bytes.plus_wrap(bytes), bytes.plus_saturated(bytes), halves.times_wrap(halves), bytes.get_lane(3), bytes.table_lookup(indices), setting))
}
