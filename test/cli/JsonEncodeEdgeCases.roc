JsonEncodeEdgeCases :: [].{}

round_trip_u8 : U8 -> Bool
round_trip_u8 = |value| {
	encoded = Json.to_str(value)

	parsed : Try(U8, [InvalidJson(Str)])
	parsed = Json.parse(encoded)
	parsed == Ok(value)
}

round_trip_i8 : I8 -> Bool
round_trip_i8 = |value| {
	encoded = Json.to_str(value)

	parsed : Try(I8, [InvalidJson(Str)])
	parsed = Json.parse(encoded)
	parsed == Ok(value)
}

round_trip_u16 : U16 -> Bool
round_trip_u16 = |value| {
	encoded = Json.to_str(value)

	parsed : Try(U16, [InvalidJson(Str)])
	parsed = Json.parse(encoded)
	parsed == Ok(value)
}

round_trip_i16 : I16 -> Bool
round_trip_i16 = |value| {
	encoded = Json.to_str(value)

	parsed : Try(I16, [InvalidJson(Str)])
	parsed = Json.parse(encoded)
	parsed == Ok(value)
}

round_trip_u32 : U32 -> Bool
round_trip_u32 = |value| {
	encoded = Json.to_str(value)

	parsed : Try(U32, [InvalidJson(Str)])
	parsed = Json.parse(encoded)
	parsed == Ok(value)
}

round_trip_i32 : I32 -> Bool
round_trip_i32 = |value| {
	encoded = Json.to_str(value)

	parsed : Try(I32, [InvalidJson(Str)])
	parsed = Json.parse(encoded)
	parsed == Ok(value)
}

round_trip_u64 : U64 -> Bool
round_trip_u64 = |value| {
	encoded = Json.to_str(value)

	parsed : Try(U64, [InvalidJson(Str)])
	parsed = Json.parse(encoded)
	parsed == Ok(value)
}

round_trip_i64 : I64 -> Bool
round_trip_i64 = |value| {
	encoded = Json.to_str(value)

	parsed : Try(I64, [InvalidJson(Str)])
	parsed = Json.parse(encoded)
	parsed == Ok(value)
}

round_trip_u128 : U128 -> Bool
round_trip_u128 = |value| {
	encoded = Json.to_str(value)

	parsed : Try(U128, [InvalidJson(Str)])
	parsed = Json.parse(encoded)
	parsed == Ok(value)
}

round_trip_i128 : I128 -> Bool
round_trip_i128 = |value| {
	encoded = Json.to_str(value)

	parsed : Try(I128, [InvalidJson(Str)])
	parsed = Json.parse(encoded)
	parsed == Ok(value)
}

expect round_trip_u8(U8.lowest) and round_trip_u8(U8.highest)
expect round_trip_i8(I8.lowest) and round_trip_i8(I8.highest)
expect round_trip_u16(U16.lowest) and round_trip_u16(U16.highest)
expect round_trip_i16(I16.lowest) and round_trip_i16(I16.highest)
expect round_trip_u32(U32.lowest) and round_trip_u32(U32.highest)
expect round_trip_i32(I32.lowest) and round_trip_i32(I32.highest)
expect round_trip_u64(U64.lowest) and round_trip_u64(U64.highest)
expect round_trip_i64(I64.lowest) and round_trip_i64(I64.highest)
expect round_trip_u128(U128.lowest) and round_trip_u128(U128.highest)
expect round_trip_i128(I128.lowest) and round_trip_i128(I128.highest)

expect {
	result : Try(U8, [InvalidJson(Str)])
	result = Json.parse("256")
	result == Err(Json.invalid_json)
}

expect {
	too_high : Try(I8, [InvalidJson(Str)])
	too_high = Json.parse("128")

	too_low : Try(I8, [InvalidJson(Str)])
	too_low = Json.parse("-129")

	too_high == Err(Json.invalid_json) and too_low == Err(Json.invalid_json)
}

expect {
	result : Try(U16, [InvalidJson(Str)])
	result = Json.parse("65536")
	result == Err(Json.invalid_json)
}

expect {
	too_high : Try(I16, [InvalidJson(Str)])
	too_high = Json.parse("32768")

	too_low : Try(I16, [InvalidJson(Str)])
	too_low = Json.parse("-32769")

	too_high == Err(Json.invalid_json) and too_low == Err(Json.invalid_json)
}

expect {
	result : Try(U32, [InvalidJson(Str)])
	result = Json.parse("4294967296")
	result == Err(Json.invalid_json)
}

expect {
	too_high : Try(I32, [InvalidJson(Str)])
	too_high = Json.parse("2147483648")

	too_low : Try(I32, [InvalidJson(Str)])
	too_low = Json.parse("-2147483649")

	too_high == Err(Json.invalid_json) and too_low == Err(Json.invalid_json)
}

expect {
	result : Try(U64, [InvalidJson(Str)])
	result = Json.parse("18446744073709551616")
	result == Err(Json.invalid_json)
}

expect {
	too_high : Try(I64, [InvalidJson(Str)])
	too_high = Json.parse("9223372036854775808")

	too_low : Try(I64, [InvalidJson(Str)])
	too_low = Json.parse("-9223372036854775809")

	too_high == Err(Json.invalid_json) and too_low == Err(Json.invalid_json)
}

expect {
	result : Try(U128, [InvalidJson(Str)])
	result = Json.parse("340282366920938463463374607431768211456")
	result == Err(Json.invalid_json)
}

expect {
	too_high : Try(I128, [InvalidJson(Str)])
	too_high = Json.parse("170141183460469231731687303715884105728")

	too_low : Try(I128, [InvalidJson(Str)])
	too_low = Json.parse("-170141183460469231731687303715884105729")

	too_high == Err(Json.invalid_json) and too_low == Err(Json.invalid_json)
}
