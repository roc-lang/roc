app [main!] { pf: platform "./platform/main.roc" }

main! : Str => U64
main! = |json| {
	decoded_result : Try(
		{
			explicit_optional : Try(Str, [Missing]),
			foo : Str,
			nested : {
				bar : Str,
				mode : [Warm, Cold],
			},
			pair : [Pair({ first : Str, second : Str })],
			question_optional : Try(Str, [Missing]),
			status : [Active, Paused],
			token : Token,
			wildcard_optional : Try(Str, _),
		},
		_,
	)
	decoded_result = Json.parse(json)

	empty_result : Try({}, _)
	empty_result = Json.parse("{}")

	invalid_empty_result : Try({}, _)
	invalid_empty_result = Json.parse("not-json")

	trailing_empty_result : Try({}, _)
	trailing_empty_result = Json.parse("{} trailing")

	top_level_string_result : Try(Str, _)
	top_level_string_result = Json.parse("\"top-level-json\"")

	invalid_string_result : Try(Str, _)
	invalid_string_result = Json.parse("bare-json-string")

	null_string_result : Try(Str, _)
	null_string_result = Json.parse("null")

	strict_trailing_comma_result : Try({ foo : Str }, Json)
	strict_trailing_comma_result = Json.parse("{\"foo\":\"comma\",}")

	lenient_trailing_comma_result : Try({ foo : Str }, Json)
	lenient_trailing_comma_result = Json.parse_trailing_commas("{\"foo\":\"comma\",}")

	strict_tag_trailing_comma_result : Try([Active, Paused], Json)
	strict_tag_trailing_comma_result = Json.parse("{\"Active\":{},}")

	lenient_tag_trailing_comma_result : Try([Active, Paused], Json)
	lenient_tag_trailing_comma_result = Json.parse_trailing_commas("{\"Active\":{},}")

	unknown_array_result : Try({ foo : Str }, Json)
	unknown_array_result = Json.parse("{\"foo\":\"array\",\"skip\":[1,2]}")

	strict_unknown_array_trailing_comma_result : Try({ foo : Str }, Json)
	strict_unknown_array_trailing_comma_result = Json.parse("{\"foo\":\"array\",\"skip\":[1,2,]}")

	lenient_unknown_array_trailing_comma_result : Try({ foo : Str }, Json)
	lenient_unknown_array_trailing_comma_result = Json.parse_trailing_commas("{\"foo\":\"array\",\"skip\":[1,2,]}")

	invalid_unknown_scalar_result : Try({ foo : Str }, Json)
	invalid_unknown_scalar_result = Json.parse("{\"foo\":\"ok\",\"skip\":not-json}")

	invalid_unknown_array_scalar_result : Try({ foo : Str }, Json)
	invalid_unknown_array_scalar_result = Json.parse("{\"foo\":\"ok\",\"skip\":[not-json]}")

	invalid_u64_plus_result : Try({ n : U64 }, Json)
	invalid_u64_plus_result = Json.parse("{\"n\":+1}")

	invalid_u64_leading_zero_result : Try({ n : U64 }, Json)
	invalid_u64_leading_zero_result = Json.parse("{\"n\":01}")

	invalid_missing_tag_payload_result : Try([Active, Paused], Json)
	invalid_missing_tag_payload_result = Json.parse("{\"Active\":}")

	match decoded_result {
		Ok(decoded) => {
			explicit_optional_length = match decoded.explicit_optional {
				Ok(value) => Str.count_utf8_bytes(value)
				Err(Missing) => 0
			}

			wildcard_optional_length = match decoded.wildcard_optional {
				Ok(value) => Str.count_utf8_bytes(value)
				Err(_) => 0
			}

			question_optional_length = match question_length(decoded.question_optional) {
				Ok(length) => length
				Err(Missing) => 0
			}

			status_score = match decoded.status {
				Active => 11
				Paused => 17
			}

			mode_score = match decoded.nested.mode {
				Warm => 19
				Cold => 23
			}

			pair_score = match decoded.pair {
				Pair(payload) =>
					if Str.is_eq(payload.first, "left") {
						if Str.is_eq(payload.second, "right") {
							31
						} else {
							999999
						}
					} else {
						999999
					}
				}

			Str.count_utf8_bytes(decoded.foo)
				+ Str.count_utf8_bytes(decoded.nested.bar)
				+ Token.count_utf8_bytes(decoded.token)
				+ empty_record_score(empty_result)
				+ invalid_empty_record_score(invalid_empty_result)
				+ trailing_empty_record_score(trailing_empty_result)
				+ top_level_string_score(top_level_string_result)
				+ invalid_string_score(invalid_string_result)
				+ null_string_score(null_string_result)
				+ strict_trailing_comma_score(strict_trailing_comma_result)
				+ lenient_trailing_comma_score(lenient_trailing_comma_result)
				+ strict_tag_trailing_comma_score(strict_tag_trailing_comma_result)
				+ lenient_tag_trailing_comma_score(lenient_tag_trailing_comma_result)
				+ unknown_array_score(unknown_array_result)
				+ strict_unknown_array_trailing_comma_score(strict_unknown_array_trailing_comma_result)
				+ lenient_unknown_array_trailing_comma_score(lenient_unknown_array_trailing_comma_result)
				+ invalid_unknown_scalar_score(invalid_unknown_scalar_result)
				+ invalid_unknown_array_scalar_score(invalid_unknown_array_scalar_result)
				+ invalid_u64_plus_score(invalid_u64_plus_result)
				+ invalid_u64_leading_zero_score(invalid_u64_leading_zero_result)
				+ invalid_missing_tag_payload_score(invalid_missing_tag_payload_result)
				+ explicit_optional_length
				+ wildcard_optional_length
				+ question_optional_length
				+ status_score
				+ mode_score
				+ pair_score
		}
		Err(_) => 999999
	}
}

Token := { raw : Str }.{
	parser_for = |encoding| |state| {
		parsed = Json.parse_str(encoding, state)?
		Ok({ value: { raw: "custom-token" }, rest: parsed.rest })
	}

	count_utf8_bytes : Token -> U64
	count_utf8_bytes = |token| Str.count_utf8_bytes(token.raw)
}

question_length : Try(Str, [Missing]) -> Try(U64, [Missing])
question_length = |maybe| {
	value = maybe?
	Ok(Str.count_utf8_bytes(value))
}

empty_record_score : Try({}, _) -> U64
empty_record_score = |empty_result|
	match empty_result {
		Ok(_) => 29
		Err(_) => 999999
	}

invalid_empty_record_score : Try({}, Json) -> U64
invalid_empty_record_score = |invalid_empty_result|
	match invalid_empty_result {
		Ok(_) => 999999
		Err(Json.InvalidJson) => 37
		Err(_) => 999999
	}

trailing_empty_record_score : Try({}, Json) -> U64
trailing_empty_record_score = |trailing_empty_result|
	match trailing_empty_result {
		Ok(_) => 999999
		Err(Json.InvalidJson) => 41
		Err(_) => 999999
	}

top_level_string_score : Try(Str, _) -> U64
top_level_string_score = |string_result|
	match string_result {
		Ok(value) => Str.count_utf8_bytes(value)
		Err(_) => 999999
	}

invalid_string_score : Try(Str, Json) -> U64
invalid_string_score = |string_result|
	match string_result {
		Ok(_) => 999999
		Err(Json.InvalidJson) => 43
		Err(_) => 999999
	}

null_string_score : Try(Str, Json) -> U64
null_string_score = |string_result|
	match string_result {
		Ok(_) => 999999
		Err(Json.InvalidJson) => 47
		Err(_) => 999999
	}

strict_trailing_comma_score : Try({ foo : Str }, Json) -> U64
strict_trailing_comma_score = |record_result|
	match record_result {
		Ok(_) => 999999
		Err(Json.InvalidJson) => 53
		Err(_) => 999999
	}

lenient_trailing_comma_score : Try({ foo : Str }, Json) -> U64
lenient_trailing_comma_score = |record_result|
	match record_result {
		Ok(record) =>
			if Str.is_eq(record.foo, "comma") {
				59
			} else {
				999999
			}
		Err(_) => 999999
	}

strict_tag_trailing_comma_score : Try([Active, Paused], Json) -> U64
strict_tag_trailing_comma_score = |tag_result|
	match tag_result {
		Ok(_) => 999999
		Err(Json.InvalidJson) => 67
		Err(_) => 999999
	}

lenient_tag_trailing_comma_score : Try([Active, Paused], Json) -> U64
lenient_tag_trailing_comma_score = |tag_result|
	match tag_result {
		Ok(Active) => 71
		Ok(_) => 999999
		Err(_) => 999999
	}

unknown_array_score : Try({ foo : Str }, Json) -> U64
unknown_array_score = |record_result|
	match record_result {
		Ok(record) =>
			if Str.is_eq(record.foo, "array") {
				61
			} else {
				999999
			}
		Err(_) => 999999
	}

strict_unknown_array_trailing_comma_score : Try({ foo : Str }, Json) -> U64
strict_unknown_array_trailing_comma_score = |record_result|
	match record_result {
		Ok(_) => 999999
		Err(Json.InvalidJson) => 73
		Err(_) => 999999
	}

lenient_unknown_array_trailing_comma_score : Try({ foo : Str }, Json) -> U64
lenient_unknown_array_trailing_comma_score = |record_result|
	match record_result {
		Ok(record) =>
			if Str.is_eq(record.foo, "array") {
				79
			} else {
				999999
			}
		Err(_) => 999999
	}

invalid_unknown_scalar_score : Try({ foo : Str }, Json) -> U64
invalid_unknown_scalar_score = |record_result|
	match record_result {
		Ok(_) => 999999
		Err(Json.InvalidJson) => 83
		Err(_) => 999999
	}

invalid_unknown_array_scalar_score : Try({ foo : Str }, Json) -> U64
invalid_unknown_array_scalar_score = |record_result|
	match record_result {
		Ok(_) => 999999
		Err(Json.InvalidJson) => 89
		Err(_) => 999999
	}

invalid_u64_plus_score : Try({ n : U64 }, Json) -> U64
invalid_u64_plus_score = |record_result|
	match record_result {
		Ok(_) => 999999
		Err(Json.InvalidJson) => 97
		Err(_) => 999999
	}

invalid_u64_leading_zero_score : Try({ n : U64 }, Json) -> U64
invalid_u64_leading_zero_score = |record_result|
	match record_result {
		Ok(_) => 999999
		Err(Json.InvalidJson) => 101
		Err(_) => 999999
	}

invalid_missing_tag_payload_score : Try([Active, Paused], Json) -> U64
invalid_missing_tag_payload_score = |tag_result|
	match tag_result {
		Ok(_) => 999999
		Err(Json.InvalidJson) => 103
		Err(_) => 999999
	}
