app [main!] { pf: platform "./platform/main.roc" }

main! : Str => U64
main! = |json| {
	if !Str.starts_with(json, "{") {
		scalar_result : Try(U64, [InvalidJson(Str)])
		scalar_result = Json.parse(json)
		return match scalar_result {
			Ok(value) => value
			Err(_) => 999999
		}
	}

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
		[InvalidJson(Str), MissingRequiredField(Str)],
	)
	decoded_result = Json.parse(json)

	empty_result : Try({}, [InvalidJson(Str)])
	empty_result = Json.parse("{}")

	invalid_empty_result : Try({}, [InvalidJson(Str)])
	invalid_empty_result = Json.parse("not-json")

	trailing_empty_result : Try({}, [InvalidJson(Str)])
	trailing_empty_result = Json.parse("{} trailing")

	top_level_string_result : Try(Str, [InvalidJson(Str)])
	top_level_string_result = Json.parse("\"top-level-json\"")

	invalid_string_result : Try(Str, [InvalidJson(Str)])
	invalid_string_result = Json.parse("bare-json-string")

	null_string_result : Try(Str, [InvalidJson(Str)])
	null_string_result = Json.parse("null")

	strict_trailing_comma_result : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	strict_trailing_comma_result = Json.parse("{\"foo\":\"comma\",}")

	lenient_trailing_comma_result : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	lenient_trailing_comma_result = Json.parse_trailing_commas("{\"foo\":\"comma\",}")

	strict_tag_trailing_comma_result : Try([Active, Paused], [InvalidJson(Str)])
	strict_tag_trailing_comma_result = Json.parse("{\"Active\":{},}")

	lenient_tag_trailing_comma_result : Try([Active, Paused], [InvalidJson(Str)])
	lenient_tag_trailing_comma_result = Json.parse_trailing_commas("{\"Active\":{},}")

	unknown_array_result : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	unknown_array_result = Json.parse("{\"foo\":\"array\",\"skip\":[1,2]}")

	strict_unknown_array_trailing_comma_result : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	strict_unknown_array_trailing_comma_result = Json.parse("{\"foo\":\"array\",\"skip\":[1,2,]}")

	lenient_unknown_array_trailing_comma_result : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	lenient_unknown_array_trailing_comma_result = Json.parse_trailing_commas("{\"foo\":\"array\",\"skip\":[1,2,]}")

	invalid_unknown_scalar_result : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	invalid_unknown_scalar_result = Json.parse("{\"foo\":\"ok\",\"skip\":not-json}")

	invalid_unknown_array_scalar_result : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	invalid_unknown_array_scalar_result = Json.parse("{\"foo\":\"ok\",\"skip\":[not-json]}")

	invalid_u64_plus_result : Try({ n : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	invalid_u64_plus_result = Json.parse("{\"n\":+1}")

	invalid_u64_leading_zero_result : Try({ n : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
	invalid_u64_leading_zero_result = Json.parse("{\"n\":01}")

	invalid_missing_tag_payload_result : Try([Active, Paused], [InvalidJson(Str)])
	invalid_missing_tag_payload_result = Json.parse("{\"Active\":}")

	# The known field's value contains \n, \", and \uXXXX escapes. The other
	# fields are unknown, so the decoder skips them; their values contain
	# escaped quotes, escaped backslashes, and a surrogate pair.
	escaped_string_result : Try({ note : Str }, [InvalidJson(Str), MissingRequiredField(Str)])
	escaped_string_result = Json.parse("{\"note\":\"roc\\nsays \\\"kree\\\" caf\\u00e9\",\"keeper\":{\"bio\":\"tends the \\\"Roc\\\" egg\",\"links\":[\"https:\\/\\/roc.example\\/1\",\"wing\\tspan\"]},\"bird\":\"\\ud83e\\udd85\"}")

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
				+ escaped_string_score(escaped_string_result)
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

empty_record_score : Try({}, [InvalidJson(Str)]) -> U64
empty_record_score = |empty_result|
	match empty_result {
		Ok(_) => 29
		Err(_) => 999999
	}

invalid_empty_record_score : Try({}, [InvalidJson(Str)]) -> U64
invalid_empty_record_score = |invalid_empty_result|
	match invalid_empty_result {
		Ok(_) => 999999
		Err(InvalidJson(_)) => 37
	}

trailing_empty_record_score : Try({}, [InvalidJson(Str)]) -> U64
trailing_empty_record_score = |trailing_empty_result|
	match trailing_empty_result {
		Ok(_) => 999999
		Err(InvalidJson(_)) => 41
	}

top_level_string_score : Try(Str, [InvalidJson(Str)]) -> U64
top_level_string_score = |string_result|
	match string_result {
		Ok(value) => Str.count_utf8_bytes(value)
		Err(_) => 999999
	}

invalid_string_score : Try(Str, [InvalidJson(Str)]) -> U64
invalid_string_score = |string_result|
	match string_result {
		Ok(_) => 999999
		Err(InvalidJson(_)) => 43
	}

escaped_string_score : Try({ note : Str }, [InvalidJson(Str), MissingRequiredField(Str)]) -> U64
escaped_string_score = |escaped_result|
	match escaped_result {
		Ok(decoded) =>
			if Str.is_eq(decoded.note, "roc\nsays \"kree\" café") {
				107
			} else {
				999999
			}
		Err(_) => 999999
	}

null_string_score : Try(Str, [InvalidJson(Str)]) -> U64
null_string_score = |string_result|
	match string_result {
		Ok(_) => 999999
		Err(InvalidJson(_)) => 47
	}

strict_trailing_comma_score : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)]) -> U64
strict_trailing_comma_score = |record_result|
	match record_result {
		Ok(_) => 999999
		Err(InvalidJson(_)) => 53
		Err(_) => 999999
	}

lenient_trailing_comma_score : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)]) -> U64
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

strict_tag_trailing_comma_score : Try([Active, Paused], [InvalidJson(Str)]) -> U64
strict_tag_trailing_comma_score = |tag_result|
	match tag_result {
		Ok(_) => 999999
		Err(InvalidJson(_)) => 67
	}

lenient_tag_trailing_comma_score : Try([Active, Paused], [InvalidJson(Str)]) -> U64
lenient_tag_trailing_comma_score = |tag_result|
	match tag_result {
		Ok(Active) => 71
		Ok(_) => 999999
		Err(_) => 999999
	}

unknown_array_score : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)]) -> U64
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

strict_unknown_array_trailing_comma_score : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)]) -> U64
strict_unknown_array_trailing_comma_score = |record_result|
	match record_result {
		Ok(_) => 999999
		Err(InvalidJson(_)) => 73
		Err(_) => 999999
	}

lenient_unknown_array_trailing_comma_score : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)]) -> U64
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

invalid_unknown_scalar_score : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)]) -> U64
invalid_unknown_scalar_score = |record_result|
	match record_result {
		Ok(_) => 999999
		Err(InvalidJson(_)) => 83
		Err(_) => 999999
	}

invalid_unknown_array_scalar_score : Try({ foo : Str }, [InvalidJson(Str), MissingRequiredField(Str)]) -> U64
invalid_unknown_array_scalar_score = |record_result|
	match record_result {
		Ok(_) => 999999
		Err(InvalidJson(_)) => 89
		Err(_) => 999999
	}

invalid_u64_plus_score : Try({ n : U64 }, [InvalidJson(Str), MissingRequiredField(Str)]) -> U64
invalid_u64_plus_score = |record_result|
	match record_result {
		Ok(_) => 999999
		Err(InvalidJson(_)) => 97
		Err(_) => 999999
	}

invalid_u64_leading_zero_score : Try({ n : U64 }, [InvalidJson(Str), MissingRequiredField(Str)]) -> U64
invalid_u64_leading_zero_score = |record_result|
	match record_result {
		Ok(_) => 999999
		Err(InvalidJson(_)) => 101
		Err(_) => 999999
	}

invalid_missing_tag_payload_score : Try([Active, Paused], [InvalidJson(Str)]) -> U64
invalid_missing_tag_payload_score = |tag_result|
	match tag_result {
		Ok(_) => 999999
		Err(InvalidJson(_)) => 103
	}
