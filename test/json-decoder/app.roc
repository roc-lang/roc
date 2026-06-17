app [main!] { pf: platform "./platform/main.roc" }

import pf.Json

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
			token : Json.Token,
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
				+ Json.Token.count_utf8_bytes(decoded.token)
				+ empty_record_score(empty_result)
				+ invalid_empty_record_score(invalid_empty_result)
				+ trailing_empty_record_score(trailing_empty_result)
				+ top_level_string_score(top_level_string_result)
				+ invalid_string_score(invalid_string_result)
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

invalid_empty_record_score : Try({}, _) -> U64
invalid_empty_record_score = |invalid_empty_result|
	match invalid_empty_result {
		Ok(_) => 999999
		Err(_) => 37
	}

trailing_empty_record_score : Try({}, _) -> U64
trailing_empty_record_score = |trailing_empty_result|
	match trailing_empty_result {
		Ok(_) => 999999
		Err(_) => 41
	}

top_level_string_score : Try(Str, _) -> U64
top_level_string_score = |string_result|
	match string_result {
		Ok(value) => Str.count_utf8_bytes(value)
		Err(_) => 999999
	}

invalid_string_score : Try(Str, _) -> U64
invalid_string_score = |string_result|
	match string_result {
		Ok(_) => 999999
		Err(_) => 43
	}
