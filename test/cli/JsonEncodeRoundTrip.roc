JsonEncodeRoundTrip :: [].{}

Token := { raw : Str }.{
	parser_for = |encoding| |state| {
		parsed = Json.parse_str(encoding, state)?
		Ok({ value: Token.{ raw: parsed.value }, rest: parsed.rest })
	}

	encode_to = |token, encoding| |state| Json.encode_str(encoding, token.raw, state)
}

Shape : {
	explicit_optional : Try(Str, [Missing]),
	foo : Str,
	missing_optional : Try(Str, [Missing]),
	nested : { bar : Str, count : U64 },
	pair : [Pair({ first : Str, second : Str })],
	status : [Active, Paused],
	token : Token,
}

source : Str
source = "{\"foo\":\"abc\",\"explicit_optional\":\"present\",\"nested\":{\"bar\":\"xyz\",\"count\":42},\"pair\":{\"Pair\":{\"first\":\"left\",\"second\":\"right\"}},\"status\":{\"Active\":{}},\"token\":\"tok\"}"

round_trips : Str -> Bool
round_trips = |json| {
	first_result : Try(Shape, Json)
	first_result = Json.parse(json)

	match first_result {
		Ok(first) => {
			encoded1_result : Try(Str, [])
			encoded1_result = Json.encode(first)
			Ok(encoded1) = encoded1_result

			second_result : Try(Shape, Json)
			second_result = Json.parse(encoded1)

			match second_result {
				Ok(second) => {
					encoded2_result : Try(Str, [])
					encoded2_result = Json.encode(second)
					Ok(encoded2) = encoded2_result

					shape_eq(first, second) and Str.is_eq(encoded1, encoded2)
				}
				Err(_) => False
			}
		}
		Err(_) => False
	}
}

shape_eq : Shape, Shape -> Bool
shape_eq = |left, right|
	Str.is_eq(left.foo, right.foo)
		and optional_str_eq(left.explicit_optional, right.explicit_optional)
			and optional_str_eq(left.missing_optional, right.missing_optional)
				and Str.is_eq(left.nested.bar, right.nested.bar)
					and left.nested.count == right.nested.count
						and pair_eq(left.pair, right.pair)
							and status_eq(left.status, right.status)
								and Str.is_eq(left.token.raw, right.token.raw)

optional_str_eq : Try(Str, [Missing]), Try(Str, [Missing]) -> Bool
optional_str_eq = |left, right|
	match left {
		Ok(left_value) =>
			match right {
				Ok(right_value) => Str.is_eq(left_value, right_value)
				Err(Missing) => False
			}
		Err(Missing) =>
			match right {
				Ok(_) => False
				Err(Missing) => True
			}
		}

pair_eq : [Pair({ first : Str, second : Str })], [Pair({ first : Str, second : Str })] -> Bool
pair_eq = |left, right|
	match left {
		Pair(left_pair) =>
			match right {
				Pair(right_pair) =>
					Str.is_eq(left_pair.first, right_pair.first) and Str.is_eq(left_pair.second, right_pair.second)
				}
		}

status_eq : [Active, Paused], [Active, Paused] -> Bool
status_eq = |left, right|
	match left {
		Active =>
			match right {
				Active => True
				Paused => False
			}
		Paused =>
			match right {
				Active => False
				Paused => True
			}
		}

expect round_trips(source)

expect {
	value : { text : Str }
	value = { text: "quote \" slash \\" }

	encoded_result : Try(Str, [])
	encoded_result = Json.encode(value)
	Ok(encoded) = encoded_result

	Str.is_eq(encoded, "{\"text\":\"quote \\\" slash \\\\\"}")
}
