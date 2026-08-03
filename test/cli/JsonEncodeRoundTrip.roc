JsonEncodeRoundTrip :: [].{}

Token := { raw : Str }.{
	parser_for = |encoding| |state| {
		parsed = Json.parse_str(encoding, state)?
		Ok({ value: Token.{ raw: parsed.value }, rest: parsed.rest })
	}

	encoder_for = |encoding| |token, state| Json.encode_str(encoding, token.raw, state)
}

AutoToken := { raw : Str, count : U64 }.{
	parser_for : _
	encoder_for : _
}

Shape : {
	boxed : Box(Str),
	coords : (Str, U64, Bool),
	dec : Dec,
	explicit_optional : Try(Str, [Missing]),
	flag : Bool,
	foo : Str,
	i8 : I8,
	i16 : I16,
	i32 : I32,
	i64 : I64,
	i128 : I128,
	items : List(Str),
	missing_optional : Try(Str, [Missing]),
	multi : [Multi(Str, U64, Bool)],
	nested : { bar : Str, count : U64 },
	pair : [Pair({ first : Str, second : Str })],
	labels : Set(Str),
	scores : Dict(Str, U64),
	status : [Active, Paused],
	token : Token,
	u8 : U8,
	u16 : U16,
	u32 : U32,
	u128 : U128,
}

NullableShape : {
	missing_nullable : Try(Try(Str, [Null]), [Missing]),
	optional_nullable : Try(Try(Str, [Null]), [Missing]),
	required_nullable : Try(Str, [Null]),
}

source : Str
source = "{\"foo\":\"abc\",\"flag\":true,\"boxed\":\"wrapped\",\"coords\":[\"north\",7,false],\"u8\":8,\"i8\":-8,\"u16\":16,\"i16\":-16,\"u32\":32,\"i32\":-32,\"i64\":-64,\"u128\":128,\"i128\":-128,\"dec\":12.5,\"explicit_optional\":\"present\",\"items\":[\"one\",\"two\"],\"multi\":{\"Multi\":[\"tag\",9,true]},\"nested\":{\"bar\":\"xyz\",\"count\":42},\"pair\":{\"Pair\":{\"first\":\"left\",\"second\":\"right\"}},\"labels\":[\"red\",\"blue\",\"red\"],\"scores\":{\"alpha\":1,\"beta\":2,\"alpha\":3},\"status\":\"Active\",\"token\":\"tok\"}"

nullable_source : Str
nullable_source = "{\"required_nullable\":null,\"optional_nullable\":null}"

auto_nominal_round_trips : Str -> Bool
auto_nominal_round_trips = |json| {
	first_result : Try(AutoToken, [InvalidJson(Str), MissingRequiredField(Str)])
	first_result = Json.parse(json)

	match first_result {
		Ok(first) => {
			encoded1 = Json.to_str(first)

			second_result : Try(AutoToken, [InvalidJson(Str), MissingRequiredField(Str)])
			second_result = Json.parse(encoded1)

			match second_result {
				Ok(second) => {
					encoded2 = Json.to_str(second)

					auto_token_eq(first, second) and Str.is_eq(encoded1, encoded2)
				}
				Err(_) => False
			}
		}
		Err(_) => False
	}
}

auto_token_eq : AutoToken, AutoToken -> Bool
auto_token_eq = |left, right| Str.is_eq(left.raw, right.raw) and left.count == right.count

expect auto_nominal_round_trips("{\"raw\":\"tok\",\"count\":2}")

expect {
	value : AutoToken
	value = AutoToken.{ raw: "tok", count: 2 }

	encoded = Json.to_str(value)

	Str.is_eq(encoded, "{\"count\":2,\"raw\":\"tok\"}")
}

round_trips : Str -> Bool
round_trips = |json| {
	first_result : Try(Shape, [InvalidJson(Str), MissingRequiredField(Str)])
	first_result = Json.parse(json)

	match first_result {
		Ok(first) => {
			encoded1 = Json.to_str(first)

			second_result : Try(Shape, [InvalidJson(Str), MissingRequiredField(Str)])
			second_result = Json.parse(encoded1)

			match second_result {
				Ok(second) => {
					encoded2 = Json.to_str(second)
					shape_eq(first, second) and Str.is_eq(encoded1, encoded2)
				}
				Err(_) => False
			}
		}
		Err(_) => False
	}
}

nullable_round_trips : Str -> Bool
nullable_round_trips = |json| {
	first_result : Try(NullableShape, [InvalidJson(Str), MissingRequiredField(Str)])
	first_result = Json.parse(json)

	match first_result {
		Ok(first) => {
			encoded1 = Json.to_str(first)

			second_result : Try(NullableShape, [InvalidJson(Str), MissingRequiredField(Str)])
			second_result = Json.parse(encoded1)

			match second_result {
				Ok(second) => {
					encoded2 = Json.to_str(second)

					nullable_shape_eq(first, second) and Str.is_eq(encoded1, encoded2)
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
		and left.flag == right.flag
			and Str.is_eq(Box.unbox(left.boxed), Box.unbox(right.boxed))
				and left.coords == right.coords
					and left.u8 == right.u8
						and left.i8 == right.i8
							and left.u16 == right.u16
								and left.i16 == right.i16
									and left.u32 == right.u32
										and left.i32 == right.i32
											and left.nested.count == right.nested.count
												and left.i64 == right.i64
													and left.u128 == right.u128
														and left.i128 == right.i128
															and left.dec == right.dec
																and optional_str_eq(left.explicit_optional, right.explicit_optional)
																	and optional_str_eq(left.missing_optional, right.missing_optional)
																		and left.items == right.items
																			and Str.is_eq(left.nested.bar, right.nested.bar)
																				and multi_eq(left.multi, right.multi)
																					and pair_eq(left.pair, right.pair)
																						and left.labels == right.labels
																							and left.scores == right.scores
																								and status_eq(left.status, right.status)
																									and Str.is_eq(left.token.raw, right.token.raw)

nullable_shape_eq : NullableShape, NullableShape -> Bool
nullable_shape_eq = |left, right|
	nullable_str_eq(left.required_nullable, right.required_nullable)
		and optional_nullable_str_eq(left.optional_nullable, right.optional_nullable)
			and optional_nullable_str_eq(left.missing_nullable, right.missing_nullable)

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

multi_eq : [Multi(Str, U64, Bool)], [Multi(Str, U64, Bool)] -> Bool
multi_eq = |left, right|
	match left {
		Multi(left_text, left_count, left_flag) =>
			match right {
				Multi(right_text, right_count, right_flag) =>
					Str.is_eq(left_text, right_text) and left_count == right_count and left_flag == right_flag
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

nullable_str_eq : Try(Str, [Null]), Try(Str, [Null]) -> Bool
nullable_str_eq = |left, right|
	match left {
		Ok(left_value) =>
			match right {
				Ok(right_value) => Str.is_eq(left_value, right_value)
				Err(Null) => False
			}
		Err(Null) =>
			match right {
				Ok(_) => False
				Err(Null) => True
			}
		}

optional_nullable_str_eq : Try(Try(Str, [Null]), [Missing]), Try(Try(Str, [Null]), [Missing]) -> Bool
optional_nullable_str_eq = |left, right|
	match left {
		Ok(left_value) =>
			match right {
				Ok(right_value) => nullable_str_eq(left_value, right_value)
				Err(Missing) => False
			}
		Err(Missing) =>
			match right {
				Ok(_) => False
				Err(Missing) => True
			}
		}

expect nullable_round_trips(nullable_source)

list_round_trips : Str -> Bool
list_round_trips = |json| {
	first_result : Try(List(Try(Str, [Null])), [InvalidJson(Str)])
	first_result = Json.parse(json)

	match first_result {
		Ok(first) => {
			encoded1 = Json.to_str(first)

			second_result : Try(List(Try(Str, [Null])), [InvalidJson(Str)])
			second_result = Json.parse(encoded1)

			match second_result {
				Ok(second) => {
					encoded2 = Json.to_str(second)

					first == second and Str.is_eq(encoded1, encoded2)
				}
				Err(_) => False
			}
		}
		Err(_) => False
	}
}

expect list_round_trips("[\"one\",null,\"two\"]")

set_round_trips : Str -> Bool
set_round_trips = |json| {
	first_result : Try(Set(Str), [InvalidJson(Str)])
	first_result = Json.parse(json)

	match first_result {
		Ok(first) => {
			encoded1 = Json.to_str(first)

			second_result : Try(Set(Str), [InvalidJson(Str)])
			second_result = Json.parse(encoded1)

			match second_result {
				Ok(second) => {
					encoded2 = Json.to_str(second)

					first == second and Str.is_eq(encoded1, encoded2)
				}
				Err(_) => False
			}
		}
		Err(_) => False
	}
}

expect set_round_trips("[\"alpha\",\"beta\",\"alpha\"]")

expect {
	value : Set(Str)
	value = Set.from_list(["alpha", "beta", "alpha"])

	encoded = Json.to_str(value)

	Str.is_eq(encoded, "[\"alpha\",\"beta\"]")
}

dict_round_trips : Str -> Bool
dict_round_trips = |json| {
	first_result : Try(Dict(Str, U64), [InvalidJson(Str)])
	first_result = Json.parse(json)

	match first_result {
		Ok(first) => {
			encoded1 = Json.to_str(first)

			second_result : Try(Dict(Str, U64), [InvalidJson(Str)])
			second_result = Json.parse(encoded1)

			match second_result {
				Ok(second) => {
					encoded2 = Json.to_str(second)

					first == second and Str.is_eq(encoded1, encoded2)
				}
				Err(_) => False
			}
		}
		Err(_) => False
	}
}

expect dict_round_trips("{\"alpha\":1,\"beta\":2,\"alpha\":3}")

expect {
	value : Dict(Str, U64)
	value = Dict.from_list([("alpha", 1), ("beta", 2), ("alpha", 3)])

	encoded = Json.to_str(value)

	Str.is_eq(encoded, "{\"alpha\":3,\"beta\":2}")
}

numeric_key_dict_round_trips : Str -> Bool
numeric_key_dict_round_trips = |json| {
	first_result : Try(Dict(U64, Bool), [InvalidJson(Str)])
	first_result = Json.parse(json)

	match first_result {
		Ok(first) => {
			encoded1 = Json.to_str(first)

			second_result : Try(Dict(U64, Bool), [InvalidJson(Str)])
			second_result = Json.parse(encoded1)

			match second_result {
				Ok(second) => {
					encoded2 = Json.to_str(second)

					first == second and Str.is_eq(encoded1, encoded2)
				}
				Err(_) => False
			}
		}
		Err(_) => False
	}
}

expect numeric_key_dict_round_trips("{\"7\":true,\"8\":false}")

unit_tag_key_dict_round_trips : Str -> Bool
unit_tag_key_dict_round_trips = |json| {
	first_result : Try(Dict([Active, Paused], U64), [InvalidJson(Str)])
	first_result = Json.parse(json)

	match first_result {
		Ok(first) => {
			encoded1 = Json.to_str(first)

			second_result : Try(Dict([Active, Paused], U64), [InvalidJson(Str)])
			second_result = Json.parse(encoded1)

			match second_result {
				Ok(second) => {
					encoded2 = Json.to_str(second)

					first == second and Str.is_eq(encoded1, encoded2)
				}
				Err(_) => False
			}
		}
		Err(_) => False
	}
}

expect unit_tag_key_dict_round_trips("{\"Active\":1,\"Paused\":2}")

expect {
	value : Dict(Bool, Str)
	value = Dict.from_list([(True, "yes"), (False, "no")])

	encoded = Json.to_str(value)

	Str.is_eq(encoded, "{\"true\":\"yes\",\"false\":\"no\"}")
}

expect {
	result : Try(Dict(U8, Str), [InvalidJson(Str)])
	result = Json.parse("{\"999\":\"too big\"}")

	result == Err(Json.invalid_json)
}

expect {
	value : List(Str)
	value = ["alpha", "beta"]

	encoded = Json.to_str(value)

	Str.is_eq(encoded, "[\"alpha\",\"beta\"]")
}

expect {
	result : Try(List(Str), [InvalidJson(Str)])
	result = Json.parse_trailing_commas("[\"alpha\",]")

	result == Ok(["alpha"])
}

tuple_round_trips : Str -> Bool
tuple_round_trips = |json| {
	first_result : Try((Str, U64, Bool), [InvalidJson(Str)])
	first_result = Json.parse(json)

	match first_result {
		Ok(first) => {
			encoded1 = Json.to_str(first)

			second_result : Try((Str, U64, Bool), [InvalidJson(Str)])
			second_result = Json.parse(encoded1)

			match second_result {
				Ok(second) => {
					encoded2 = Json.to_str(second)

					first == second and Str.is_eq(encoded1, encoded2)
				}
				Err(_) => False
			}
		}
		Err(_) => False
	}
}

expect tuple_round_trips("[\"north\",7,false]")

expect {
	value : (Str, U64, Bool)
	value = ("north", 7, False)

	encoded = Json.to_str(value)

	Str.is_eq(encoded, "[\"north\",7,false]")
}

expect {
	result : Try((Str, U64), [InvalidJson(Str)])
	result = Json.parse("[\"only\"]")

	result == Err(Json.invalid_json)
}

expect {
	result : Try((Str, U64), [InvalidJson(Str)])
	result = Json.parse("[\"too\",2,true]")

	result == Err(Json.invalid_json)
}

expect {
	result : Try(Try(Str, [Null]), [InvalidJson(Str)])
	result = Json.parse("null")

	result == Ok(Err(Null))
}

expect {
	value : Try(Str, [Null])
	value = Err(Null)

	encoded = Json.to_str(value)

	Str.is_eq(encoded, "null")
}

expect {
	value : { text : Str }
	value = { text: "quote \" slash \\" }

	encoded = Json.to_str(value)

	Str.is_eq(encoded, "{\"text\":\"quote \\\" slash \\\\\"}")
}

box_parses : Str -> Bool
box_parses = |json| {
	result : Try(Box(Str), [InvalidJson(Str)])
	result = Json.parse(json)

	match result {
		Ok(boxed) => Str.is_eq(Box.unbox(boxed), "boxed")
		Err(_) => False
	}
}

expect box_parses("\"boxed\"")

expect {
	boxed : Box(Str)
	boxed = Box.box("boxed")

	encoded = Json.to_str(boxed)

	Str.is_eq(encoded, "\"boxed\"")
}

expect {
	value : [Active, Paused]
	value = Active

	encoded = Json.to_str(value)

	Str.is_eq(encoded, "\"Active\"")
}

expect {
	value : [Multi(Str, U64, Bool)]
	value = Multi("tag", 9, True)

	encoded = Json.to_str(value)

	Str.is_eq(encoded, "{\"Multi\":[\"tag\",9,true]}")
}

expect {
	result : Try([Multi(Str, U64, Bool)], [InvalidJson(Str)])
	result = Json.parse("{\"Multi\":[\"tag\",9,true]}")

	result == Ok(Multi("tag", 9, True))
}

expect {
	value : [One(Str)]
	value = One("payload")

	Json.to_str(value) == "{\"One\":\"payload\"}"
}

expect {
	result : Try([One(Str)], [InvalidJson(Str)])
	result = Json.parse("{\"One\":\"payload\"}")

	result == Ok(One("payload"))
}

expect {
	result : Try([Active, Paused], [InvalidJson(Str)])
	result = Json.parse("{\"Active\":{}}")

	result == Ok(Active)
}

expect {
	result : Try([Multi(Str, U64, Bool)], [InvalidJson(Str)])
	result = Json.parse("\"Multi\"")

	result == Err(Json.invalid_json)
}

expect {
	result : Try([Multi(Str, U64, Bool)], [InvalidJson(Str)])
	result = Json.parse("{\"Multi\":[\"tag\",9]}")

	result == Err(Json.invalid_json)
}

expect {
	result : Try([Multi(Str, U64, Bool)], [InvalidJson(Str)])
	result = Json.parse("{\"Multi\":[\"tag\",9,true,0]}")

	result == Err(Json.invalid_json)
}

expect {
	result : Try([Multi(Str, U64, Bool)], [InvalidJson(Str)])
	result = Json.parse("{\"Multi\":[\"tag\",9,true] {}}")

	result == Err(Json.invalid_json)
}
