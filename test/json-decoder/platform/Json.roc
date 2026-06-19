JsonState := [Input(Str)]

JsonEncoding := [Default].{
	rename_field : Str -> Str
	rename_field = |name| name

	parse_str : JsonState -> Try({ value : Str, rest : JsonState }, Json.DecodeErr)
	parse_str = |state|
		match state {
			Input(raw) => {
				trimmed = Str.trim_start(raw)
				if Str.starts_with(trimmed, "\"") {
					string_parts = split_json_string_tail(Str.drop_prefix(trimmed, "\""))?
					rest = Str.trim_start(string_parts.after)
					Ok({ value: string_parts.value, rest: JsonState.Input(rest) })
				} else {
					Err(invalid_json)
				}
			}
		}

	parse_record_field : Fields(_shape), JsonState -> Try(
		[
			Field({ field : Field(_shape), rest : JsonState }),
			TryField({ name : Str, rest : JsonState }),
			TryFieldCaseless({ name : Str, rest : JsonState }),
			Continue({ rest : JsonState }),
			Done({ rest : JsonState }),
		],
		Json.DecodeErr,
	)
	parse_record_field = |fields, state|
		match state {
			Input(raw) => parse_record_field_from_object(fields, raw)
		}

	skip_record_field : JsonState -> Try(JsonState, Json.DecodeErr)
	skip_record_field = |state| skip_json_value(state)

	missing_record_field : Str, JsonState -> Json.DecodeErr
	missing_record_field = |_, _| Json.DecodeErr.MissingRequired

	missing_optional_field : Str, JsonState -> [Missing]
	missing_optional_field = |_, _| Missing

	parse_tag_union : ParseTagUnionSpec(a), JsonState -> Try({ value : a, rest : JsonState }, Json.DecodeErr)
	parse_tag_union = |spec, state|
		match state {
			Input(value) => parse_tag_union_from_json(value, spec)
	}
}

JsonCamelEncoding := [Default].{
	rename_field : Str -> Str
	rename_field = |name| snake_to_camel(name)

	parse_str : JsonState -> Try({ value : Str, rest : JsonState }, Json.DecodeErr)
	parse_str = |state| JsonEncoding.parse_str(state)

	parse_record_field : Fields(_shape), JsonState -> Try(
		[
			Field({ field : Field(_shape), rest : JsonState }),
			TryField({ name : Str, rest : JsonState }),
			TryFieldCaseless({ name : Str, rest : JsonState }),
			Continue({ rest : JsonState }),
			Done({ rest : JsonState }),
		],
		Json.DecodeErr,
	)
	parse_record_field = |fields, state|
		match state {
			Input(raw) => parse_record_field_from_object(fields, raw)
		}

	skip_record_field : JsonState -> Try(JsonState, Json.DecodeErr)
	skip_record_field = |state| skip_json_value(state)

	missing_record_field : Str, JsonState -> Json.DecodeErr
	missing_record_field = |_, _| Json.DecodeErr.MissingRequired

	missing_optional_field : Str, JsonState -> [Missing]
	missing_optional_field = |_, _| Missing

	parse_tag_union : ParseTagUnionSpec(a), JsonState -> Try({ value : a, rest : JsonState }, Json.DecodeErr)
	parse_tag_union = |spec, state|
		match state {
			Input(value) => parse_tag_union_from_json(value, spec)
		}
}

Json :: [].{
	DecodeErr := [MissingRequired, InvalidJson].{}

	Token := { raw : Str }.{
		parser_for : JsonEncoding -> (JsonState -> Try({ value : Token, rest : JsonState }, Json.DecodeErr))
		parser_for = |_encoding| |state| {
			parsed = JsonEncoding.parse_str(state)?
			Ok({ value: { raw: "custom-token" }, rest: parsed.rest })
		}

		count_utf8_bytes : Token -> U64
		count_utf8_bytes = |token| Str.count_utf8_bytes(token.raw)
	}

	parse : Str -> Try(a, Json.DecodeErr)
		where [
			a.parser_for : JsonEncoding -> (JsonState -> Try({ value : a, rest : JsonState }, Json.DecodeErr)),
		]
	parse = |json| {
		Shape : a
		parse_shape = Shape.parser_for(JsonEncoding.Default)
		parsed = parse_shape(JsonState.Input(json))?

		match parsed.rest {
			Input(rest) =>
				if Str.is_empty(Str.trim_start(rest)) {
					Ok(parsed.value)
				} else {
					Err(invalid_json)
				}
		}
	}

	parser_camel : () -> (Str -> Try(a, Json.DecodeErr))
		where [
			a.parser_for : JsonCamelEncoding -> (JsonState -> Try({ value : a, rest : JsonState }, Json.DecodeErr)),
		]
	parser_camel = || {
		Shape : a
		parse_shape = Shape.parser_for(JsonCamelEncoding.Default)

		|json| {
			parsed = parse_shape(JsonState.Input(json))?

			match parsed.rest {
				Input(rest) =>
					if Str.is_empty(Str.trim_start(rest)) {
						Ok(parsed.value)
					} else {
						Err(invalid_json)
					}
			}
		}
	}

	parse_camel : Str -> Try(a, Json.DecodeErr)
		where [
			a.parser_for : JsonCamelEncoding -> (JsonState -> Try({ value : a, rest : JsonState }, Json.DecodeErr)),
		]
	parse_camel = |json| {
		Shape : a
		parse_shape = Shape.parser_for(JsonCamelEncoding.Default)
		parsed = parse_shape(JsonState.Input(json))?

		match parsed.rest {
			Input(rest) =>
				if Str.is_empty(Str.trim_start(rest)) {
					Ok(parsed.value)
				} else {
					Err(invalid_json)
				}
		}
	}
}

invalid_json : Json.DecodeErr
invalid_json = Json.DecodeErr.InvalidJson

parse_record_field_from_object : Fields(_shape), Str -> Try(
	[
		Field({ field : Field(_shape), rest : JsonState }),
		TryField({ name : Str, rest : JsonState }),
		TryFieldCaseless({ name : Str, rest : JsonState }),
		Continue({ rest : JsonState }),
		Done({ rest : JsonState }),
	],
	Json.DecodeErr,
)
parse_record_field_from_object = |fields, raw| {
	remaining = Str.trim_start(raw)

	if Str.starts_with(remaining, "{") {
		return parse_record_field_from_object(fields, Str.trim_start(Str.drop_prefix(remaining, "{")))
	}

	if Str.starts_with(remaining, ",") {
		return parse_record_field_from_object(fields, Str.trim_start(Str.drop_prefix(remaining, ",")))
	}

	if Str.starts_with(remaining, "}") {
		after_record = Str.trim_start(Str.drop_prefix(remaining, "}"))
		return Ok(Done({ rest: JsonState.Input(after_record) }))
	}

	if !Str.starts_with(remaining, "\"") {
		return Err(invalid_json)
	}

	key_parts = split_json_string_tail(Str.drop_prefix(remaining, "\""))?
	key = key_parts.value
	after_key = Str.trim_start(key_parts.after)

	if !Str.starts_with(after_key, ":") {
		return Err(invalid_json)
	}

	after_colon = Str.trim_start(Str.drop_prefix(after_key, ":"))
	rest = JsonState.Input(after_colon)

	match find_field(fields, key) {
		Ok(field) => Ok(Field({ field, rest }))
		Err(NotFound) => {
			after_skip = skip_json_value(rest)?
			Ok(Continue({ rest: after_skip }))
		}
	}
}

find_field : Fields(_shape), Str -> Try(Field(_shape), [NotFound])
find_field = |fields, name| {
	var $remaining = Fields.for_size(fields, Str.count_utf8_bytes(name))

	while True {
		match Iter.next($remaining) {
			One({ item, rest }) =>
				if Str.is_eq(Field.name(item), name) {
					return Ok(item)
				} else {
					$remaining = rest
				}

			Skip({ rest }) => {
				$remaining = rest
			}

			Done =>
				return Err(NotFound)
		}
	}
}

snake_to_camel : Str -> Str
snake_to_camel = |text|
	match Str.find_first(text, "_") {
		Ok({ before, after }) =>
			before.concat(upper_first_ascii(snake_to_camel(after)))

		Err(NotFound) => text
	}

upper_first_ascii : Str -> Str
upper_first_ascii = |text| {
	bytes = Str.to_utf8(text)

	match List.first(bytes) {
		Ok(first) => {
			upper = if first >= 97 {
				if first <= 122 {
					first - 32
				} else {
					first
				}
			} else {
				first
			}

			match Str.from_utf8([upper].concat(List.drop_first(bytes, 1))) {
				Ok(value) => value
				Err(_) => text
			}
		}

		Err(_) => text
	}
}

skip_json_value : JsonState -> Try(JsonState, Json.DecodeErr)
skip_json_value = |state|
	match state {
		Input(raw) => {
			trimmed = Str.trim_start(raw)

			if Str.starts_with(trimmed, "\"") {
				value_parts = split_json_string_tail(Str.drop_prefix(trimmed, "\""))?
				Ok(JsonState.Input(Str.trim_start(value_parts.after)))
			} else if Str.starts_with(trimmed, "{") {
				end_parts = find_object_end(trimmed)?
				Ok(JsonState.Input(Str.trim_start(end_parts.after)))
			} else {
				Err(invalid_json)
			}
		}
	}

parse_tag_union_from_json : Str, ParseTagUnionSpec(a) -> Try({ value : a, rest : JsonState }, Json.DecodeErr)
parse_tag_union_from_json = |raw, spec| {
	remaining = Str.trim_start(raw)

	if !Str.starts_with(remaining, "{") {
		return Err(invalid_json)
	}

	after_open = Str.trim_start(Str.drop_prefix(remaining, "{"))

	if !Str.starts_with(after_open, "\"") {
		return Err(invalid_json)
	}

	key_split = split_json_string_tail(Str.drop_prefix(after_open, "\""))

	match key_split {
		Ok(key_parts) => {
			tag_name = key_parts.value
			after_key = Str.trim_start(key_parts.after)

			if !Str.starts_with(after_key, ":") {
				return Err(invalid_json)
			}

			payload = Str.trim_start(Str.drop_prefix(after_key, ":"))

			after_payload = if Str.starts_with(payload, "{") {
				object_end = find_object_end(payload)

				match object_end {
					Ok(end_parts) => Str.trim_start(end_parts.after)
					Err(_) => return Err(invalid_json)
				}
			} else if Str.starts_with(payload, "\"") {
				value_split = split_json_string_tail(Str.drop_prefix(payload, "\""))

				match value_split {
					Ok(value_parts) => Str.trim_start(value_parts.after)
					Err(_) => return Err(invalid_json)
				}
			} else {
				return Err(invalid_json)
			}

			if !Str.starts_with(after_payload, "}") {
				return Err(invalid_json)
			}

			value = ParseTagUnionSpec.parse(spec, {
				tag: tag_name,
				encoding: JsonEncoding.Default,
				state: JsonState.Input(payload),
				missing: Json.DecodeErr.MissingRequired,
			})?
			after_close = Str.trim_start(Str.drop_prefix(after_payload, "}"))

			Ok({ value, rest: JsonState.Input(after_close) })
		}
		Err(_) => Err(invalid_json)
	}
}

split_json_string_tail : Str -> Try({ value : Str, after : Str }, Json.DecodeErr)
split_json_string_tail = |tail| {
	quote_split = Str.find_first(tail, "\"")

	match quote_split {
		Ok(quote_parts) => {
			slash_split = Str.find_first(tail, "\\")

			match slash_split {
				Ok(slash_parts) =>
					if Str.count_utf8_bytes(slash_parts.before) < Str.count_utf8_bytes(quote_parts.before) {
						Err(invalid_json)
					} else {
						Ok({ value: quote_parts.before, after: quote_parts.after })
					}
				Err(NotFound) => Ok({ value: quote_parts.before, after: quote_parts.after })
			}
		}
		Err(NotFound) => Err(invalid_json)
	}
}

split_before : Try({ before : Str, after : Str }, [NotFound]), U64 -> Bool
split_before = |split, offset|
	match split {
		Ok(parts) => Str.count_utf8_bytes(parts.before) < offset
		Err(NotFound) => False
	}

find_object_end : Str -> Try({ after : Str }, Json.DecodeErr)
find_object_end = |object_text| {
	var $remaining = object_text
	var $depth = 0

	while True {
		quote_split = Str.find_first($remaining, "\"")
		open_split = Str.find_first($remaining, "{")
		close_split = Str.find_first($remaining, "}")
		var $skipped_string = False

		match quote_split {
			Ok(quote_parts) => {
				quote_offset = Str.count_utf8_bytes(quote_parts.before)

				if !split_before(open_split, quote_offset) {
					if !split_before(close_split, quote_offset) {
						string_parts = split_json_string_tail(quote_parts.after)?
						$remaining = string_parts.after
						$skipped_string = True
					}
				}
			}
			Err(NotFound) => {}
		}

		if $skipped_string == False {
			match open_split {
				Ok(open_parts) => {
					match close_split {
						Ok(close_parts) => {
							if Str.count_utf8_bytes(open_parts.before) < Str.count_utf8_bytes(close_parts.before) {
								$depth = $depth + 1
								$remaining = open_parts.after
							} else {
								if $depth == 0 {
									return Err(invalid_json)
								}

								$depth = $depth - 1
								$remaining = close_parts.after

								if $depth == 0 {
									return Ok({ after: $remaining })
								}
							}
						}
						Err(NotFound) => {
							$depth = $depth + 1
							$remaining = open_parts.after
						}
					}
				}
				Err(NotFound) => {
					match close_split {
						Ok(close_parts) => {
							if $depth == 0 {
								return Err(invalid_json)
							}

							$depth = $depth - 1
							$remaining = close_parts.after

							if $depth == 0 {
								return Ok({ after: $remaining })
							}
						}
						Err(NotFound) => return Err(invalid_json)
					}
				}
			}
		}
	}
}
