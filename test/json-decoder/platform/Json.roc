JsonState := [Input(Str)]

JsonEncoding :: [Default, CamelCase].{
	rename_field : JsonEncoding, Str -> Str
	rename_field = |encoding, name|
		match encoding {
			Default => name
			CamelCase => snake_to_camel(name)
		}

	parse_str : JsonEncoding, JsonState -> Try({ value : Str, rest : JsonState }, Json.DecodeErr)
	parse_str = |_, state|
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

	parse_record_field : JsonEncoding, Str.FieldName.FieldNames(_shape), JsonState -> Try(
		[
			Field({ field : Str.FieldName(_shape), rest : JsonState }),
			TryField({ name : Str, rest : JsonState }),
			TryFieldCaseless({ name : Str, rest : JsonState }),
			Continue({ rest : JsonState }),
			Done({ rest : JsonState }),
		],
		Json.DecodeErr,
	)
	parse_record_field = |_, _, state|
		match state {
			Input(raw) => parse_record_field_from_object(raw)
		}

	skip_record_field : JsonEncoding, JsonState -> Try(JsonState, Json.DecodeErr)
	skip_record_field = |_, state| skip_json_value(state)

	missing_record_field : JsonEncoding, Str, JsonState -> Json.DecodeErr
	missing_record_field = |_, _, _| Json.DecodeErr.MissingRequired

	missing_optional_field : JsonEncoding, Str, JsonState -> [Missing]
	missing_optional_field = |_, _, _| Missing

	parse_tag_union : JsonEncoding, ParseTagUnionSpec(a), JsonState -> Try({ value : a, rest : JsonState }, Json.DecodeErr)
	parse_tag_union = |encoding, spec, state|
		match state {
			Input(value) => parse_tag_union_from_json(value, encoding, spec)
		}
}

Json :: [].{
	DecodeErr := [MissingRequired, InvalidJson].{}

	Token := { raw : Str }.{
		parser_for : JsonEncoding -> (JsonState -> Try({ value : Token, rest : JsonState }, Json.DecodeErr))
		parser_for = |encoding| |state| {
			parsed = JsonEncoding.parse_str(encoding, state)?
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
			a.parser_for : JsonEncoding -> (JsonState -> Try({ value : a, rest : JsonState }, Json.DecodeErr)),
		]
	parser_camel = || {
		Shape : a
		parse_shape = Shape.parser_for(JsonEncoding.CamelCase)

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
}

invalid_json : Json.DecodeErr
invalid_json = Json.DecodeErr.InvalidJson

parse_record_field_from_object : Str -> Try(
	[
		Field({ field : Str.FieldName(_shape), rest : JsonState }),
		TryField({ name : Str, rest : JsonState }),
		TryFieldCaseless({ name : Str, rest : JsonState }),
		Continue({ rest : JsonState }),
		Done({ rest : JsonState }),
	],
	Json.DecodeErr,
)
parse_record_field_from_object = |raw| {
	remaining = Str.trim_start(raw)

	if Str.starts_with(remaining, "{") {
		return parse_record_field_from_object(Str.trim_start(Str.drop_prefix(remaining, "{")))
	}

	if Str.starts_with(remaining, ",") {
		return parse_record_field_from_object(Str.trim_start(Str.drop_prefix(remaining, ",")))
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

	Ok(TryField({ name: key, rest }))
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

parse_tag_union_from_json : Str, JsonEncoding, ParseTagUnionSpec(a) -> Try({ value : a, rest : JsonState }, Json.DecodeErr)
parse_tag_union_from_json = |raw, encoding, spec| {
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

				parsed = ParseTagUnionSpec.parse(spec, {
					tag: tag_name,
					encoding,
					state: JsonState.Input(payload),
					missing: Json.DecodeErr.MissingRequired,
				})?

				match parsed.rest {
					Input(after_payload) => finish_tag_payload(parsed.value, after_payload)
				}
		}
		Err(_) => Err(invalid_json)
	}
}

finish_tag_payload : a, Str -> Try({ value : a, rest : JsonState }, Json.DecodeErr)
finish_tag_payload = |value, raw| {
	remaining = Str.trim_start(raw)

	if Str.starts_with(remaining, "}") {
		after_close = Str.trim_start(Str.drop_prefix(remaining, "}"))
		return Ok({ value, rest: JsonState.Input(after_close) })
	}

	empty_payload = consume_empty_json_object(remaining)?
	after_payload = Str.trim_start(empty_payload.after)

	if Str.starts_with(after_payload, "}") {
		after_close = Str.trim_start(Str.drop_prefix(after_payload, "}"))
		Ok({ value, rest: JsonState.Input(after_close) })
	} else {
		Err(invalid_json)
	}
}

consume_empty_json_object : Str -> Try({ after : Str }, Json.DecodeErr)
consume_empty_json_object = |raw| {
	remaining = Str.trim_start(raw)

	if !Str.starts_with(remaining, "{") {
		return Err(invalid_json)
	}

	after_open = Str.trim_start(Str.drop_prefix(remaining, "{"))

	if Str.starts_with(after_open, "}") {
		Ok({ after: Str.drop_prefix(after_open, "}") })
	} else {
		Err(invalid_json)
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
