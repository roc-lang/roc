DecodeErr := [MissingRequired, InvalidJson].{}

JsonFormat :: [Input(Str), Object(Str), Value(Str)].{
	parse_str : JsonFormat -> Try({ value : Str, rest : JsonFormat }, DecodeErr)
	parse_str = |slot|
		match slot {
			Input(raw) => {
				trimmed = Str.trim_start(raw)
				if Str.starts_with(trimmed, "\"") {
					string_parts = split_json_string_tail(Str.drop_prefix(trimmed, "\""))?
					rest = Str.trim_start(string_parts.after)
					Ok({ value: string_parts.value, rest: Input(rest) })
				} else {
					Err(invalid_json)
				}
			}
			Object(_) => Err(invalid_json)
			Value(value) => Ok({ value, rest: Value("") })
		}

	parse_record_field : U64, JsonFormat -> Try(
		[
			Field({ name : Str, value : JsonFormat, rest : JsonFormat }),
			Continue({ rest : JsonFormat }),
			Done({ rest : JsonFormat }),
		],
		DecodeErr,
	)
	parse_record_field = |longest_field_len, slot|
		match slot {
			Input(raw) => {
				trimmed = Str.trim_start(raw)
				if Str.starts_with(trimmed, "{") {
					parse_record_field_from_object(longest_field_len, Str.trim_start(Str.drop_prefix(trimmed, "{")))
				} else {
					Err(invalid_json)
				}
			}
			Object(raw) => parse_record_field_from_object(longest_field_len, raw)
			Value(_) => Err(invalid_json)
		}

	missing_record_field : Str, JsonFormat -> DecodeErr
	missing_record_field = |_, _| DecodeErr.MissingRequired

	parse_tag_union : ParseTagUnionSpec(a), JsonFormat -> Try({ value : a, rest : JsonFormat }, DecodeErr)
	parse_tag_union = |spec, slot|
		match slot {
			Input(value) => {
				parsed = parse_tag_union_from_json(value, spec)?
				Ok(parsed)
			}
			Object(_) => Err(invalid_json)
			Value(_) => Err(invalid_json)
		}
}

Json :: [].{
	Token := { raw : Str }.{
		parse_from : JsonFormat -> Try({ value : Token, rest : JsonFormat }, DecodeErr)
		parse_from = |slot|
			match slot {
				Input(_) | Object(_) | Value(_) => Ok({ value: { raw: "custom-token" }, rest: Value("") })
			}

		count_utf8_bytes : Token -> U64
		count_utf8_bytes = |token| Str.count_utf8_bytes(token.raw)
	}

	parse : Str -> Try(a, DecodeErr)
		where [
			a.parse_from : JsonFormat -> Try({ value : a, rest : JsonFormat }, DecodeErr),
		]
	parse = |json| {
		Shape : a
		parsed = Shape.parse_from(JsonFormat.Input(json))?

		match parsed.rest {
			Value(_) => Ok(parsed.value)
			Object(_) => Err(invalid_json)
			Input(rest) =>
				if Str.is_empty(Str.trim_start(rest)) {
					Ok(parsed.value)
				} else {
					Err(invalid_json)
				}
			}
	}
}

invalid_json : DecodeErr
invalid_json = DecodeErr.InvalidJson

parse_record_field_from_object : U64, Str -> Try(
	[
		Field({ name : Str, value : JsonFormat, rest : JsonFormat }),
		Continue({ rest : JsonFormat }),
		Done({ rest : JsonFormat }),
	],
	DecodeErr,
)
parse_record_field_from_object = |longest_field_len, raw| {
	var $remaining = Str.trim_start(raw)

	if Str.starts_with($remaining, "}") {
		after_record = Str.trim_start(Str.drop_prefix($remaining, "}"))
		return Ok(Done({ rest: JsonFormat.Input(after_record) }))
	}

	if !Str.starts_with($remaining, "\"") {
		return Err(invalid_json)
	}

	key_parts = split_json_string_tail(Str.drop_prefix($remaining, "\""))?
	key = key_parts.value
	after_key = Str.trim_start(key_parts.after)

	if !Str.starts_with(after_key, ":") {
		return Err(invalid_json)
	}

	after_colon = Str.trim_start(Str.drop_prefix(after_key, ":"))

	value_and_after = if Str.starts_with(after_colon, "\"") {
		value_parts = split_json_string_tail(Str.drop_prefix(after_colon, "\""))?
		{ value: JsonFormat.Value(value_parts.value), after: Str.trim_start(value_parts.after) }
	} else if Str.starts_with(after_colon, "{") {
		end_parts = find_object_end(after_colon)?
		{ value: JsonFormat.Input(after_colon), after: Str.trim_start(end_parts.after) }
	} else {
		return Err(invalid_json)
	}

	next_remaining =
		if Str.starts_with(value_and_after.after, ",") {
			Str.trim_start(Str.drop_prefix(value_and_after.after, ","))
		} else if Str.starts_with(value_and_after.after, "}") {
			value_and_after.after
		} else {
			return Err(invalid_json)
		}

	if Str.count_utf8_bytes(key) > longest_field_len {
		Ok(Continue({ rest: JsonFormat.Object(next_remaining) }))
	} else {
		Ok(Field({
			name: key,
			value: value_and_after.value,
			rest: JsonFormat.Object(next_remaining),
		}))
	}
}

parse_tag_union_from_json : Str, ParseTagUnionSpec(a) -> Try({ value : a, rest : JsonFormat }, DecodeErr)
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

			parsed = ParseTagUnionSpec.parse(spec, tag_name, JsonFormat.Input(payload), DecodeErr.MissingRequired)?
			after_close = Str.trim_start(Str.drop_prefix(after_payload, "}"))

			Ok({ value: parsed, rest: JsonFormat.Input(after_close) })
		}
		Err(_) => Err(invalid_json)
	}
}

split_json_string_tail : Str -> Try({ value : Str, after : Str }, DecodeErr)
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

find_object_end : Str -> Try({ after : Str }, DecodeErr)
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
