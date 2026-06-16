DecodeErr := [MissingRequired, InvalidJson].{}

JsonFormat :: [Present(Str), Missing].{
	parse_str : ParseStrSpec(a), JsonFormat -> Try({ value : a, rest : JsonFormat }, DecodeErr)
	parse_str = |spec, slot|
		match slot {
			Present(raw) => {
				trimmed = Str.trim_start(raw)
				string_value = if Str.starts_with(trimmed, "\"") {
					string_parts = split_json_string_tail(Str.drop_prefix(trimmed, "\""))?
					rest = Str.trim_start(string_parts.after)

					if !Str.is_empty(rest) {
						return Err(invalid_json)
					}

					string_parts.value
				} else {
					raw
				}

				value = ParseStrSpec.parse(spec, JsonFormat.Present(string_value), DecodeErr.MissingRequired)?
				Ok({ value, rest: JsonFormat.Missing })
			}
			Missing => Err(DecodeErr.MissingRequired)
		}

	parse_record : ParseRecordSpec(a), JsonFormat -> Try({ value : a, rest : JsonFormat }, DecodeErr)
	parse_record = |spec, slot|
		match slot {
			Present(raw) => {
				record_end = find_object_end(Str.trim_start(raw))?
				value = parse_record_from_json(raw, spec)?
				after_record = Str.trim_start(record_end.after)
				rest = if Str.is_empty(after_record) {
					JsonFormat.Missing
				} else {
					JsonFormat.Present(after_record)
				}

				Ok({ value, rest })
			}
			Missing => Err(DecodeErr.MissingRequired)
		}

	parse_tag_union : ParseTagUnionSpec(a), JsonFormat -> Try({ value : a, rest : JsonFormat }, DecodeErr)
	parse_tag_union = |spec, slot|
		match slot {
			Present(value) => {
				parsed = parse_tag_union_from_json(value, spec)?
				Ok(parsed)
			}
			Missing => Err(DecodeErr.MissingRequired)
		}
}

Json :: [].{
	Token := { raw : Str }.{
		parse_from : JsonFormat -> Try({ value : Token, rest : JsonFormat }, DecodeErr)
		parse_from = |slot|
			match slot {
				Present(_) => Ok({ value: { raw: "custom-token" }, rest: Missing })
				Missing => Err(DecodeErr.MissingRequired)
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
		parsed = Shape.parse_from(JsonFormat.Present(json))?

		match parsed.rest {
			Missing => Ok(parsed.value)
			Present(rest) =>
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

parse_record_from_json : Str, ParseRecordSpec(a) -> Try(a, DecodeErr)
parse_record_from_json = |raw, spec| {
	var $remaining = Str.trim_start(raw)
	var $state = ParseRecordSpec.init(spec, JsonFormat.Missing)
	var $keep_scanning = True

	if Str.starts_with($remaining, "{") {
		$remaining = Str.trim_start(Str.drop_prefix($remaining, "{"))
	} else {
		return Err(invalid_json)
	}

	while $keep_scanning {
		if Str.starts_with($remaining, "}") {
			$keep_scanning = False
		} else if Str.starts_with($remaining, "\"") {
			key_split = split_json_string_tail(Str.drop_prefix($remaining, "\""))

			match key_split {
				Ok(key_parts) => {
					key = key_parts.value
					after_key = Str.trim_start(key_parts.after)

					if Str.starts_with(after_key, ":") {
						after_colon = Str.trim_start(Str.drop_prefix(after_key, ":"))

						after_value = if Str.starts_with(after_colon, "\"") {
							value_split = split_json_string_tail(Str.drop_prefix(after_colon, "\""))

							match value_split {
								Ok(value_parts) => {
									$state = ParseRecordSpec.put(spec, $state, key, JsonFormat.Present(value_parts.value), Str.is_eq)
									Str.trim_start(value_parts.after)
								}
								Err(_) => {
									return Err(invalid_json)
								}
							}
						} else if Str.starts_with(after_colon, "{") {
							object_end = find_object_end(after_colon)

							match object_end {
								Ok(end_parts) => {
									$state = ParseRecordSpec.put(spec, $state, key, JsonFormat.Present(after_colon), Str.is_eq)
									Str.trim_start(end_parts.after)
								}
								Err(_) => {
									return Err(invalid_json)
								}
							}
						} else {
							return Err(invalid_json)
						}

						if Str.starts_with(after_value, ",") {
							$remaining = Str.trim_start(Str.drop_prefix(after_value, ","))
						} else if Str.starts_with(after_value, "}") {
							$keep_scanning = False
						} else {
							return Err(invalid_json)
						}
					} else {
						return Err(invalid_json)
					}
				}
				Err(_) => {
					return Err(invalid_json)
				}
			}
		} else {
			return Err(invalid_json)
		}
	}

	ParseRecordSpec.finish(spec, $state, DecodeErr.MissingRequired)
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

			parsed = ParseTagUnionSpec.parse(spec, tag_name, JsonFormat.Present(payload), Str.is_eq, DecodeErr.MissingRequired)?
			after_close = Str.trim_start(Str.drop_prefix(after_payload, "}"))
			rest = if Str.is_empty(after_close) {
				JsonFormat.Missing
			} else {
				JsonFormat.Present(after_close)
			}

			Ok({ value: parsed, rest })
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
