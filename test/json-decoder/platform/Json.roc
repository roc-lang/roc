DecodeErr := [MissingRequired, InvalidJson].{}

JsonFormat :: [Present(Str), Missing].{
	parse_str : ParseStrSpec(a), JsonFormat -> Try({ value : a, rest : JsonFormat }, DecodeErr)
	parse_str = |spec, slot| {
		value = ParseStrSpec.parse(spec, slot, DecodeErr.MissingRequired)?
		Ok({ value, rest: Missing })
	}

	parse_record : ParseRecordSpec(a), JsonFormat -> Try({ value : a, rest : JsonFormat }, DecodeErr)
	parse_record = |spec, slot|
		match slot {
			Present(raw) => {
				value = parse_record_from_json(raw, spec)?
				Ok({ value, rest: Missing })
			}
			Missing => Err(DecodeErr.MissingRequired)
		}

	parse_tag_union : ParseTagUnionSpec(a), JsonFormat -> Try({ value : a, rest : JsonFormat }, DecodeErr)
	parse_tag_union = |spec, slot|
		match slot {
			Present(value) => {
				parsed = parse_tag_union_from_json(value, spec)?
				Ok({ value: parsed, rest: Missing })
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

	parse : Str -> Try(a, DecodeErr) where [
		a.parse_from : JsonFormat -> Try({ value : a, rest : JsonFormat }, DecodeErr),
	]
	parse = |json| {
		Shape : a
		parsed = Shape.parse_from(JsonFormat.Present(json))?
		Ok(parsed.value)
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
			key_split = Str.find_first(Str.drop_prefix($remaining, "\""), "\"")

			match key_split {
				Ok(key_parts) => {
					key = key_parts.before
					after_key = Str.trim_start(key_parts.after)

					if Str.starts_with(after_key, ":") {
						after_colon = Str.trim_start(Str.drop_prefix(after_key, ":"))

						after_value = if Str.starts_with(after_colon, "\"") {
							value_split = Str.find_first(Str.drop_prefix(after_colon, "\""), "\"")

							match value_split {
								Ok(value_parts) => {
									$state = ParseRecordSpec.put(spec, $state, key, JsonFormat.Present(value_parts.before), Str.is_eq)
									Str.trim_start(value_parts.after)
								}
								Err(NotFound) => {
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
								Err(NotFound) => {
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
				Err(NotFound) => {
					return Err(invalid_json)
				}
			}
		} else {
			return Err(invalid_json)
		}
	}

	ParseRecordSpec.finish(spec, $state, DecodeErr.MissingRequired)
}

parse_tag_union_from_json : Str, ParseTagUnionSpec(a) -> Try(a, DecodeErr)
parse_tag_union_from_json = |tag_name, spec|
	ParseTagUnionSpec.parse(spec, tag_name, JsonFormat.Present(tag_name), Str.is_eq, DecodeErr.MissingRequired)

find_object_end : Str -> Try({ before : Str, after : Str }, [NotFound])
find_object_end = |object_text| {
	close_split = Str.find_first(Str.drop_prefix(object_text, "{"), "}")
	close_split
}
