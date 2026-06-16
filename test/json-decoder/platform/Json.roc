DecodeErr := [MissingRequired, InvalidJson].{}

JsonFormat :: [Present(Str), Missing].{
	decode_str : DecoderStrSpec(a), JsonFormat -> Try(a, DecodeErr)
	decode_str = |spec, slot|
		Decoder.decode_str(spec, slot, DecodeErr.MissingRequired)

	decode_record : DecoderRecordSpec(a), JsonFormat -> Try(a, DecodeErr)
	decode_record = |spec, slot|
		match slot {
			Present(value) => decode_record_from_json(value, spec)
			Missing => Err(DecodeErr.MissingRequired)
		}

	decode_tag_union : DecoderTagUnionSpec(a), JsonFormat -> Try(a, DecodeErr)
	decode_tag_union = |spec, slot|
		match slot {
			Present(value) =>
				decode_tag_union_from_json(value, spec)
			Missing => Err(DecodeErr.MissingRequired)
		}
}

Json :: [].{
	decode : Str -> Try(a, DecodeErr) where [a.decoder : () -> Decoder(a)]
	decode = |json| {
		Shape : a
		Shape.decoder().decode(JsonFormat.Present(json))
	}
}

invalid_json : DecodeErr
invalid_json = DecodeErr.InvalidJson

decode_record_from_json : Str, DecoderRecordSpec(a) -> Try(a, DecodeErr)
decode_record_from_json = |raw, spec| {
	var $remaining = Str.trim_start(raw)
	var $state = Decoder.Record.init(spec, JsonFormat.Missing)
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
									$state = Decoder.Record.put(spec, $state, key, JsonFormat.Present(value_parts.before), Str.is_eq)
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
									$state = Decoder.Record.put(spec, $state, key, JsonFormat.Present(after_colon), Str.is_eq)
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

	Decoder.Record.finish(spec, $state, DecodeErr.MissingRequired)
}

decode_tag_union_from_json : Str, DecoderTagUnionSpec(a) -> Try(a, DecodeErr)
decode_tag_union_from_json = |tag_name, spec|
	Decoder.TagUnion.decode(spec, tag_name, JsonFormat.Present(tag_name), Str.is_eq, DecodeErr.MissingRequired)

find_object_end : Str -> Try({ before : Str, after : Str }, [NotFound])
find_object_end = |object_text| {
	close_split = Str.find_first(Str.drop_prefix(object_text, "{"), "}")
	close_split
}
