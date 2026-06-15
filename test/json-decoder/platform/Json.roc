Json :: { raw : Str }.{
	FieldValue :: [Present(Str), Missing].{}

	decode : Json, Decoder(a) -> a
	decode = |json, decoder|
		Decoder.dispatch(decoder, FieldValue.Present(json.raw), decode_str, decode_record_slot, decode_tag_union_slot)

	decode_record : Str, DecoderRecordSpec(a) -> a
	decode_record = |raw, spec| {
		var $remaining = Str.trim_start(raw)
		var $state = Decoder.Record.init(spec, FieldValue.Missing)
		var $keep_scanning = True

		if Str.starts_with($remaining, "{") {
			$remaining = Str.trim_start(Str.drop_prefix($remaining, "{"))
		} else {
			crash "invalid JSON"
		}

		while $keep_scanning {
			if Str.starts_with($remaining, "}") {
				$keep_scanning = False
			} else if Str.starts_with($remaining, "\"") {
				key_split = Str.find_first(Str.drop_prefix($remaining, "\""), "\"")

				if key_split.found {
					key = key_split.before
					after_key = Str.trim_start(key_split.after)

					if Str.starts_with(after_key, ":") {
						after_colon = Str.trim_start(Str.drop_prefix(after_key, ":"))

						after_value = if Str.starts_with(after_colon, "\"") {
							value_split = Str.find_first(Str.drop_prefix(after_colon, "\""), "\"")

							if value_split.found {
								$state = Decoder.Record.put(spec, $state, key, FieldValue.Present(value_split.before), Str.is_eq)
								Str.trim_start(value_split.after)
							} else {
								crash "invalid JSON"
							}
						} else if Str.starts_with(after_colon, "{") {
							object_end = find_object_end(after_colon)

							if object_end.found {
								$state = Decoder.Record.put(spec, $state, key, FieldValue.Present(after_colon), Str.is_eq)
								Str.trim_start(object_end.after)
							} else {
								crash "invalid JSON"
							}
						} else {
							crash "invalid JSON"
						}

						if Str.starts_with(after_value, ",") {
							$remaining = Str.trim_start(Str.drop_prefix(after_value, ","))
						} else if Str.starts_with(after_value, "}") {
							$keep_scanning = False
						} else {
							crash "invalid JSON"
						}
					} else {
						crash "invalid JSON"
					}
				} else {
					crash "invalid JSON"
				}
			} else {
				crash "invalid JSON"
			}
		}

		Decoder.Record.finish(spec, $state, decode_slot)
	}

	decode_slot : FieldValue, Decoder(a) -> a
	decode_slot = |slot, decoder|
		Decoder.dispatch(decoder, slot, decode_str, decode_record_slot, decode_tag_union_slot)

	decode_str : DecoderStrSpec(a), FieldValue -> a
	decode_str = |spec, slot|
		Decoder.decode_str(spec, slot)

	decode_record_slot : DecoderRecordSpec(a), FieldValue -> a
	decode_record_slot = |spec, slot|
		match slot {
			Present(value) => decode_record(value, spec)
			Missing => {
				crash "missing required decoded field"
			}
		}

	decode_tag_union_slot : DecoderTagUnionSpec(a), FieldValue -> a
	decode_tag_union_slot = |spec, slot|
		match slot {
			Present(value) =>
				decode_tag_union(value, spec)
			Missing => {
				crash "missing required decoded field"
			}
		}

	decode_tag_union : Str, DecoderTagUnionSpec(a) -> a
	decode_tag_union = |tag_name, spec|
		Decoder.TagUnion.decode(spec, tag_name, FieldValue.Present(tag_name), Str.is_eq, decode_slot)

	find_object_end : Str -> { after : Str, found : Bool }
	find_object_end = |object_text| {
		close_split = Str.find_first(Str.drop_prefix(object_text, "{"), "}")
		{ after: close_split.after, found: close_split.found }
	}
}
