Headers :: [].{
	DecodeErr := [MissingRequired, BadHeader].{}

	decode : Str -> Try(a, DecodeErr) where [a.decoder : () -> Decoder(a)]
	decode = |headers| {
		Shape : a
		Shape.decoder().decode(HeaderFormat.Present(headers))
	}
}

HeaderFormat :: [Present(Str), Missing].{
	decode_str : DecoderStrSpec(a), HeaderFormat -> Try(a, Headers.DecodeErr)
	decode_str = |spec, slot|
		Decoder.decode_str(spec, slot, Headers.DecodeErr.MissingRequired)

	decode_record : DecoderRecordSpec(a), HeaderFormat -> Try(a, Headers.DecodeErr)
	decode_record = |spec, slot|
		match slot {
			Present(value) => decode_record_from_headers(value, spec)
			Missing => Err(Headers.DecodeErr.MissingRequired)
		}

	decode_tag_union : DecoderTagUnionSpec(a), HeaderFormat -> Try(a, Headers.DecodeErr)
	decode_tag_union = |spec, slot|
		match slot {
			Present(value) => decode_tag_union_from_header(value, spec)
			Missing => Err(Headers.DecodeErr.MissingRequired)
		}
}

decode_record_from_headers : Str, DecoderRecordSpec(a) -> Try(a, Headers.DecodeErr)
decode_record_from_headers = |headers, spec| {
	var $state = Decoder.Record.init(spec, HeaderFormat.Missing)
	var $remaining = headers

	while True {
		match $remaining.find_first("\r\n") {
			Ok(line_parts) => {
				line = line_parts.before

				if line.is_empty() {
					break
				} else {
					match line.find_first(":") {
						Ok({ before: name, after: value }) => {
							$state = Decoder.Record.put(
								spec,
								$state,
								name,
								HeaderFormat.Present(value.trim()),
								header_name_matches_field,
							)
						}
						Err(NotFound) => {
							return Err(Headers.DecodeErr.BadHeader)
						}
					}

					$remaining = line_parts.after
				}
			}
			Err(NotFound) => {
				break
			}
		}
	}

	Decoder.Record.finish(spec, $state, Headers.DecodeErr.MissingRequired)
}

decode_tag_union_from_header : Str, DecoderTagUnionSpec(a) -> Try(a, Headers.DecodeErr)
decode_tag_union_from_header = |tag_name, spec|
	Decoder.TagUnion.decode(spec, tag_name, HeaderFormat.Present(tag_name), Str.is_eq, Headers.DecodeErr.MissingRequired)

header_name_matches_field : Str, Str -> Bool
header_name_matches_field = |header_name, field_name| {
	var $header_remaining = header_name
	var $field_remaining = field_name
	var $matched = True
	var $keep_scanning = True

	while $keep_scanning {
		header_split = Str.find_first($header_remaining, "-")
		field_split = Str.find_first($field_remaining, "_")

		(header_found, header_segment, header_after) = match header_split {
			Ok(split) => (True, split.before, split.after)
			Err(NotFound) => (False, $header_remaining, "")
		}
		(field_found, field_segment, field_after) = match field_split {
			Ok(split) => (True, split.before, split.after)
			Err(NotFound) => (False, $field_remaining, "")
		}

		if !Str.caseless_ascii_equals(header_segment, field_segment) {
			$matched = False
			$keep_scanning = False
		} else if header_found != field_found {
			$matched = False
			$keep_scanning = False
		} else if header_found {
			$header_remaining = header_after
			$field_remaining = field_after
		} else {
			$keep_scanning = False
		}
	}

	$matched
}
