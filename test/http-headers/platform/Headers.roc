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
	var $state = Decoder.Record.init(spec, Missing)
	var $remaining = headers

	while True {
		match $remaining.find_first("\r\n") {
			Ok({ before, after }) if !before.is_empty() => {
				match before.find_first(":") {
					Ok({ before: name, after: value }) => {
						$state = Decoder.Record.put(
							spec,
							$state,
							name,
							HeaderFormat.Present(value.trim()),
							header_name_matches_field,
						)
					}
					Err(NotFound) => return Err(BadHeader)
				}

				$remaining = after
			}
			Err(NotFound) | Ok(_) => break
		}
	}

	Decoder.Record.finish(spec, $state, MissingRequired)
}

decode_tag_union_from_header : Str, DecoderTagUnionSpec(a) -> Try(a, Headers.DecodeErr)
decode_tag_union_from_header = |tag_name, spec|
	Decoder.TagUnion.decode(spec, tag_name, HeaderFormat.Present(tag_name), Str.is_eq, Headers.DecodeErr.MissingRequired)

header_name_matches_field : Str, Str -> Bool
header_name_matches_field = |header_name, field_name| {
	var $header_remaining = header_name
	var $field_remaining = field_name

	while True {
	    # Find the next segment in the header and the next segment in the field.
	    # Delimit header segments by "-" and field segments by "_" so that
		# (for example) a header like "X-API-Key" becomes the field name "x_api_key"
	    header_split = Str.find_first($header_remaining, "-")
	    field_split = Str.find_first($field_remaining, "_")

		match header_split {
			Ok({ before: header_segment, after: header_after }) => {
          		match field_split {
         			Ok({ before: field_segment, after: field_after }) => {
                  		if Str.caseless_ascii_equals(header_segment, field_segment) {
                            # The segments matched; continue.
                 			$header_remaining = header_after
                 			$field_remaining = field_after
                  		} else {
                 			break
                  		}
                    }
         			Err(NotFound) => {
         			    # Field had a separator left but header didn't; they don't match.
         			    break
         			}
          		}
			}
			Err(NotFound) => {
			    match field_split {
					Err(NotFound) => {
					    # Neither header nor field had any separators left. Good!
					    # Compare their final segments and we're done.
             			return Str.caseless_ascii_equals($header_remaining, $field_remaining)
					}
					Ok(_) => {
					    # Header had a separator left but field didn't; they don't match.
					    break
					}
				}
			}
		}
	}

	False
}
