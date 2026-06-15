Headers :: { raw : Str }.{
	FieldValue :: [Present(Str), Missing].{}
	HeaderFormat :: [Default].{
		default : () -> HeaderFormat
		default = || HeaderFormat.Default

		init : HeaderFormat, Headers -> FieldValue
		init = |_, headers|
			FieldValue.Present(headers.raw)

		decode_str : DecoderStrSpec(a), FieldValue -> a
		decode_str = |spec, slot|
			decode_str(spec, slot)

		decode_record : DecoderRecordSpec(a), FieldValue -> a
		decode_record = |spec, slot| decode_record_slot(spec, slot)

		decode_tag_union : DecoderTagUnionSpec(a), FieldValue -> a
		decode_tag_union = |spec, slot| decode_tag_union_slot(spec, slot)
	}

	decode : Headers -> a where [a.decoder : () -> Decoder(a)]
	decode = |headers| {
		Shape : a
		Shape.decoder().decode(headers, HeaderFormat.default())
	}

	decode_record_from_headers : Str, DecoderRecordSpec(a) -> a
	decode_record_from_headers = |headers, spec| {
		request_line = Str.find_first(headers, "\r\n")
		var $remaining = if request_line.found {
			request_line.after
		} else {
			""
		}
		var $state = Decoder.Record.init(spec, FieldValue.Missing)
		var $keep_scanning = request_line.found

		while $keep_scanning {
			line_split = Str.find_first($remaining, "\r\n")

			if line_split.found {
				line = line_split.before

				if Str.is_empty(line) {
					$keep_scanning = False
				} else {
					header_split = Str.find_first(line, ":")

					if header_split.found {
						name = header_split.before
						value = Str.trim(header_split.after)

						$state = Decoder.Record.put(spec, $state, name, FieldValue.Present(value), header_name_matches_field)
					} else {
						{}
					}

					$remaining = line_split.after
				}
			} else {
				$keep_scanning = False
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
			Present(value) => decode_record_from_headers(value, spec)
			Missing => {
				crash "missing required decoded field"
			}
		}

	decode_tag_union_slot : DecoderTagUnionSpec(a), FieldValue -> a
	decode_tag_union_slot = |spec, slot|
		match slot {
			Present(value) => decode_tag_union_from_header(value, spec)
			Missing => {
				crash "missing required decoded field"
			}
		}

	decode_tag_union_from_header : Str, DecoderTagUnionSpec(a) -> a
	decode_tag_union_from_header = |tag_name, spec|
		Decoder.TagUnion.decode(spec, tag_name, FieldValue.Present(tag_name), Str.is_eq, decode_slot)

	header_name_matches_field : Str, Str -> Bool
	header_name_matches_field = |header_name, field_name| {
		var $header_remaining = header_name
		var $field_remaining = field_name
		var $matched = True
		var $keep_scanning = True

		while $keep_scanning {
			header_split = Str.find_first($header_remaining, "-")
			field_split = Str.find_first($field_remaining, "_")

			header_segment = if header_split.found {
				header_split.before
			} else {
				$header_remaining
			}
			field_segment = if field_split.found {
				field_split.before
			} else {
				$field_remaining
			}

			if !Str.caseless_ascii_equals(header_segment, field_segment) {
				$matched = False
				$keep_scanning = False
			} else if header_split.found != field_split.found {
				$matched = False
				$keep_scanning = False
			} else if header_split.found {
				$header_remaining = header_split.after
				$field_remaining = field_split.after
			} else {
				$keep_scanning = False
			}
		}

		$matched
	}
}
