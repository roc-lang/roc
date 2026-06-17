Headers :: [].{
	DecodeErr := [MissingRequired, BadHeader].{}

	parse : Str -> Try(output, DecodeErr)
		where [
			output.parse_from : HeaderFormat -> Try({ value : output, rest : HeaderFormat }, DecodeErr),
		]
	parse = |headers| {
		Output : output

		{ value, rest: _ } = Output.parse_from(HeaderFormat.Headers(headers))?

		Ok(value)
	}
}

HeaderFormat :: [Headers(Str), Value(Str)].{
	parse_str : HeaderFormat -> Try({ value : Str, rest : HeaderFormat }, Headers.DecodeErr)
	parse_str = |slot|
		match slot {
			Value(value) => Ok({ value, rest: Value("") })
			Headers(_) => Err(Headers.DecodeErr.BadHeader)
		}

	parse_record : ParseRecordSpec(a), HeaderFormat -> Try({ value : a, rest : HeaderFormat }, Headers.DecodeErr)
	parse_record = |spec, slot|
		match slot {
			Headers(headers) => {
				value = parse_record_from_headers(headers, spec, slot)?
				Ok({ value, rest: Value("") })
			}
			Value(_) => Err(Headers.DecodeErr.BadHeader)
		}

	parse_tag_union : ParseTagUnionSpec(a), HeaderFormat -> Try({ value : a, rest : HeaderFormat }, Headers.DecodeErr)
	parse_tag_union = |spec, slot|
		match slot {
			Value(tag_name) => {
				value = parse_tag_union_from_header(tag_name, spec)?
				Ok({ value, rest: Value("") })
			}
			Headers(_) => Err(Headers.DecodeErr.BadHeader)
		}
}

parse_record_from_headers : Str, ParseRecordSpec(a), HeaderFormat -> Try(a, Headers.DecodeErr)
parse_record_from_headers = |headers, spec, slot| {
	var $state = ParseRecordSpec.init(spec, slot)
	var $remaining = headers

	while True {
		match $remaining.find_first("\r\n") {
			Ok({ before, after }) if !before.is_empty() => {
				match before.find_first(":") {
					Ok({ before: name, after: value }) => {
						$state = ParseRecordSpec.put(
							spec,
							$state,
							name,
							HeaderFormat.Value(value.trim()),
							header_name_matches_field,
						)
					}
					Err(NotFound) => return Err(BadHeader)
				}

				$remaining = after
			}
			Err(NotFound) | Ok(_) => return ParseRecordSpec.finish(spec, $state, MissingRequired)
		}
	}
}

parse_tag_union_from_header : Str, ParseTagUnionSpec(a) -> Try(a, Headers.DecodeErr)
parse_tag_union_from_header = |tag_name, spec|
	ParseTagUnionSpec.parse(spec, tag_name, HeaderFormat.Value(tag_name), Str.is_eq, Headers.DecodeErr.MissingRequired)

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
							return False
						}
					}
					Err(NotFound) => {
						# Field had a separator left but header didn't; they don't match.
						return False
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
						return False
					}
				}
			}
		}
	}
}
