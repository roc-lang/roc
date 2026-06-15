import Decoder exposing [Decoder, Decoder.*]

Headers :: { raw : Str }.{
	decode : Headers, Decoder(a) -> a
	decode = |headers, decoder|
		match decoder {
			Record2(first_name, second_name, build) =>
				decode_record2(headers.raw, first_name, second_name, build)
		}

	decode_record2 : Str, Str, Str, (Str, Str -> a) -> a
	decode_record2 = |headers, first_name, second_name, build| {
		request_line = Str.find_first(headers, "\r\n")
		var $remaining = if request_line.found {
			request_line.after
		} else {
			""
		}
		var $first_value = ""
		var $second_value = ""
		var $found_first = False
		var $found_second = False
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

						if header_name_matches_field(name, first_name) {
							$first_value = value
							$found_first = True
						} else if header_name_matches_field(name, second_name) {
							$second_value = value
							$found_second = True
						} else {
							{}
						}
					} else {
						{}
					}

					$remaining = line_split.after
				}
			} else {
				$keep_scanning = False
			}
		}

		if $found_first and $found_second {
			build($first_value, $second_value)
		} else {
			crash "missing required HTTP header"
		}
	}

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
