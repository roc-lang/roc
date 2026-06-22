Headers := { raw : Str }.{
	DecodeErr := [MissingRequired, BadHeader].{}

	parser_for : () -> (Headers -> Try({ value : output, rest : Headers }, DecodeErr))
		where [
			output.parser_for : HeaderEncoding -> (Headers -> Try({ value : output, rest : Headers }, DecodeErr)),
		]
	parser_for = || {
		Output : output

		parse_output : Headers -> Try({ value : output, rest : Headers }, DecodeErr)
		parse_output = Output.parser_for(HeaderEncoding.Caseless)

		parse_output
	}

	parse : Str -> Try(output, DecodeErr)
		where [
			output.parser_for : HeaderEncoding -> (Headers -> Try({ value : output, rest : Headers }, DecodeErr)),
		]
	parse = |raw| {
		Output : output

		parse_output = Output.parser_for(HeaderEncoding.Caseless)
		parsed = parse_output(Headers.{ raw })?

		Ok(parsed.value)
	}
}

HeaderEncoding :: [Caseless].{
	rename_field : HeaderEncoding, Str -> Str
	rename_field = |_, name| underscores_to_dashes(name)

	parse_str : HeaderEncoding, Headers -> Try({ value : Str, rest : Headers }, Headers.DecodeErr)
	parse_str = |_, state| {
		value_parts = take_header_value(state.raw)?
		Ok({ value: value_parts.value, rest: { raw: value_parts.after } })
	}

	parse_u64 : HeaderEncoding, Headers -> Try({ value : U64, rest : Headers }, Headers.DecodeErr)
	parse_u64 = |_, state| {
		value_parts = take_header_value(state.raw)?

		match U64.from_str(value_parts.value) {
			Ok(value) => Ok({ value, rest: { raw: value_parts.after } })
			Err(_) => Err(Headers.DecodeErr.BadHeader)
		}
	}

	parse_record_field : HeaderEncoding, Str.FieldName.FieldNames(_shape), Headers -> Try(
		[
			Field({ field : Str.FieldName(_shape), rest : Headers }),
			TryField({ name : Str, rest : Headers }),
			TryFieldCaseless({ name : Str, rest : Headers }),
			Continue({ rest : Headers }),
			Done({ rest : Headers }),
		],
		Headers.DecodeErr,
	)
	parse_record_field = |_, fields, state|
		parse_record_field_from_headers(fields, state.raw)

	skip_record_field : HeaderEncoding, Headers -> Try(Headers, Headers.DecodeErr)
	skip_record_field = |_, state| {
		parts = take_header_value(state.raw)?
		Ok({ raw: parts.after })
	}

	missing_record_field : HeaderEncoding, Str, Headers -> Headers.DecodeErr
	missing_record_field = |_, _, _| Headers.DecodeErr.MissingRequired

	missing_optional_field : HeaderEncoding, Str, Headers -> [Missing]
	missing_optional_field = |_, _, _| Missing
}

parse_record_field_from_headers : Str.FieldName.FieldNames(_shape), Str -> Try(
	[
		Field({ field : Str.FieldName(_shape), rest : Headers }),
		TryField({ name : Str, rest : Headers }),
		TryFieldCaseless({ name : Str, rest : Headers }),
		Continue({ rest : Headers }),
		Done({ rest : Headers }),
	],
	Headers.DecodeErr,
)
parse_record_field_from_headers = |fields, headers|
	if headers.is_empty() {
		Ok(Done({ rest: { raw: "" } }))
	} else {
		line_parts = match headers.find_first("\r\n") {
			Ok(parts) => parts
			Err(NotFound) => { before: headers, after: "" }
		}

		if line_parts.before.is_empty() {
			Ok(Done({ rest: { raw: line_parts.after } }))
		} else {
			match headers.find_first(":") {
				Ok({ before: name, after: value_start }) => {
					name_len = Str.count_utf8_bytes(name)
					line_len = Str.count_utf8_bytes(line_parts.before)

					if name_len >= line_len {
						Err(Headers.DecodeErr.BadHeader)
					} else {
						if name_len < Str.FieldName.FieldNames.shortest_name(fields) {
							Ok(Continue({ rest: { raw: line_parts.after } }))
						} else {
							if name_len > Str.FieldName.FieldNames.longest_name(fields) {
								Ok(Continue({ rest: { raw: line_parts.after } }))
							} else {
								Ok(TryFieldCaseless({
									name,
									rest: { raw: value_start },
								}))
							}
						}
					}
				}

				Err(NotFound) => Err(Headers.DecodeErr.BadHeader)
			}
		}
	}

take_header_value : Str -> Try({ value : Str, after : Str }, Headers.DecodeErr)
take_header_value = |raw|
	match raw.find_first("\r\n") {
		Ok({ before, after }) => Ok({ value: before.trim(), after })
		Err(NotFound) => Ok({ value: raw.trim(), after: "" })
	}

underscores_to_dashes : Str -> Str
underscores_to_dashes = |text|
	match text.find_first("_") {
		Ok({ before, after }) =>
			before.concat("-").concat(underscores_to_dashes(after))

		Err(NotFound) => text
	}
