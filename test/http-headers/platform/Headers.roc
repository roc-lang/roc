Headers := { raw : Str }.{
	DecodeErr := [MissingRequired, BadHeader].{}

	parser : () -> (Headers -> Try({ value : output, rest : Headers }, DecodeErr))
		where [
			output.parser : HeaderEncoding -> (Headers -> Try({ value : output, rest : Headers }, DecodeErr)),
		]
	parser = || {
		Output : output

		parse_output : Headers -> Try({ value : output, rest : Headers }, DecodeErr)
		parse_output = Output.parser(HeaderEncoding.Caseless)

		parse_output
	}

	parse : Str -> Try(output, DecodeErr)
		where [
			output.parser : HeaderEncoding -> (Headers -> Try({ value : output, rest : Headers }, DecodeErr)),
		]
	parse = |raw| {
		Output : output

		parse_output = Output.parser(HeaderEncoding.Caseless)
		parsed = parse_output(Headers.{ raw })?

		Ok(parsed.value)
	}
}

HeaderEncoding := [Caseless].{
	rename_field : Str -> Str
	rename_field = |name| underscores_to_dashes(name)

	parse_str : Headers -> Try({ value : Str, rest : Headers }, Headers.DecodeErr)
	parse_str = |state| {
		value_parts = take_header_value(state.raw)?
		Ok({ value: value_parts.value, rest: { raw: value_parts.after } })
	}

	parse_u64 : Headers -> Try({ value : U64, rest : Headers }, Headers.DecodeErr)
	parse_u64 = |state| {
		value_parts = take_header_value(state.raw)?

		match U64.from_str(value_parts.value) {
			Ok(value) => Ok({ value, rest: { raw: value_parts.after } })
			Err(_) => Err(Headers.DecodeErr.BadHeader)
		}
	}

	parse_record_field : Fields(_shape), Headers -> Try(
		[
			Field({ field : Field(_shape), rest : Headers }),
			TryField({ name : Str, rest : Headers }),
			TryFieldCaseless({ name : Str, rest : Headers }),
			Continue({ rest : Headers }),
			Done({ rest : Headers }),
		],
		Headers.DecodeErr,
	)
	parse_record_field = |fields, state|
		parse_record_field_from_headers(fields, state.raw)

	skip_record_field : Headers -> Try(Headers, Headers.DecodeErr)
	skip_record_field = |state| {
		parts = take_header_value(state.raw)?
		Ok({ raw: parts.after })
	}

	missing_record_field : Str, Headers -> Headers.DecodeErr
	missing_record_field = |_, _| Headers.DecodeErr.MissingRequired

	missing_optional_field : Str, Headers -> [Missing]
	missing_optional_field = |_, _| Missing

	parse_tag_union : ParseTagUnionSpec(a), Headers -> Try({ value : a, rest : Headers }, Headers.DecodeErr)
	parse_tag_union = |spec, state| {
		value_parts = take_header_value(state.raw)?
		value = ParseTagUnionSpec.parse(spec, {
			tag: value_parts.value,
			encoding: HeaderEncoding.Caseless,
			state: { raw: value_parts.value },
			missing: Headers.DecodeErr.MissingRequired,
		})?
		Ok({ value, rest: { raw: value_parts.after } })
	}
}

parse_record_field_from_headers : Fields(_shape), Str -> Try(
	[
		Field({ field : Field(_shape), rest : Headers }),
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

					if name_len < line_len {
						shorter_than_any_field = name_len < Fields.shortest_name(fields)
						longer_than_any_field = name_len > Fields.longest_name(fields)

						if shorter_than_any_field {
							Ok(Continue({ rest: { raw: line_parts.after } }))
						} else if longer_than_any_field {
							Ok(Continue({ rest: { raw: line_parts.after } }))
						} else {
							match find_header_field(fields, name) {
								Ok(field) =>
									Ok(Field({
										field,
										rest: { raw: value_start },
									}))

								Err(NotFound) =>
									Ok(Continue({ rest: { raw: line_parts.after } }))
							}
						}
					} else {
						Err(Headers.DecodeErr.BadHeader)
					}
				}

				Err(NotFound) => Err(Headers.DecodeErr.BadHeader)
			}
		}
	}

find_header_field : Fields(_shape), Str -> Try(Field(_shape), [NotFound])
find_header_field = |fields, name| {
	var $remaining = Fields.for_size(fields, Str.count_utf8_bytes(name))

	while True {
		match Iter.next($remaining) {
			One({ item, rest }) =>
				if name.caseless_ascii_equals(Field.name(item)) {
					return Ok(item)
				} else {
					$remaining = rest
				}

			Skip({ rest }) => {
				$remaining = rest
			}

			Done =>
				return Err(NotFound)
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
