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

	parse_record_field : HeaderFormat -> Try(
		[
			Field({ name : Str, value : HeaderFormat, rest : HeaderFormat }),
			End({ rest : HeaderFormat, missing : Headers.DecodeErr }),
		],
		Headers.DecodeErr,
	)
	parse_record_field = |slot|
		match slot {
			Headers(headers) => parse_record_field_from_headers(headers)
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

parse_record_field_from_headers : Str -> Try(
	[
		Field({ name : Str, value : HeaderFormat, rest : HeaderFormat }),
		End({ rest : HeaderFormat, missing : Headers.DecodeErr }),
	],
	Headers.DecodeErr,
)
parse_record_field_from_headers = |headers|
	match headers.find_first("\r\n") {
		Ok({ before, after }) if !before.is_empty() =>
			match before.find_first(":") {
				Ok({ before: name, after: value }) =>
					Ok(Field({
						name: header_name_to_record_field_name(name),
						value: HeaderFormat.Value(value.trim()),
						rest: HeaderFormat.Headers(after),
					}))

				Err(NotFound) => Err(BadHeader)
			}

		Err(NotFound) | Ok(_) =>
			Ok(End({
				rest: HeaderFormat.Value(""),
				missing: MissingRequired,
			}))
	}

parse_tag_union_from_header : Str, ParseTagUnionSpec(a) -> Try(a, Headers.DecodeErr)
parse_tag_union_from_header = |tag_name, spec|
	ParseTagUnionSpec.parse(spec, tag_name, HeaderFormat.Value(tag_name), Str.is_eq, Headers.DecodeErr.MissingRequired)

header_name_to_record_field_name : Str -> Str
header_name_to_record_field_name = |header_name|
	dashes_to_underscores(header_name.with_ascii_lowercased())

dashes_to_underscores : Str -> Str
dashes_to_underscores = |text|
	match text.find_first("-") {
		Ok({ before, after }) =>
			before.concat("_").concat(dashes_to_underscores(after))

		Err(NotFound) => text
	}
