ParserStoredTryFieldCaseless :: [].{}

Format := [Default].{
	rename_field : Format, Str -> Str
	rename_field = |_, name| underscores_to_dashes(name)

	parse_str : Format, State -> Try({ value : Str, rest : State }, [MissingRequired])
	parse_str = |_, state|
		match state {
			Value(value) => Ok({ value, rest: Done })
			FooValue => Ok({ value: "abcdefghijklmnopqrstuvwxyz", rest: CacheName })
			CacheValue => Ok({ value: "no-cache", rest: ContentLengthName })
			Key(_, _) | Start | FooName | CacheName | ContentLengthName | ContentLengthValue | RequestCountName | RequestCountValue | Done => Err(MissingRequired)
		}

	parse_u64 : Format, State -> Try({ value : U64, rest : State }, [MissingRequired])
	parse_u64 = |_, state|
		match state {
			ContentLengthValue => Ok({ value: 5, rest: RequestCountName })
			RequestCountValue => Ok({ value: 17, rest: Done })
			Key(_, _) | Value(_) | Start | FooName | FooValue | CacheName | CacheValue | ContentLengthName | RequestCountName | Done => Err(MissingRequired)
		}

	parse_record_field : Format, Encoding.FieldName.FieldNames(_shape), State -> Try(
		[
			Field({ field : Encoding.FieldName(_shape), rest : State }),
			TryField({ name : Str, rest : State }),
			TryFieldCaseless({ name : Str, rest : State }),
			Continue({ rest : State }),
			Done({ rest : State }),
		],
		[MissingRequired],
	)
	parse_record_field = |_, fields, state|
		match state {
			Key(name, value) => Ok(TryFieldCaseless({ name, rest: Value(value) }))
			Start =>
				if Encoding.FieldName.FieldNames.shortest_name(fields) == 3 and Encoding.FieldName.FieldNames.longest_name(fields) == 14 {
					Ok(Continue({ rest: FooName }))
				} else {
					Ok(Done({ rest: Done }))
				}
			FooName => Ok(TryFieldCaseless({ name: "fOo", rest: FooValue }))
			CacheName => Ok(TryFieldCaseless({ name: "Cache-Control", rest: CacheValue }))
			ContentLengthName => Ok(TryFieldCaseless({ name: "Content-Length", rest: ContentLengthValue }))
			RequestCountName => Ok(TryFieldCaseless({ name: "Request-Count", rest: RequestCountValue }))
			Value(_) | FooValue | CacheValue | ContentLengthValue | RequestCountValue | Done => Ok(Done({ rest: Done }))
		}

	skip_record_field : Format, State -> Try(State, [MissingRequired])
	skip_record_field = |_, _| Ok(Done)

	missing_record_field : Format, Str, State -> [MissingRequired]
	missing_record_field = |_, _, _| MissingRequired

	missing_optional_field : Format, Str, State -> [Missing]
	missing_optional_field = |_, _, _| Missing
}

State := [
	Key(Str, Str),
	Value(Str),
	Start,
	FooName,
	FooValue,
	CacheName,
	CacheValue,
	ContentLengthName,
	ContentLengthValue,
	RequestCountName,
	RequestCountValue,
	Done,
]

Shape : { foo : Str }

parse_shape : State -> Try({ value : { foo : Str }, rest : State }, [MissingRequired])
parse_shape = Shape.parser_for(Format.Default)

expect {
	parsed = parse_shape(Key("fOo", "value"))?

	parsed.value == { foo: "value" }
}

HttpShape : {
	cache_control : Str,
	content_length : U64,
	foo : Str,
	request_count : U64,
}

parse_http_shape : State -> Try(
	{
		value : {
			cache_control : Str,
			content_length : U64,
			foo : Str,
			request_count : U64,
		},
		rest : State,
	},
	[MissingRequired],
)
parse_http_shape = HttpShape.parser_for(Format.Default)

expect {
	parsed = parse_http_shape(Start)?

	parsed.value == {
		cache_control: "no-cache",
		content_length: 5,
		foo: "abcdefghijklmnopqrstuvwxyz",
		request_count: 17,
	}
}

underscores_to_dashes : Str -> Str
underscores_to_dashes = |text|
	match text.find_first("_") {
		Ok({ before, after }) =>
			before.concat("-").concat(underscores_to_dashes(after))

		Err(NotFound) => text
	}
