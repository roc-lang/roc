ParserTagPayloadProtocol :: [].{}

Token := [TagName(Str), Start, Next, Finish, StrValue(Str), U64Value(U64)].{
	is_eq : _
}

ParseErr := [Invalid].{
	is_eq : _
}

Format := [Default].{
	parse_tag_union : Format, Encoding.ParseTagUnionSpec(a), List(Token) -> Try({ value : a, rest : List(Token) }, ParseErr)
	parse_tag_union = |encoding, spec, state|
		match state {
			[TagName(name), .. as rest] =>
				Encoding.ParseTagUnionSpec.parse(
					spec,
					{
						tag: name,
						encoding,
						state: rest,
						start_payloads: |payload_state, count|
							if count == 2 consume(payload_state, Start) else Err(Invalid),
						next_payload: |payload_state, index, count|
							if index == 1 and count == 2 consume(payload_state, Next) else Err(Invalid),
						finish_payloads: |payload_state, count|
							if count == 2 consume(payload_state, Finish) else Err(Invalid),
						missing: Invalid,
					},
				)
			_ => Err(Invalid)
		}

	parse_str : Format, List(Token) -> Try({ value : Str, rest : List(Token) }, ParseErr)
	parse_str = |_, state|
		match state {
			[StrValue(value), .. as rest] => Ok({ value, rest })
			_ => Err(Invalid)
		}

	parse_u64 : Format, List(Token) -> Try({ value : U64, rest : List(Token) }, ParseErr)
	parse_u64 = |_, state|
		match state {
			[U64Value(value), .. as rest] => Ok({ value, rest })
			_ => Err(Invalid)
		}
}

consume : List(Token), Token -> Try(List(Token), ParseErr)
consume = |state, expected|
	match state {
		[first, .. as rest] => if first == expected Ok(rest) else Err(Invalid)
		_ => Err(Invalid)
	}

parse : List(Token) -> Try(a, ParseErr)
	where [
		a.parser_for : Format -> (List(Token) -> Try({ value : a, rest : List(Token) }, ParseErr)),
	]
parse = |input| {
	Shape : a
	parse_shape = Shape.parser_for(Format.Default)
	parsed = parse_shape(input)?
	if List.is_empty(parsed.rest) Ok(parsed.value) else Err(Invalid)
}

expect {
	result : Try([Pair(Str, U64)], ParseErr)
	result = parse([TagName("Pair"), Start, StrValue("x"), Next, U64Value(9), Finish])

	result == Ok(Pair("x", 9))
}

expect {
	result : Try([Pair(Str, U64)], ParseErr)
	result = parse([TagName("Pair"), Start, StrValue("x"), Finish, U64Value(9), Finish])

	result == Err(Invalid)
}
