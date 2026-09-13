ParserFormatErrorComposition :: [].{}

State := { bytes : List(U8) }.{ is_eq : _ }

Format := {}.{
	parse_u8 : Format, State -> Try({ value : U8, rest : State }, [BadByte(U8), NoByte])
	parse_u8 = |_, state| match state.bytes {
		[] => Err(NoByte)
		[255, ..] => Err(BadByte(255))
		[value, .. as bytes] => Ok({ value, rest: State.{ bytes } })
	}

	parse_list_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [Start(U64)])
	parse_list_start = |_, state| match state.bytes {
		[254, ..] => Err(Start(254))
		_ => Ok(Uncounted(state))
	}

	parse_list_next : Format, State -> Try([Item(State), Done(State)], _)
	parse_list_next = |_, state| match state.bytes {
		[] => Ok(Done(state))
		_ => Ok(Item(state))
	}

	parse_list_after_item : Format, State -> Try([Continue(State), Done(State)], [Separator(Str)])
	parse_list_after_item = |_, state| match state.bytes {
		[] => Ok(Done(state))
		[0, .. as bytes] => Ok(Continue(State.{ bytes }))
		_ => Err(Separator("expected zero"))
	}
}

parse : List(U8) -> Try({ value : List(U8), rest : State }, [BadByte(U8), NoByte, Separator(Str), Start(U64)])
parse = |bytes| {
	T : List(U8)
	parse_ = T.parser_for(Format.{})
	parse_(State.{ bytes })
}

expect parse([]) == Ok({ value: [], rest: State.{ bytes: [] } })
expect parse([1, 0, 2]) == Ok({ value: [1, 2], rest: State.{ bytes: [] } })
expect parse([254]) == Err(Start(254))
expect parse([255]) == Err(BadByte(255))
expect parse([1, 2]) == Err(Separator("expected zero"))

expect {
	T : U8
	parse_ = T.parser_for(Format.{})
	result : Try({ value : U8, rest : State }, [BadByte(U8), NoByte, Unrelated(Str)])
	result = parse_(State.{ bytes: [] })
	result == Err(NoByte) and parse([255]) == Err(BadByte(255))
}

Infallible := {}.{
	parse_u8 : Infallible, {} -> Try({ value : U8, rest : {} }, [])
	parse_u8 = |_, state| Ok({ value: 7, rest: state })
}

expect {
	T : U8
	parse_ = T.parser_for(Infallible.{})
	result : Try({ value : U8, rest : {} }, [])
	result = parse_({})
	Ok(parsed) = result
	parsed.value == 7
}

TextErrors := {}.{
	parse_u8 : TextErrors, {} -> Try({ value : U8, rest : {} }, Str)
	parse_u8 = |_, _| Err("bad input")
}

expect {
	T : U8
	parse_ = T.parser_for(TextErrors.{})
	parse_({}) == Err("bad input")
}
