JsonGenericCustomParser :: [].{}

# https://github.com/roc-lang/roc/issues/11728
Token := { raw : Str }.{
	parser_for : encoding -> (state -> Try({ value : Token, rest : state }, err))
		where [encoding.parse_str : encoding, state -> Try({ value : Str, rest : state }, err)]
	parser_for = |encoding| {
		Encoding : encoding
		|state| {
			parsed = Encoding.parse_str(encoding, state)?
			Ok({ value: Token.{ raw: parsed.value }, rest: parsed.rest })
		}
	}
}

expect {
	parsed : Try(Token, [InvalidJson(Str)])
	parsed = Json.parse("\"hi\"")
	parsed.map_ok(|token| token.raw) == Ok("hi")
}

expect {
	parsed : Try({ t : Token }, _)
	parsed = Json.parse("{\"t\":\"hi\"}")
	parsed.map_ok(|record| record.t.raw) == Ok("hi")
}

expect {
	parsed : Try({ t : Token }, _)
	parsed = Json.parse("{\"t\":42}")
	match parsed {
		Err(InvalidJson(_)) => True
		_ => False
	}
}

expect {
	parsed : Try({ t : Token }, _)
	parsed = Json.parse("{}")
	match parsed {
		Err(MissingRequiredField(name)) => name == "t"
		_ => False
	}
}

expect {
	parsed : Try({ tokens : List(Token) }, _)
	parsed = Json.parse("{\"tokens\":[\"hi\",\"bye\"]}")
	parsed.map_ok(|record| record.tokens.map(|token| token.raw)) == Ok(["hi", "bye"])
}

# Error types may be determined by transitive format requirements.
State := {}.{
	read : State -> Try({ value : Str, rest : State }, [ReadFailed(Str)])
	read = |_| Err(ReadFailed("failed"))
}

Format := {}.{
	parse_str : Format, state -> Try({ value : Str, rest : state }, err)
		where [state.read : state -> Try({ value : Str, rest : state }, err)]
	parse_str = |_, state| {
		S : state
		S.read(state)
	}
	parse_list_start : Format, State -> Try([Counted({ len : U64, rest : State }), Uncounted(State)], [])
	parse_list_start = |_, state| Ok(Counted({ len: 1, rest: state }))
	parse_list_next : Format, State -> Try([Item(State), Done(State)], [])
	parse_list_next = |_, state| Ok(Done(state))
	parse_list_after_item : Format, State -> Try([Continue(State), Done(State)], [])
	parse_list_after_item = |_, state| Ok(Done(state))
}

parse : {} -> Try({ value : List(Token), rest : State }, [ReadFailed(Str)])
parse = |_| {
	T : List(Token)
	parser = T.parser_for(Format.{})
	parser(State.{})
}

expect {
	match parse({}) {
		Err(ReadFailed(message)) => message == "failed"
		_ => False
	}
}

expect {
	T : List(Str)
	parser = T.parser_for(Format.{})
	match parser(State.{}) {
		Err(ReadFailed(message)) => message == "failed"
		_ => False
	}
}
