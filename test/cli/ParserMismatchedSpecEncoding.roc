ParserMismatchedSpecEncoding :: [].{}

OtherFormat := { extra : U64 }

Format := {}.{
	parse_tag_union = |_, spec, state| Encoding.ParseTagUnionSpec.parse(spec, {
		tag: "Item",
		encoding: OtherFormat.{ extra: 42 },
		state,
		start_payloads: |s, _| Ok(s),
		next_payload: |s, _, _| Ok(s),
		finish_payloads: |s, _| Ok(s),
		missing: Syntax("unknown"),
	})
}

Payload := {}.{
	parser_for : Format -> (Str -> Try({ value : Payload, rest : Str }, [Syntax(Str)]))
	parser_for = |_| |_| Err(Syntax("child"))
}

expect {
	Shape : [Item(Payload)]
	parse = Shape.parser_for(Format.{})
	match parse("") {
		Err(Syntax("child")) => True
		_ => False
	}
}
