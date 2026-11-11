ParserMismatchedSpecState :: [].{}

Format := {}.{
	parse_tag_union : Format, _, U64 -> Try({ value : a, rest : U64 }, [Syntax(Str)])
	parse_tag_union = |encoding, spec, state| {
		parsed = Encoding.ParseTagUnionSpec.parse(spec, {
			tag: "Item",
			encoding,
			state: "",
			start_payloads: |s, _| Ok(s),
			next_payload: |s, _, _| Ok(s),
			finish_payloads: |s, _| Ok(s),
			missing: Syntax("unknown"),
		})?
		Ok({ value: parsed.value, rest: state })
	}
}

Payload := {}.{
	parser_for : Format -> (U64 -> Try({ value : Payload, rest : U64 }, [Syntax(Str)]))
	parser_for = |_| |_| Err(Syntax("child"))
}

expect {
	Shape : [Item(Payload)]
	parse = Shape.parser_for(Format.{})
	match parse(0.U64) {
		Err(Syntax("child")) => True
		_ => False
	}
}
