ParserUniformTagErrors :: [].{}

Format := {}.{
	parse_tag_union : Format, Encoding.ParseTagUnionSpec(a), Str -> Try({ value : a, rest : Str }, [Syntax(Str)])
	parse_tag_union = |encoding, spec, state| {
		parse_once = |next_state| Encoding.ParseTagUnionSpec.parse(spec, {
			tag: "Item",
			encoding,
			state: next_state,
			start_payloads: |s, _| Ok(s),
			next_payload: |s, _, _| Ok(s),
			finish_payloads: |s, _| Ok(s),
			missing: Syntax("unknown tag"),
		})
		match parse_once(state) {
			Ok(value) => Ok(value)
			Err(Syntax(message)) =>
				if state == "retry" parse_once("ok") else Err(Syntax("caught ${message}"))
		}
	}
}

Payload := {}.{
	parser_for : Format -> (Str -> Try({ value : Payload, rest : Str }, [Syntax(Str)]))
	parser_for = |_| |state|
		if state == "ok" Ok({ value: Payload.{}, rest: state }) else Err(Syntax("child"))
}

expect {
	Shape : [Item(Payload)]
	parse = Shape.parser_for(Format.{})
	match parse("") {
		Err(Syntax("caught child")) => True
		_ => False
	}
}

expect {
	Shape : [Item(Payload)]
	parse = Shape.parser_for(Format.{})
	match parse("retry") {
		Ok({ value: Item(_), rest: "ok" }) => True
		_ => False
	}
}

expect {
	Shape : [Other(Payload)]
	parse = Shape.parser_for(Format.{})
	match parse("") {
		Err(Syntax("caught unknown tag")) => True
		_ => False
	}
}

# A non-row error remains format-owned, including missing-field construction.
TextFormat := {}.{
	missing_field : TextFormat, Str -> Str
	missing_field = |_, name| "absent ${name}"
	rename_field : TextFormat, Str -> Str
	rename_field = |_, name| name
	parse_u8 : TextFormat, {} -> Try({ value : U8, rest : {} }, Str)
	parse_u8 = |_, _| Err("no byte")
	parse_record_start : TextFormat, {} -> Try([Counted({ len : U64, rest : {} }), Uncounted({})], Str)
	parse_record_start = |_, state| Ok(Uncounted(state))
	parse_record_field : TextFormat, Encoding.FieldName.FieldNames(a), {} -> Try(
		[
			Field({ field : Encoding.FieldName(a), rest : {} }),
			TryField({ name : Str, rest : {} }),
			TryFieldCaseless({ name : Str, rest : {} }),
			Continue({}),
			Done({}),
		],
		Str,
	)
	parse_record_field = |_, _, state| Ok(Done(state))
	parse_record_after_field : TextFormat, {} -> Try([Continue({}), Done({})], Str)
	parse_record_after_field = |_, state| Ok(Done(state))
	skip_record_field : TextFormat, {} -> Try({}, Str)
	skip_record_field = |_, state| Ok(state)
}

expect {
	Shape : { count : U8 }
	parse = Shape.parser_for(TextFormat.{})
	parse({}) == Err("absent count")
}
