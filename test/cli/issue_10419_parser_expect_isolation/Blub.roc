# Repro for https://github.com/roc-lang/roc/issues/10419: the first expect is
# independent and must still pass when the later parser expectation reports a
# checking diagnostic.
expect "hi" == Blub.parse("hi")?
expect Friendly == Blub.parse("Friendly")?

Blub :: {}.{
	parse : Str -> Try(a, [])
		where [
			a.parser_for : Parser -> (Str -> Try({ value : a, rest : Str }, [])),
		]
	parse = |str| {
		T : a
		parse = T.parser_for({})
		{ value, .. } = parse(str)?
		Ok(value)
	}
}

Parser := {}.{
	parse_str : Parser, Str -> Try({ value : Str, rest : Str }, [])
	parse_str = |_parser, str| Ok({ value: str, rest: "" })
}
