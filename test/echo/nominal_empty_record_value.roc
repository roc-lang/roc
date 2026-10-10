# `{}` constructs a `Parser`, a nominal type whose backing is the empty
# record, both as a dispatch argument and as a plain value.
main! = |_args| {
	r = Blub.parse("hi")
	echo!("${Str.inspect(Ok("hi") == r)}\n")
	p : Parser
	p = {}
	echo!("${Str.inspect(p.parse_str("x"))}\n")
	Ok({})
}

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
