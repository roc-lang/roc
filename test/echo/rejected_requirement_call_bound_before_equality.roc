# `Blub.parse`'s where-clause requirement `a.parser_for` is made concrete by
# the later `==`, which relates `a` to `[Friendly]` and reports the rejected
# requirement (a tag union's parser needs `Parser.parse_tag_union`). The call
# bound to `r` runs first, in source order, and crashes at the dispatch that
# cannot run, in every lowering mode; it is not evaluated at compile time.
main! = |_args| {
	echo!("before\n")
	r = Blub.parse("Friendly")
	same = Ok(Friendly) == r
	echo!(Str.inspect(same))
	echo!("after\n")
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
