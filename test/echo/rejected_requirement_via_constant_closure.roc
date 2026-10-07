# `check_one` relates `Blub.parse`'s result to `[Friendly]`, so the `==`
# rejects the instantiated requirement `a.parser_for` (a tag union's parser
# needs `Parser.parse_tag_union`), and the call through that instantiation
# cannot run. A hoisted binding calls `check_one` through a top-level record
# constant that stores it. The call is never evaluated at compile time: in
# every lowering mode it runs in source order, after "before" and the `dbg`,
# and crashes at the dispatch that cannot run, before "middle".
main! = |_args| {
	echo!("before\n")
	same = (fns.f)("Friendly")
	echo!("middle\n")
	echo!("${Str.inspect(same)}\n")
	Ok({})
}

fns = { f: check_one, n: 1 }

check_one = |s| {
	dbg "check_one"
	r = Blub.parse(s)
	Ok(Friendly) == r
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
