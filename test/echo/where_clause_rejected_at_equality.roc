# `Blub.parse`'s where-clause requirement `a.parser_for` is made concrete by
# the `==`, which relates `a` to `[Friendly]`. A tag union's parser needs a
# `parse_tag_union` method that `Parser` lacks, so the requirement is rejected
# and reported at the `==`. The requirement belongs to the call, not to the
# `==`, so the `==` does not evaluate its operands, which would call a
# `Blub.parse` that cannot run; running the program crashes at the `==`.
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

main! = |_args| {
	echo!("before")
	same = Ok(Friendly) == Blub.parse("Friendly")
	echo!(Str.inspect(same))
	echo!("after")
	Ok({})
}
