# Support module for the rejected_requirement_via_imported_* programs:
# `check_one` cannot run, and is reached through a direct call and through a
# method dispatch this module's checking selected.
RejectedRequirementHelper := {}.{
	outer = |s| check_one(s)

	run = |s| Wrap.Wrap(s).check()
}

Wrap := [Wrap(Str)].{
	check = |Wrap(s)| check_one(s)
}

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
