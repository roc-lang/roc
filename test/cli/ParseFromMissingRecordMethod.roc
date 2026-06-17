ParseFromMissingRecordMethod :: [].{}

Format := [Present(Str)].{
	parse_str : Format -> Try({ value : Str, rest : Format }, [MissingRequired])
	parse_str = |_| Err(MissingRequired)
}

parse : Str -> Try(a, [MissingRequired])
	where [
		a.parse_from : Format -> Try({ value : a, rest : Format }, [MissingRequired]),
	]
parse = |input| {
	Shape : a
	parsed = Shape.parse_from(Format.Present(input))?
	Ok(parsed.value)
}

main : Try({ foo : Str }, [MissingRequired])
main = parse("foo: bar")
