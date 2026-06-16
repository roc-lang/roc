ParseFromMissingTagUnionMethod :: [].{}

Format := [Present(Str)].{
	parse_str : ParseStrSpec(a), Format -> Try({ value : a, rest : Format }, [MissingRequired])
	parse_str = |_, _| Err(MissingRequired)
}

parse : Str -> Try(a, [MissingRequired]) where [
	a.parse_from : Format -> Try({ value : a, rest : Format }, [MissingRequired]),
]
parse = |input| {
	Shape : a
	parsed = Shape.parse_from(Format.Present(input))?
	Ok(parsed.value)
}

main : Try([One], [MissingRequired])
main = parse("One")
