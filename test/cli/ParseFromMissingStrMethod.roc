ParseFromMissingStrMethod :: [].{}

Format := [Present(Str)].{
	parse_record : ParseRecordSpec(a), Format -> Try({ value : a, rest : Format }, [MissingRequired])
	parse_record = |_, _| Err(MissingRequired)

	parse_tag_union : ParseTagUnionSpec(a), Format -> Try({ value : a, rest : Format }, [MissingRequired])
	parse_tag_union = |_, _| Err(MissingRequired)
}

parse : Str -> Try(a, [MissingRequired]) where [
	a.parse_from : Format -> Try({ value : a, rest : Format }, [MissingRequired]),
]
parse = |input| {
	Shape : a
	parsed = Shape.parse_from(Format.Present(input))?
	Ok(parsed.value)
}

main : Try({ aaa : Str, choice : [One(Str)] }, [MissingRequired])
main = parse("One")
