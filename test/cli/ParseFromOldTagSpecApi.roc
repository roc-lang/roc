ParseFromOldTagSpecApi :: [].{}

Format := [Present(Str)].{
	parse_tag_union : ParseTagUnionSpec(a), Format -> Try({ value : a, rest : Format }, [MissingRequired])
	parse_tag_union = |spec, slot|
		match slot {
			Present(tag_name) => {
				value = ParseTagUnionSpec.parse(spec, tag_name, Present(tag_name), Str.is_eq, MissingRequired)?
				Ok({ value, rest: Present("") })
			}
		}
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

main : Try([One], [MissingRequired])
main = parse("One")
