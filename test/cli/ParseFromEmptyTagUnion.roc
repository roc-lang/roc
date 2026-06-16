ParseFromEmptyTagUnion :: [].{}

Format := [Present(Str)]

parse : Str -> Try(a, [MissingRequired]) where [
	a.parse_from : Format -> Try({ value : a, rest : Format }, [MissingRequired]),
]
parse = |input| {
	Shape : a
	parsed = Shape.parse_from(Format.Present(input))?
	Ok(parsed.value)
}

main : Try([], [MissingRequired])
main = parse("")
