ParseFromInvalidReturn :: [].{}

Format := [Present(Str)]

parse : Str -> a where [
	a.parse_from : Format -> a,
]
parse = |input| {
	Shape : a
	Shape.parse_from(Format.Present(input))
}

main : { foo : Str }
main = parse("foo: bar")
