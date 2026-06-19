ParserInvalidReturn :: [].{}

Format := [Default]
State := [Present(Str)]

parse : Str -> a where [
	a.parser : Format -> a,
]
parse = |input| {
	Shape : a
	Shape.parser(Format.Default)
}

main : { foo : Str }
main = parse("foo: bar")
