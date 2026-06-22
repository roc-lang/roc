ParserInvalidReturn :: [].{}

Format := [Default]
State := [Present(Str)]

parse : Str -> a where [
	a.parser_for : Format -> a,
]
parse = |input| {
	Shape : a
	Shape.parser_for(Format.Default)
}

main : { foo : Str }
main = parse("foo: bar")
