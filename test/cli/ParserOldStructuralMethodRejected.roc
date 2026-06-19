ParserOldStructuralMethodRejected :: [].{}

Format := [Default]

parse_old : { foo : Str }
parse_old = {
	Shape : { foo : Str }
	Shape.parse_from(Format.Default)
}

main : { foo : Str }
main = parse_old
