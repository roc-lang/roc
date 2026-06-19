ParserFieldsPatternRejected :: [].{}

get_count : Fields({ foo : Str }) -> U64
get_count = |fields|
	match fields {
		Fields({ items }) => 0
	}

main : U64
main = 0
