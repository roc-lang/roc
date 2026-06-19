ParserFieldPatternRejected :: [].{}

get_name : Field({ foo : Str }) -> Str
get_name = |field|
	match field {
		Field({ name }) => name
	}

main : Str
main = ""
