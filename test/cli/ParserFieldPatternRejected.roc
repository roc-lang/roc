ParserFieldPatternRejected :: [].{}

get_name : Str.FieldName({ foo : Str }) -> Str
get_name = |field|
	match field {
		Str.FieldName({ name }) => name
	}

main : Str
main = ""
