ParserFieldPatternRejected :: [].{}

get_name : Encoding.FieldName({ foo : Str }) -> Str
get_name = |field|
	match field {
		Encoding.FieldName({ name }) => name
	}

main : Str
main = ""
