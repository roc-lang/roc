ParserFieldsPatternRejected :: [].{}

get_count : Str.FieldName.FieldNames({ foo : Str }) -> U64
get_count = |fields|
	match fields {
		Str.FieldName.FieldNames({ items }) => 0
	}

main : U64
main = 0
