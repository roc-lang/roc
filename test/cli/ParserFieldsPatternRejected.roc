ParserFieldsPatternRejected :: [].{}

get_count : Encoding.FieldName.FieldNames({ foo : Str }) -> U64
get_count = |fields|
	match fields {
		Encoding.FieldName.FieldNames({ items }) => 0
	}

main : U64
main = 0
