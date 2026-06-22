ParserFieldsAccessRejected :: [].{}

get_items : Str.FieldName.FieldNames({ foo : Str }) -> U64
get_items = |fields| fields.items

main : U64
main = 0
