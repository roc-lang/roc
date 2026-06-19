ParserFieldsAccessRejected :: [].{}

get_items : Fields({ foo : Str }) -> U64
get_items = |fields| fields.items

main : U64
main = 0
