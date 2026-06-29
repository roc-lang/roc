ParserFieldAccessRejected :: [].{}

get_index : Encoding.FieldName({ foo : Str }) -> U64
get_index = |field| field.index

main : U64
main = 0
