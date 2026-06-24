ParserFieldConstructionRejected :: [].{}

field : Str.FieldName({ foo : Str })
field = Str.FieldName({ index: 0, name: "foo", name_len: 3 })

main : Str
main = Str.FieldName.name(field)
