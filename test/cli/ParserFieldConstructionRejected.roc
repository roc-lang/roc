ParserFieldConstructionRejected :: [].{}

field : Encoding.FieldName({ foo : Str })
field = Encoding.FieldName({ index: 0, name: "foo", name_len: 3 })

main : Str
main = Encoding.FieldName.name(field)
