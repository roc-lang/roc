ParserFieldConstructionRejected :: [].{}

field : Field({ foo : Str })
field = Field({ index: 0, name: "foo" })

main : Str
main = Field.name(field)
