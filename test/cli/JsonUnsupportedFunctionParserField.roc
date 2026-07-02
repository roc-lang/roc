JsonUnsupportedFunctionParserField :: [].{}

main : Try({ handler : Str -> Str }, Json)
main = Json.parse("{}")
