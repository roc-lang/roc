JsonUnsupportedFunctionParserField :: [].{}

main : Try({ handler : Str -> Str }, Json.ParseErr)
main = Json.parse("{}")
