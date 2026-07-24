JsonUnsupportedFunctionParserField :: [].{}

main : Try({ handler : Str -> Str }, [InvalidJson(Str), MissingRequiredField(Str)])
main = Json.parse("{}")
