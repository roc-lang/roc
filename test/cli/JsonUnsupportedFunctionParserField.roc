JsonUnsupportedFunctionParserField :: [].{}

main : Try({ handler : Str -> Str }, [InvalidJson(Str)])
main = Json.parse("{}")
