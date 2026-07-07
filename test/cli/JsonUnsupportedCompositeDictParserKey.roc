JsonUnsupportedCompositeDictParserKey :: [].{}

main : Try(Dict((Str, Str), U64), Json.ParseErr)
main = Json.parse("{}")
