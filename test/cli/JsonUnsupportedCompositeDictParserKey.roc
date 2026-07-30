JsonUnsupportedCompositeDictParserKey :: [].{}

main : Try(Dict((Str, Str), U64), [InvalidJson(Str)])
main = Json.parse("{}")
