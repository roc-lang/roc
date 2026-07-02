JsonUnsupportedCompositeDictParserKey :: [].{}

main : Try(Dict((Str, Str), U64), Json)
main = Json.parse("{}")
