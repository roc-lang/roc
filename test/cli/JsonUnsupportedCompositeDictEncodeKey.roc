JsonUnsupportedCompositeDictEncodeKey :: [].{}

value : Dict((Str, Str), U64)
value = Dict.from_list([(("alpha", "beta"), 1)])

main : Try(Str, [])
main = Json.encode(value)
