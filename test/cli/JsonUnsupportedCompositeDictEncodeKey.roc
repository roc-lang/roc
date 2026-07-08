JsonUnsupportedCompositeDictEncodeKey :: [].{}

value : Dict((Str, Str), U64)
value = Dict.from_list([(("alpha", "beta"), 1)])

main : Str
main = Json.to_str(value)
