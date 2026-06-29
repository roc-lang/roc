JsonUnsupportedEmptyTagUnionEncode :: [].{}

value : []
value = {
	crash "unreachable"
}

main : Try(Str, [])
main = Json.encode(value)
