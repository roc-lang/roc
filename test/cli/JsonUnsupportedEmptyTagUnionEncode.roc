JsonUnsupportedEmptyTagUnionEncode :: [].{}

value : []
value = {
	crash "unreachable"
}

main : Str
main = Json.to_str(value)
