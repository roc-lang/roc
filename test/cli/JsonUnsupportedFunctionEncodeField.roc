JsonUnsupportedFunctionEncodeField :: [].{}

value : { handler : Str -> Str }
value = { handler: |text| text }

main : Try(Str, [])
main = Json.encode(value)
