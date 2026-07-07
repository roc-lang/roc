JsonUnsupportedFunctionEncodeField :: [].{}

value : { handler : Str -> Str }
value = { handler: |text| text }

main : Str
main = Json.to_str(value)
