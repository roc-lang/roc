JsonToStrRejectsF32 :: [].{}

value : F32
value = 1.5

main : Str
main = Json.to_str(value)
