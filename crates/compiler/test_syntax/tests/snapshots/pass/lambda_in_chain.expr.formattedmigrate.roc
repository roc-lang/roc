"a string"
|> Str.to_utf8
|> List.map \byte -> byte + 1
|> List.reverse