"a string"
|> Str.toUtf8
|> List.map |byte| byte + 1
|> List.reverse