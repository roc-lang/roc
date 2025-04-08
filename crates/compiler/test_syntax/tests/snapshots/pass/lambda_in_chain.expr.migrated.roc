"a string"
.(Str.toUtf8)()
.map(|byte| byte + 1)
.reverse()
