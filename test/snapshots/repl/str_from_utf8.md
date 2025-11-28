# META
~~~ini
description=Str.from_utf8 converts UTF-8 bytes to strings with error handling
type=repl
~~~
# SOURCE
~~~roc
» Str.from_utf8([72, 105])
» Str.from_utf8([])
» Str.from_utf8([82, 111, 99])
» Str.from_utf8([240, 159, 144, 166])
» Str.from_utf8([195, 169])
» Str.from_utf8([255]).is_err()
» Str.from_utf8([72, 105]).is_ok()
» Str.from_utf8([72, 105]).ok_or("fallback")
» Str.from_utf8([255]).ok_or("fallback")
» Str.from_utf8([255])
~~~
# OUTPUT
Ok("Hi")
---
Ok("")
---
Ok("Roc")
---
Ok("🐦")
---
Ok("é")
---
True
---
True
---
"Hi"
---
"fallback"
---
Err(BadUtf8({ index: 0, problem: 3 }))
# PROBLEMS
NIL
