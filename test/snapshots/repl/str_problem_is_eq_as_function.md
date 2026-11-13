# META
~~~ini
description=A Str problem type's is_eq is callable as a function and usable as a function value
type=repl
~~~
# SOURCE
~~~roc
» Utf8Problem.is_eq(Utf8Problem.InvalidStartByte, Str.Utf8Problem.InvalidStartByte)
» Str.Utf8Problem.is_eq(Utf8Problem.InvalidStartByte, Utf8Problem.OverlongEncoding)
» Str.Utf16Problem.is_eq(Utf16Problem.UnpairedHighSurrogate, Utf16Problem.UnpairedLowSurrogate)
» Utf32Problem.is_eq(Utf32Problem.CodePointTooLarge, Utf32Problem.CodePointTooLarge)
» eq = Utf8Problem.is_eq
» eq(Utf8Problem.OverlongEncoding, Utf8Problem.OverlongEncoding)
~~~
# OUTPUT
True
---
False
---
False
---
True
---
assigned `eq`
---
True
# PROBLEMS
NIL
