# META
~~~ini
description=Unicode not hex (should error))
type=expr
~~~
# SOURCE
~~~roc
"abc\u(zzzz)def"
~~~
# EXPECTED
INVALID UNICODE ESCAPE SEQUENCE - unicode_not_hex.md:1:5:1:13
# PROBLEMS
**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**unicode_not_hex.md:1:5:1:13:**
```roc
"abc\u(zzzz)def"
```
    ^^^^^^^^


# TOKENS
~~~zig
StringStart,MalformedStringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-string)
~~~
# FORMATTED
~~~roc
""
~~~
# CANONICALIZE
~~~clojure
(e-string)
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
