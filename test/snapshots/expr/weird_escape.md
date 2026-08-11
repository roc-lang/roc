# META
~~~ini
description=Weird escape (should error)
type=expr
~~~
# SOURCE
~~~roc
"abc\qdef"
~~~
# EXPECTED
INVALID ESCAPE SEQUENCE - weird_escape.md:1:5:1:7
# PROBLEMS
── ✗ invalid escape sequence ─────────────────────────────── weird_escape.md:1:5

This escape sequence is not recognized.

"abc\qdef"
    ^^

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
