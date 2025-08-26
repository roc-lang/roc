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
INVALID ESCAPE SEQUENCE - :0:0:0:0
# PROBLEMS
**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.

```roc
"abc\qdef"
```
    ^^


# TOKENS
~~~zig
StringStart(1:1-1:2),MalformedStringPart(1:2-1:10),StringEnd(1:10-1:11),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.11)
~~~
# FORMATTED
~~~roc
""
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.11)
~~~
# TYPES
~~~clojure
(expr @1.1-1.11 (type "Str"))
~~~
