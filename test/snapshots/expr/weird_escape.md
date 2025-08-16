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
UNEXPECTED TOKEN IN STRING - weird_escape.md:1:10:1:10
# PROBLEMS
**INVALID ESCAPE SEQUENCE**
This escape sequence is not recognized.

**UNEXPECTED TOKEN IN STRING**
The token **<unknown>** is not expected in a string literal.
String literals should be enclosed in double quotes.

Here is the problematic code:
**weird_escape.md:1:10:1:10:**
```roc
"abc\qdef"
```
         ^


# TOKENS
~~~zig
StringStart(1:1-1:2),MalformedStringPart(1:2-1:10),StringEnd(1:10-1:11),EndOfFile(1:11-1:11),
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
