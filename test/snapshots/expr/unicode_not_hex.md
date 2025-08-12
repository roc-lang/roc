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
INVALID UNICODE ESCAPE SEQUENCE - :0:0:0:0
UNEXPECTED TOKEN IN STRING - unicode_not_hex.md:1:16:1:16
# PROBLEMS
**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

**UNEXPECTED TOKEN IN STRING**
The token **<unknown>** is not expected in a string literal.
String literals should be enclosed in double quotes.

Here is the problematic code:
**unicode_not_hex.md:1:16:1:16:**
```roc
"abc\u(zzzz)def"
```
               


# TOKENS
~~~zig
StringStart(1:1-1:2),MalformedStringPart(1:2-1:16),StringEnd(1:16-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.17)
~~~
# FORMATTED
~~~roc
""
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.17)
~~~
# TYPES
~~~clojure
(expr @1.1-1.17 (type "Str"))
~~~
