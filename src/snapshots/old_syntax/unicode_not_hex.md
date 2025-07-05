# META
~~~ini
description=unicode_not_hex fail
type=expr
~~~
# SOURCE
~~~roc
"abc\u(zzzz)def"
~~~
# EXPECTED
NIL
# PROBLEMS
**INVALID UNICODE ESCAPE SEQUENCE**
This Unicode escape sequence is not valid.

# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:16),StringEnd(1:16-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.17
	(e-string-part @1.2-1.16 (raw "abc\u(zzzz)def")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.17
	(e-literal @1.2-1.16 (string "abc\u(zzzz)def")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.17 (type "Str"))
~~~
