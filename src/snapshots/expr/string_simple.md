# META
~~~ini
description=Simple string literal
type=expr
~~~
# SOURCE
~~~roc
"hello world"
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:13),StringEnd(1:13-1:14),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.14
	(e-string-part @1.2-1.13 (raw "hello world")))
~~~
# FORMATTED
~~~roc
"hello world"
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.14
	(e-literal @1.2-1.13 (string "hello world")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.14 (type "Str"))
~~~
