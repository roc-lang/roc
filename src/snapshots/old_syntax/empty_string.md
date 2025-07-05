# META
~~~ini
description=empty_string
type=expr
~~~
# SOURCE
~~~roc
""
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:2),StringEnd(1:2-1:3),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.3
	(e-string-part @1.2-1.2 (raw "")))
~~~
# FORMATTED
~~~roc
""
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.3
	(e-literal @1.2-1.2 (string "")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "Str"))
~~~
