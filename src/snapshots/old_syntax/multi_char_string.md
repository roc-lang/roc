# META
~~~ini
description=multi_char_string
type=expr
~~~
# SOURCE
~~~roc
"foo"
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:5),StringEnd(1:5-1:6),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.6
	(e-string-part @1.2-1.5 (raw "foo")))
~~~
# FORMATTED
~~~roc
"foo"
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.6
	(e-literal @1.2-1.5 (string "foo")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "Str"))
~~~
