# META
~~~ini
description=one_char_string
type=expr
~~~
# SOURCE
~~~roc
"x"
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:3),StringEnd(1:3-1:4),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.4
	(e-string-part @1.2-1.3 (raw "x")))
~~~
# FORMATTED
~~~roc
"x"
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.4
	(e-literal @1.2-1.3 (string "x")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Str"))
~~~
