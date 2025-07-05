# META
~~~ini
description=string_without_escape
type=expr
~~~
# SOURCE
~~~roc
"123 abc 456 def"
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:17),StringEnd(1:17-1:18),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.18
	(e-string-part @1.2-1.17 (raw "123 abc 456 def")))
~~~
# FORMATTED
~~~roc
"123 abc 456 def"
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.18
	(e-literal @1.2-1.17 (string "123 abc 456 def")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.18 (type "Str"))
~~~
