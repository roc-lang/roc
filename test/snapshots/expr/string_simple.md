# META
~~~ini
description=Simple string literal
type=expr
~~~
# SOURCE
~~~roc
"hello world"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:13),StringEnd(1:13-1:14),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.14
	(e-string-part @1.2-1.13 (raw "hello world")))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
