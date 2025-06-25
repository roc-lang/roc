# META
~~~ini
description=Empty string literal
type=expr
~~~
# SOURCE
~~~roc
""
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:2),StringEnd(1:2-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(e-string @1-1-1-3
	(e-string-part @1-2-1-2 (raw "")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string @1-1-1-3 (id 73)
	(e-literal @1-2-1-2 (string "")))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Str"))
~~~