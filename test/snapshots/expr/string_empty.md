# META
~~~ini
description=Empty string literal
type=expr
~~~
# SOURCE
~~~roc
""
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:2),StringEnd(1:2-1:3),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.3
	(e-string-part @1.2-1.2 (raw "")))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
