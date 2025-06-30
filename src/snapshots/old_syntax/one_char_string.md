# META
~~~ini
description=one_char_string
type=expr
~~~
# SOURCE
~~~roc
"x"
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:3),StringEnd(1:3-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.4
	(e-string-part @1.2-1.3 (raw "x")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.4 (id 74)
	(e-literal @1.2-1.3 (string "x")))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Str"))
~~~
