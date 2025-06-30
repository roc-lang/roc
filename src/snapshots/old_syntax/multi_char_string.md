# META
~~~ini
description=multi_char_string
type=expr
~~~
# SOURCE
~~~roc
"foo"
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:5),StringEnd(1:5-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.6
	(e-string-part @1.2-1.5 (raw "foo")))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
