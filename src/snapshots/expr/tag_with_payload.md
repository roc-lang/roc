# META
~~~ini
description=Tag with payload
type=expr
~~~
# SOURCE
~~~roc
Some(42)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:5),NoSpaceOpenRound(1:5-1:6),Int(1:6-1:8),CloseRound(1:8-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.9
	(e-tag @1.1-1.5 (raw "Some"))
	(e-int @1.6-1.8 (raw "42")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.5 (name "Some")
	(args
		(e-int @1.6-1.8 (value "42"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "[Some(Num(size))]a"))
~~~
