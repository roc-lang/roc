# META
~~~ini
description=Tag with payload
type=expr
~~~
# SOURCE
~~~roc
Some(42)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:5),NoSpaceOpenRound(1:5-1:6),Int(1:6-1:8),CloseRound(1:8-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-apply @1-1-1-9
	(e-tag @1-1-1-5 (raw "Some"))
	(e-int @1-6-1-8 (raw "42")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1-1-1-9 (id 76)
	(e-tag @1-1-1-5 (ext-var 0) (name "Some") (args "TODO"))
	(e-int @1-6-1-8 (value "42")))
~~~
# TYPES
~~~clojure
(expr (id 76) (type "*"))
~~~
