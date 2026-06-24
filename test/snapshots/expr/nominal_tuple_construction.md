# META
~~~ini
description=Nominal tuple construction syntax
type=expr
~~~
# SOURCE
~~~roc
Pair.(1, 2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,Dot,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-nominal-apply
	(mapper (e-tag (raw "Pair")))
	(e-int (raw "1"))
	(e-int (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
