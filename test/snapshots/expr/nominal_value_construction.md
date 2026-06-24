# META
~~~ini
description=Nominal value construction syntax
type=expr
~~~
# SOURCE
~~~roc
Distance.(26)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,Dot,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-nominal-apply
	(mapper (e-tag (raw "Distance")))
	(e-int (raw "26")))
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
