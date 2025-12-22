# META
~~~ini
description=Mono test: arithmetic expression
type=mono
~~~
# SOURCE
~~~roc
1 + 2
~~~
# MONO
~~~roc
3 : Dec
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "+")
	(e-int (raw "1"))
	(e-int (raw "2")))
~~~
# CANONICALIZE
~~~clojure
(e-num (value "3"))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
