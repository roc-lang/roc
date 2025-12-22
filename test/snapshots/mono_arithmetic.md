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
(1 + 2)
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
(e-binop (op "add")
	(e-num (value "1"))
	(e-num (value "2")))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
