# META
~~~ini
description=Lambda expression
type=expr
~~~
# SOURCE
~~~roc
|x| x + 1
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x")))
	(e-binop (op "+")
		(e-ident (raw "x"))
		(e-int (raw "1"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "x")))
	(e-binop (op "add")
		(e-lookup-local
			(p-assign (ident "x")))
		(e-num (value "1"))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size) -> Num(_size2)"))
~~~
