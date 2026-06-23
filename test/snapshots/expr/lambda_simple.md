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
	(e-dispatch-call (method "plus") (constraint-fn-var 44)
		(receiver
			(e-lookup-local
				(p-assign (ident "x"))))
		(args
			(e-num (value "1")))))
~~~
# TYPES
~~~clojure
(expr (type "a -> a where [a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
~~~
