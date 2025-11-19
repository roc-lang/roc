# META
~~~ini
description=Lambda with multiple arguments
type=expr
~~~
# SOURCE
~~~roc
|x, y| x + y
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x"))
		(p-ident (raw "y")))
	(e-binop (op "+")
		(e-ident (raw "x"))
		(e-ident (raw "y"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "x"))
		(p-assign (ident "y")))
	(e-binop (op "add")
		(e-lookup-local
			(p-assign (ident "x")))
		(e-lookup-local
			(p-assign (ident "y")))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size), Num(_size2) -> Num(_size3)"))
~~~
