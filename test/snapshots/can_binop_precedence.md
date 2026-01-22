# META
~~~ini
description=Test binary operator precedence
type=expr
~~~
# SOURCE
~~~roc
|a, b, c| a + b * c - a / b
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpStar,LowerIdent,OpBinaryMinus,LowerIdent,OpSlash,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "a"))
		(p-ident (raw "b"))
		(p-ident (raw "c")))
	(e-binop (op "-")
		(e-binop (op "+")
			(e-ident (raw "a"))
			(e-binop (op "*")
				(e-ident (raw "b"))
				(e-ident (raw "c"))))
		(e-binop (op "/")
			(e-ident (raw "a"))
			(e-ident (raw "b")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "a"))
		(p-assign (ident "b"))
		(p-assign (ident "c")))
	(e-binop (op "sub")
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "a")))
			(e-binop (op "mul")
				(e-lookup-local
					(p-assign (ident "b")))
				(e-lookup-local
					(p-assign (ident "c")))))
		(e-binop (op "div")
			(e-lookup-local
				(p-assign (ident "a")))
			(e-lookup-local
				(p-assign (ident "b"))))))
~~~
# TYPES
~~~clojure
(expr (type "d, e, f -> d where [d.div_by : d, e -> d, d.minus : d, d -> d, d.plus : d, e -> d, e.times : e, f -> e]"))
~~~
