# META
~~~ini
description=Test logical operator (and/or) precedence for emitter parenthesization
type=expr
~~~
# SOURCE
~~~roc
|a, b, c| a and b or c and a
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpAnd,LowerIdent,OpOr,LowerIdent,OpAnd,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "a"))
		(p-ident (raw "b"))
		(p-ident (raw "c")))
	(e-binop (op "or")
		(e-binop (op "and")
			(e-ident (raw "a"))
			(e-ident (raw "b")))
		(e-binop (op "and")
			(e-ident (raw "c"))
			(e-ident (raw "a")))))
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
	(e-binop (op "or")
		(e-binop (op "and")
			(e-lookup-local
				(p-assign (ident "a")))
			(e-lookup-local
				(p-assign (ident "b"))))
		(e-binop (op "and")
			(e-lookup-local
				(p-assign (ident "c")))
			(e-lookup-local
				(p-assign (ident "a"))))))
~~~
# TYPES
~~~clojure
(expr (type "Bool, Bool, Bool -> Bool"))
~~~
