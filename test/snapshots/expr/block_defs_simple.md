# META
~~~ini
description=Block expression with two decls and final binop expr
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    y = x + 1
    y * 2
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpStar,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "y"))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "1"))))
		(e-binop (op "*")
			(e-ident (raw "y"))
			(e-int (raw "2")))))
~~~
# FORMATTED
~~~roc
{
	x = 42
	y = x + 1
	y * 2
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "42")))
	(s-let
		(p-assign (ident "y"))
		(e-dispatch-call (method "plus") (constraint-fn-var 84)
			(receiver
				(e-lookup-local
					(p-assign (ident "x"))))
			(args
				(e-num (value "1")))))
	(e-dispatch-call (method "times") (constraint-fn-var 119)
		(receiver
			(e-lookup-local
				(p-assign (ident "y"))))
		(args
			(e-num (value "2")))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
