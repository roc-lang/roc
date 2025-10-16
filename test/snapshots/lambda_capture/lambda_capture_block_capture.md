# META
~~~ini
description=Block expression with lambda capture
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    f = |y| x + y
    f(10)
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
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
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "y")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "y")))))
		(e-apply
			(e-ident (raw "f"))
			(e-int (raw "10")))))
~~~
# FORMATTED
~~~roc
{
	x = 42
	f = |y| x + y
	f(10)
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "42")))
	(s-let
		(p-assign (ident "f"))
		(e-closure
			(captures
				(capture (ident "x")))
			(e-lambda
				(args
					(p-assign (ident "y")))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "x")))
					(e-lookup-local
						(p-assign (ident "y")))))))
	(e-call
		(e-lookup-local
			(p-assign (ident "f")))
		(e-num (value "10"))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
