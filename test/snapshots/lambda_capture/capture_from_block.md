# META
~~~ini
description="A lambda within a block expression captures a variable also defined within that block."
type=expr
~~~
# SOURCE
~~~roc
{
    a = 10
    b = (|_| a * 2)(5)
    b
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
LowerIdent,OpAssign,OpenRound,OpBar,Underscore,OpBar,LowerIdent,OpStar,Int,CloseRound,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-int (raw "10")))
		(s-decl
			(p-ident (raw "b"))
			(e-apply
				(e-tuple
					(e-lambda
						(args
							(p-underscore))
						(e-binop (op "*")
							(e-ident (raw "a"))
							(e-int (raw "2")))))
				(e-int (raw "5"))))
		(e-ident (raw "b"))))
~~~
# FORMATTED
~~~roc
{
	a = 10
	b = (|_| a * 2)(5)
	b
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "a"))
		(e-num (value "10")))
	(s-let
		(p-assign (ident "b"))
		(e-call
			(e-closure
				(captures
					(capture (ident "a")))
				(e-lambda
					(args
						(p-underscore))
					(e-binop (op "mul")
						(e-lookup-local
							(p-assign (ident "a")))
						(e-num (value "2")))))
			(e-num (value "5"))))
	(e-lookup-local
		(p-assign (ident "b"))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
