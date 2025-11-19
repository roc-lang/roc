# META
~~~ini
description="A basic case where a lambda captures one variable from its immediate parent scope."
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    y = (|_| x)(1)
    y
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
LowerIdent,OpAssign,OpenRound,OpBar,Underscore,OpBar,LowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(s-decl
			(p-ident (raw "y"))
			(e-apply
				(e-tuple
					(e-lambda
						(args
							(p-underscore))
						(e-ident (raw "x"))))
				(e-int (raw "1"))))
		(e-ident (raw "y"))))
~~~
# FORMATTED
~~~roc
{
	x = 5
	y = (|_| x)(1)
	y
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(s-let
		(p-assign (ident "y"))
		(e-call
			(e-closure
				(captures
					(capture (ident "x")))
				(e-lambda
					(args
						(p-underscore))
					(e-lookup-local
						(p-assign (ident "x")))))
			(e-num (value "1"))))
	(e-lookup-local
		(p-assign (ident "y"))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
