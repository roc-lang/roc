# META
~~~ini
description="An inner lambda captures a variable defined in an outer lambda's scope."
type=expr
~~~
# SOURCE
~~~roc
{
    f = (|a| |b| a + b)
    g = f(10)
    g(5) # Expect: 15
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,OpenRound,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "f"))
			(e-tuple
				(e-lambda
					(args
						(p-ident (raw "a")))
					(e-lambda
						(args
							(p-ident (raw "b")))
						(e-binop (op "+")
							(e-ident (raw "a"))
							(e-ident (raw "b")))))))
		(s-decl
			(p-ident (raw "g"))
			(e-apply
				(e-ident (raw "f"))
				(e-int (raw "10"))))
		(e-apply
			(e-ident (raw "g"))
			(e-int (raw "5")))))
~~~
# FORMATTED
~~~roc
{
	f = (|a| |b| a + b)
	g = f(10)
	g(5) # Expect: 15
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-closure
				(captures
					(capture (ident "a")))
				(e-lambda
					(args
						(p-assign (ident "b")))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "a")))
						(e-lookup-local
							(p-assign (ident "b"))))))))
	(s-let
		(p-assign (ident "g"))
		(e-call
			(e-lookup-local
				(p-assign (ident "f")))
			(e-num (value "10"))))
	(e-call
		(e-lookup-local
			(p-assign (ident "g")))
		(e-num (value "5"))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
