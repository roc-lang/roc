# META
~~~ini
description="A `let` binding inside a lambda's body shadows a would-be captured variable."
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    y = (|_| { 
        x = 10
        x 
    })({}) # Inner `x` should be used; outer `x` is not captured (it should be a shadowing warning)
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
LowerIdent,OpAssign,OpenRound,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,
CloseCurly,CloseRound,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
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
						(e-block
							(statements
								(s-decl
									(p-ident (raw "x"))
									(e-int (raw "10")))
								(e-ident (raw "x"))))))
				(e-record)))
		(e-ident (raw "y"))))
~~~
# FORMATTED
~~~roc
{
	x = 5
	y = (
		|_| {
			x = 10
			x
		},
	)({}) # Inner `x` should be used; outer `x` is not captured (it should be a shadowing warning)
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
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "x"))
						(e-num (value "10")))
					(e-lookup-local
						(p-assign (ident "x")))))
			(e-empty_record)))
	(e-lookup-local
		(p-assign (ident "y"))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
