# META
~~~ini
description="A lambda argument shadows a variable from an outer scope. The lambda should use the argument, not the captured variable."
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    (|x| x)(10) # Should not capture outer `x` -- this should give a shadowing warning
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
OpenRound,OpBar,LowerIdent,OpBar,LowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,
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
		(e-apply
			(e-tuple
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-ident (raw "x"))))
			(e-int (raw "10")))))
~~~
# FORMATTED
~~~roc
{
	x = 5
	(|x| x)(10) # Should not capture outer `x` -- this should give a shadowing warning
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(e-call
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x"))))
		(e-num (value "10"))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
