# META
~~~ini
description=Numeric let-generalization inside nested block (rank > top_level)
type=expr
~~~
# SOURCE
~~~roc
{
    n = 42
    a = I64.to_str(n)
    b = Dec.to_str(n)
    Str.concat(a, b)
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
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "n"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "a"))
			(e-apply
				(e-ident (raw "I64.to_str"))
				(e-ident (raw "n"))))
		(s-decl
			(p-ident (raw "b"))
			(e-apply
				(e-ident (raw "Dec.to_str"))
				(e-ident (raw "n"))))
		(e-apply
			(e-ident (raw "Str.concat"))
			(e-ident (raw "a"))
			(e-ident (raw "b")))))
~~~
# FORMATTED
~~~roc
{
	n = 42
	a = I64.to_str(n)
	b = Dec.to_str(n)
	Str.concat(a, b)
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "n"))
		(e-num (value "42")))
	(s-let
		(p-assign (ident "a"))
		(e-call
			(e-lookup-external
				(builtin))
			(e-lookup-local
				(p-assign (ident "n")))))
	(s-let
		(p-assign (ident "b"))
		(e-call
			(e-lookup-external
				(builtin))
			(e-lookup-local
				(p-assign (ident "n")))))
	(e-call
		(e-lookup-external
			(builtin))
		(e-lookup-local
			(p-assign (ident "a")))
		(e-lookup-local
			(p-assign (ident "b")))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
