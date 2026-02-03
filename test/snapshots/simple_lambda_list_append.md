# META
~~~ini
description=Simple lambda that calls List.append
type=expr
~~~
# SOURCE
~~~roc
{
    add_one = |l| List.append(l, 42)
    add_one([1, 2])
}
~~~
# EXPECTED
~~~
[1, 2, 42]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,CloseRound,
LowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,CloseSquare,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "add_one"))
			(e-lambda
				(args
					(p-ident (raw "l")))
				(e-apply
					(e-ident (raw "List.append"))
					(e-ident (raw "l"))
					(e-int (raw "42")))))
		(e-apply
			(e-ident (raw "add_one"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
{
	add_one = |l| List.append(l, 42)
	add_one([1, 2])
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "add_one"))
		(e-lambda
			(args
				(p-assign (ident "l")))
			(e-call
				(e-lookup-external
					(builtin))
				(e-lookup-local
					(p-assign (ident "l")))
				(e-num (value "42")))))
	(e-call
		(e-lookup-local
			(p-assign (ident "add_one")))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))))))
~~~
# TYPES
~~~clojure
(expr (type "List(Dec)"))
~~~
