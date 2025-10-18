# META
~~~ini
description=Dot access with proper variable definitions
type=expr
~~~
# SOURCE
~~~roc
{
    list = [1, 2, 3]
    fn = |x| x + 1
    list.map(fn)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "list"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3"))))
		(s-decl
			(p-ident (raw "fn"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-int (raw "1")))))
		(e-field-access
			(e-ident (raw "list"))
			(e-apply
				(e-ident (raw "map"))
				(e-ident (raw "fn"))))))
~~~
# FORMATTED
~~~roc
{
	list = [1, 2, 3]
	fn = |x| x + 1
	list.map(fn)
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "list"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))
				(e-num (value "3")))))
	(s-let
		(p-assign (ident "fn"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-num (value "1")))))
	(e-dot-access (field "map")
		(receiver
			(e-lookup-local
				(p-assign (ident "list"))))
		(args
			(e-lookup-local
				(p-assign (ident "fn"))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
