# META
~~~ini
description=Test closures without captures
type=expr
~~~
# SOURCE
~~~roc
{
    list = [1, 2, 3]
    List.map(list, |x| x + 1)
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
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,CloseRound,
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
		(e-apply
			(e-ident (raw "List.map"))
			(e-ident (raw "list"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-int (raw "1")))))))
~~~
# FORMATTED
~~~roc
{
	list = [1, 2, 3]
	List.map(list, |x| x + 1)
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
	(e-call
		(e-lookup-external
			(builtin))
		(e-lookup-local
			(p-assign (ident "list")))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-num (value "1"))))))
~~~
# TYPES
~~~clojure
(expr (type "List(b) where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, c -> b, c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
~~~
