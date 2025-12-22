# META
~~~ini
description=Mono test: multiple different closures in single expression
type=mono
~~~
# SOURCE
~~~roc
{
    x = 10
    y = 20
    addX = |a| a + x
    addY = |b| b + y
    addX(1) + addY(2)
}
~~~
# MONO
~~~roc
33 : Dec
~~~
# FORMATTED
~~~roc
{
	x = 10
	y = 20
	addX = |a| a + x
	addY = |b| b + y
	addX(1) + addY(2)
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
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpPlus,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "10")))
		(s-decl
			(p-ident (raw "y"))
			(e-int (raw "20")))
		(s-decl
			(p-ident (raw "addX"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-binop (op "+")
					(e-ident (raw "a"))
					(e-ident (raw "x")))))
		(s-decl
			(p-ident (raw "addY"))
			(e-lambda
				(args
					(p-ident (raw "b")))
				(e-binop (op "+")
					(e-ident (raw "b"))
					(e-ident (raw "y")))))
		(e-binop (op "+")
			(e-apply
				(e-ident (raw "addX"))
				(e-int (raw "1")))
			(e-apply
				(e-ident (raw "addY"))
				(e-int (raw "2"))))))
~~~
# CANONICALIZE
~~~clojure
(e-num (value "33"))
~~~
# TYPES
~~~clojure
(expr (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
~~~
