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
    (addX, addY)
}
~~~
# MONO
~~~roc
{
    x = 10
    y = 20
    addX = #addX({x: x})
    addY = #addY({y: y})
    (addX, addY)
} : (c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])], c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])])
~~~
# FORMATTED
~~~roc
{
	x = 10
	y = 20
	addX = |a| a + x
	addY = |b| b + y
	(addX, addY)
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
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
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
		(e-tuple
			(e-ident (raw "addX"))
			(e-ident (raw "addY")))))
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "10")))
	(s-let
		(p-assign (ident "y"))
		(e-num (value "20")))
	(s-let
		(p-assign (ident "addX"))
		(e-closure
			(captures
				(capture (ident "x")))
			(e-lambda
				(args
					(p-assign (ident "a")))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "a")))
					(e-lookup-local
						(p-assign (ident "x")))))))
	(s-let
		(p-assign (ident "addY"))
		(e-closure
			(captures
				(capture (ident "y")))
			(e-lambda
				(args
					(p-assign (ident "b")))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "b")))
					(e-lookup-local
						(p-assign (ident "y")))))))
	(e-tuple
		(elems
			(e-lookup-local
				(p-assign (ident "addX")))
			(e-lookup-local
				(p-assign (ident "addY"))))))
~~~
# TYPES
~~~clojure
(expr (type "(c -> c, d -> d) where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
~~~
