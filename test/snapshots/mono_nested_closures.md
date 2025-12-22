# META
~~~ini
description=Mono test: nested closures with captures
type=mono
~~~
# SOURCE
~~~roc
{
    x = 10
    makeAdder = |y| |z| x + y + z
    addFive = makeAdder(5)
    addFive(3)
}
~~~
# MONO
~~~roc
18 : Dec
~~~
# FORMATTED
~~~roc
{
	x = 10
	makeAdder = |y| |z| x + y + z
	addFive = makeAdder(5)
	addFive(3)
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
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
			(p-ident (raw "x"))
			(e-int (raw "10")))
		(s-decl
			(p-ident (raw "makeAdder"))
			(e-lambda
				(args
					(p-ident (raw "y")))
				(e-lambda
					(args
						(p-ident (raw "z")))
					(e-binop (op "+")
						(e-binop (op "+")
							(e-ident (raw "x"))
							(e-ident (raw "y")))
						(e-ident (raw "z"))))))
		(s-decl
			(p-ident (raw "addFive"))
			(e-apply
				(e-ident (raw "makeAdder"))
				(e-int (raw "5"))))
		(e-apply
			(e-ident (raw "addFive"))
			(e-int (raw "3")))))
~~~
# CANONICALIZE
~~~clojure
(e-num (value "18"))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
