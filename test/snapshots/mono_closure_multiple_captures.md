# META
~~~ini
description=Mono test: closure with multiple captures transforms to tag with capture record
type=mono
~~~
# SOURCE
~~~roc
{
    a = 1
    b = 2
    f = |x| a + b + x
    f(3)
}
~~~
# MONO
~~~roc
6 : Dec
~~~
# FORMATTED
~~~roc
{
	a = 1
	b = 2
	f = |x| a + b + x
	f(3)
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-int (raw "1")))
		(s-decl
			(p-ident (raw "b"))
			(e-int (raw "2")))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-binop (op "+")
						(e-ident (raw "a"))
						(e-ident (raw "b")))
					(e-ident (raw "x")))))
		(e-apply
			(e-ident (raw "f"))
			(e-int (raw "3")))))
~~~
# CANONICALIZE
~~~clojure
(e-num (value "6"))
~~~
# TYPES
~~~clojure
(expr (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
~~~
