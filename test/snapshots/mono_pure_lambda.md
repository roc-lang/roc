# META
~~~ini
description=Mono test: pure lambda (no captures) transforms to tag with empty record
type=mono
~~~
# SOURCE
~~~roc
{
    f = |x| x + 1
    f(41)
}
~~~
# MONO
~~~roc
42 : Dec
~~~
# FORMATTED
~~~roc
{
	f = |x| x + 1
	f(41)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-int (raw "1")))))
		(e-apply
			(e-ident (raw "f"))
			(e-int (raw "41")))))
~~~
# CANONICALIZE
~~~clojure
(e-num (value "42"))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
