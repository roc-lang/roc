# META
~~~ini
description=Test dbg statement inside a block
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    dbg x
    x
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
KwDbg,LowerIdent,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "42")))
		(s-dbg
			(e-ident (raw "x")))
		(e-ident (raw "x"))))
~~~
# FORMATTED
~~~roc
{
	x = 42
	dbg x
	x
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "42")))
	(s-dbg
		(e-lookup-local
			(p-assign (ident "x"))))
	(e-lookup-local
		(p-assign (ident "x"))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
