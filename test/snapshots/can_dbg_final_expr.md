# META
~~~ini
description=Test dbg as final expression in block (returns unit)
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    dbg x
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
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(s-dbg
			(e-ident (raw "x")))))
~~~
# FORMATTED
~~~roc
{
	x = 5
	dbg x
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(e-dbg
		(e-lookup-local
			(p-assign (ident "x")))))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
