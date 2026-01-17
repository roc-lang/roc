# META
~~~ini
description=Test expect statement inside block
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    expect x > 0
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
KwExpect,LowerIdent,OpGreaterThan,Int,
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
			(e-int (raw "5")))
		(s-expect
			(e-binop (op ">")
				(e-ident (raw "x"))
				(e-int (raw "0"))))
		(e-ident (raw "x"))))
~~~
# FORMATTED
~~~roc
{
	x = 5
	expect x > 0
	x
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(s-expect
		(e-binop (op "gt")
			(e-lookup-local
				(p-assign (ident "x")))
			(e-num (value "0"))))
	(e-lookup-local
		(p-assign (ident "x"))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_gt : a, a -> Bool]"))
~~~
