# META
~~~ini
description=Mono test: block with let binding
type=mono
~~~
# SOURCE
~~~roc
{
    x = 42
    x
}
~~~
# MONO
~~~roc
{
    x = 42
    x
} : a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
~~~
# FORMATTED
~~~roc
{
	x = 42
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
		(e-ident (raw "x"))))
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "42")))
	(e-lookup-local
		(p-assign (ident "x"))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
