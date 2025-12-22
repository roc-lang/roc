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
42 : Dec
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
(e-num (value "42"))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
