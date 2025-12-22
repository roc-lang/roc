# META
~~~ini
description=Mono test: identity function applied to integer
type=mono
~~~
# SOURCE
~~~roc
{
    identity = |x| x
    identity(42)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "identity"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(e-apply
			(e-ident (raw "identity"))
			(e-int (raw "42")))))
~~~
# FORMATTED
~~~roc
{
	identity = |x| x
	identity(42)
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "identity"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(e-call
		(e-lookup-local
			(p-assign (ident "identity")))
		(e-num (value "42"))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
# MONO
~~~roc
{
    identity = |x| x
    identity(42)
}
~~~
