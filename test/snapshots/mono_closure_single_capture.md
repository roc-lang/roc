# META
~~~ini
description=Mono test: closure with single capture transforms to tag
type=mono
~~~
# SOURCE
~~~roc
{
    x = 42
    |y| x + y
}
~~~
# MONO
~~~roc
{
    x = 42
    |x0, y| x0 + y
} : Dec, Dec -> Dec
~~~
# FORMATTED
~~~roc
{
	x = 42
	|y| x + y
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
OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
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
		(e-lambda
			(args
				(p-ident (raw "y")))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))))
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "42")))
	(e-closure
		(captures
			(capture (ident "x")))
		(e-lambda
			(args
				(p-assign (ident "y")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-lookup-local
					(p-assign (ident "y")))))))
~~~
# TYPES
~~~clojure
(expr (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
