# META
~~~ini
description=Mono test: nested closures with captures
type=mono
~~~
# SOURCE
~~~roc
{
    x = 10
    |y| |z| x + y + z
}
~~~
# MONO
~~~roc
{
    x = 10
    #1({x: x})
} : a -> (a -> a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
~~~
# FORMATTED
~~~roc
{
	x = 10
	|y| |z| x + y + z
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
OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
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
					(e-ident (raw "z")))))))
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "10")))
	(e-closure
		(captures
			(capture (ident "x")))
		(e-lambda
			(args
				(p-assign (ident "y")))
			(e-closure
				(captures
					(capture (ident "x"))
					(capture (ident "y")))
				(e-lambda
					(args
						(p-assign (ident "z")))
					(e-binop (op "add")
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "x")))
							(e-lookup-local
								(p-assign (ident "y"))))
						(e-lookup-local
							(p-assign (ident "z")))))))))
~~~
# TYPES
~~~clojure
(expr (type "a -> (a -> a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
