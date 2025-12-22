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
    |x| a + b + x
}
~~~
# MONO
~~~roc
{
    a = 1
    b = 2
    #1({a: a, b: b})
} : c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]
~~~
# FORMATTED
~~~roc
{
	a = 1
	b = 2
	|x| a + b + x
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
OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
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
		(e-lambda
			(args
				(p-ident (raw "x")))
			(e-binop (op "+")
				(e-binop (op "+")
					(e-ident (raw "a"))
					(e-ident (raw "b")))
				(e-ident (raw "x"))))))
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "a"))
		(e-num (value "1")))
	(s-let
		(p-assign (ident "b"))
		(e-num (value "2")))
	(e-closure
		(captures
			(capture (ident "a"))
			(capture (ident "b")))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "a")))
					(e-lookup-local
						(p-assign (ident "b"))))
				(e-lookup-local
					(p-assign (ident "x")))))))
~~~
# TYPES
~~~clojure
(expr (type "c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
~~~
