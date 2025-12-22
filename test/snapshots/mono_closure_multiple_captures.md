# META
~~~ini
description=Mono test: closure with multiple captures at top-level
type=mono
~~~
# SOURCE
~~~roc
a = 1
b = 2
add_ab = |x| a + b + x
result = add_ab(10)
~~~
# MONO
~~~roc
a : Dec
a = 1
b : Dec
b = 2
add_ab : Dec -> Dec
add_ab = |x| a + b + x
result : Dec
result = 13
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-int (raw "1")))
		(s-decl
			(p-ident (raw "b"))
			(e-int (raw "2")))
		(s-decl
			(p-ident (raw "add_ab"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-binop (op "+")
						(e-ident (raw "a"))
						(e-ident (raw "b")))
					(e-ident (raw "x")))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "add_ab"))
				(e-int (raw "10"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "b"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "add_ab"))
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
	(d-let
		(p-assign (ident "result"))
		(e-num (value "13"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))))
~~~
