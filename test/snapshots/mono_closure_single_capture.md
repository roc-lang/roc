# META
~~~ini
description=Mono test: closure with single capture at top-level
type=mono
~~~
# SOURCE
~~~roc
x = 42
add_x = |y| x + y
result = add_x(10)
~~~
# MONO
~~~roc
x : Dec
x = 42
add_x : Dec, Dec -> Dec
add_x = |y| x + y
result : Dec
result = 52
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "add_x"))
			(e-lambda
				(args
					(p-ident (raw "y")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "y")))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "add_x"))
				(e-int (raw "10"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "add_x"))
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
	(d-let
		(p-assign (ident "result"))
		(e-num (value "52"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
