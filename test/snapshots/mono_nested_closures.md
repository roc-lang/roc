# META
~~~ini
description=Mono test: nested closures with captures at top-level
type=mono
~~~
# SOURCE
~~~roc
x = 10
make_adder = |y| |z| x + y + z
add_five = make_adder(5)
result = add_five(3)
~~~
# MONO
~~~roc
x : Dec
x = 10

make_adder : Dec -> (Dec -> Dec)
make_adder = |y| Closure_1({ y: y })

add_five : Dec -> Dec
add_five = make_adder(5)

result : Dec
result = add_five(3)
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
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
			(e-int (raw "10")))
		(s-decl
			(p-ident (raw "make_adder"))
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
						(e-ident (raw "z"))))))
		(s-decl
			(p-ident (raw "add_five"))
			(e-apply
				(e-ident (raw "make_adder"))
				(e-int (raw "5"))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "add_five"))
				(e-int (raw "3"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "make_adder"))
		(e-lambda
			(args
				(p-assign (ident "y")))
			(e-tag (name "Closure_1")
				(args
					(e-record
						(fields
							(field (name "y")
								(e-lookup-local
									(p-assign (ident "y"))))))))))
	(d-let
		(p-assign (ident "add_five"))
		(e-call
			(e-lookup-local
				(p-assign (ident "make_adder")))
			(e-num (value "5"))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "add_five")))
			(e-num (value "3")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a where [_b.from_numeral : Numeral -> Try(_c, [InvalidNumeral(Str)])]"))
		(patt (type "a -> (a -> a) where [a.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "_a where [_b.from_numeral : Numeral -> Try(_c, [InvalidNumeral(Str)])]"))
		(expr (type "a -> (a -> a) where [a.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_a where [_b.from_numeral : Numeral -> Try(_c, [InvalidNumeral(Str)])]"))))
~~~
