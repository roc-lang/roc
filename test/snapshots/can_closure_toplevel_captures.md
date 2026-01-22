# META
~~~ini
description=Test closure capturing only toplevel variables (optimization case)
type=snippet
~~~
# SOURCE
~~~roc
x = 42
y = 100
f = |_| x + y

main = f
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
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
			(p-ident (raw "y"))
			(e-int (raw "100")))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-underscore))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "y")))))
		(s-decl
			(p-ident (raw "main"))
			(e-ident (raw "f")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "y"))
		(e-num (value "100")))
	(d-let
		(p-assign (ident "f"))
		(e-closure
			(captures
				(capture (ident "x"))
				(capture (ident "y")))
			(e-lambda
				(args
					(p-underscore))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "x")))
					(e-lookup-local
						(p-assign (ident "y")))))))
	(d-let
		(p-assign (ident "main"))
		(e-lookup-local
			(p-assign (ident "f")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))))
~~~
