# META
~~~ini
description=Mono test: dispatch match for closure call with multiple possible closures
type=mono
~~~
# SOURCE
~~~roc
condition = True
f = if condition |x| x + 1 else |x| x * 2
result = f(10)
~~~
# MONO
~~~roc
condition : [True]
condition = True

f : Dec -> Dec
f = if (condition) |x| x + 1 else |x| x * 2

result : Dec
result = 11
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
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpAssign,KwIf,LowerIdent,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,KwElse,OpBar,LowerIdent,OpBar,LowerIdent,OpStar,Int,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "condition"))
			(e-tag (raw "True")))
		(s-decl
			(p-ident (raw "f"))
			(e-if-then-else
				(e-ident (raw "condition"))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-int (raw "1"))))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-binop (op "*")
						(e-ident (raw "x"))
						(e-int (raw "2"))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "f"))
				(e-int (raw "10"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "condition"))
		(e-zero-argument-tag (closure "True") (name "True")))
	(d-let
		(p-assign (ident "f"))
		(e-if
			(if-branches
				(if-branch
					(e-lookup-local
						(p-assign (ident "condition")))
					(e-lambda
						(args
							(p-assign (ident "x")))
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "x")))
							(e-num (value "1"))))))
			(if-else
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-binop (op "mul")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-num (value "2")))))))
	(d-let
		(p-assign (ident "result"))
		(e-num (value "11"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[True, .._others]"))
		(patt (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(b)]), b.from_numeral : Numeral -> Try(c, [InvalidNumeral(b)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(_b)])]")))
	(expressions
		(expr (type "[True, .._others]"))
		(expr (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(b)]), b.from_numeral : Numeral -> Try(c, [InvalidNumeral(b)])]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(_b, [InvalidNumeral(a)])]"))))
~~~
