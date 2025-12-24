# META
~~~ini
description=Mono test: top-level constants are never captured by lambdas
type=mono
~~~
# SOURCE
~~~roc
one = 1
add_one = |x| x + one
result = add_one(5)
~~~
# MONO
~~~roc
one : Dec
one = 1

add_one : Dec -> Dec
add_one = |x| x + one

result : Dec
result = add_one(5)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
COMPTIME EVAL ERROR - mono_pure_lambda.md:1:1:1:1
# PROBLEMS
**COMPTIME EVAL ERROR**
This definition could not be evaluated at compile time:
**mono_pure_lambda.md:1:1:1:1:**
```roc
one = 1
```
^

The evaluation failed with error:

    ªªªªªªªªªªªªªªªªªª

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
			(p-ident (raw "one"))
			(e-int (raw "1")))
		(s-decl
			(p-ident (raw "add_one"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "one")))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "add_one"))
				(e-int (raw "5"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "one"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "add_one"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-lookup-local
					(p-assign (ident "one"))))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "add_one")))
			(e-num (value "5")))))
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
		(expr (type "Bool"))))
~~~
