# META
~~~ini
description=Mono test: pure lambda (no captures) assigned to top-level
type=mono
~~~
# SOURCE
~~~roc
add_one = |x| x + 1
result = add_one(5)
~~~
# MONO
~~~roc
add_one : Dec -> Dec
add_one = |x| x + 1
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
**COMPTIME CRASH**
This definition crashed during compile-time evaluation:
**mono_pure_lambda.md:1:1:1:1:**
```roc
add_one = |x| x + 1
```
^

The `crash` happened with this message:

    ªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªªª

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "add_one"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-int (raw "1")))))
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
		(p-assign (ident "add_one"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-num (value "1")))))
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
		(patt (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "[False, True]"))
		(expr (type "[]"))))
~~~
