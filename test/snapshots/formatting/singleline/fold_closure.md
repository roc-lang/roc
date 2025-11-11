# META
~~~ini
description=Fold with closure should remain singleline
type=snippet
~~~
# SOURCE
~~~roc
sumResult = fold([1, 2, 3, 4], 0, |acc, x| acc + x)
~~~
# EXPECTED
UNDEFINED VARIABLE - fold_closure.md:1:13:1:17
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `fold` in this scope.
Is there an `import` or `exposing` missing up-top?

**fold_closure.md:1:13:1:17:**
```roc
sumResult = fold([1, 2, 3, 4], 0, |acc, x| acc + x)
```
            ^^^^


# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,Int,Comma,Int,Comma,Int,Comma,Int,CloseSquare,Comma,Int,Comma,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "sumResult"))
			(e-apply
				(e-ident (raw "fold"))
				(e-list
					(e-int (raw "1"))
					(e-int (raw "2"))
					(e-int (raw "3"))
					(e-int (raw "4")))
				(e-int (raw "0"))
				(e-lambda
					(args
						(p-ident (raw "acc"))
						(p-ident (raw "x")))
					(e-binop (op "+")
						(e-ident (raw "acc"))
						(e-ident (raw "x"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "sumResult"))
		(e-call
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-list
				(elems
					(e-num (value "1"))
					(e-num (value "2"))
					(e-num (value "3"))
					(e-num (value "4"))))
			(e-num (value "0"))
			(e-lambda
				(args
					(p-assign (ident "acc"))
					(p-assign (ident "x")))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "acc")))
					(e-lookup-local
						(p-assign (ident "x"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
