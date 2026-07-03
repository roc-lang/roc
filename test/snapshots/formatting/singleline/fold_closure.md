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
NAME NOT IN SCOPE - fold_closure.md:1:13:1:17
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `fold` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  sumResult = fold([1, 2, 3, 4], 0, |acc, x| acc + x)                       │
 │              ‾‾‾‾                                                          │
 └────────────────────────────────────────────────────── fold_closure.md:1:13 ┘

    Is it misspelled, or is there an import missing?

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
				(e-dispatch-call (method "plus") (constraint-fn-var 190)
					(receiver
						(e-lookup-local
							(p-assign (ident "acc"))))
					(args
						(e-lookup-local
							(p-assign (ident "x")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
