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
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,Number,Comma,Number,Comma,Number,Comma,Number,CloseSquare,Comma,Number,Comma,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,CloseRound,
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
					(e-num (raw "1"))
					(e-num (raw "2"))
					(e-num (raw "3"))
					(e-num (raw "4")))
				(e-num (raw "0"))
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
			(e-lookup-global (ident "fold") (module-id "_Num") (toplevel true))
			(e-list
				(elems
					(e-num (value "1"))
					(e-num (value "2"))
					(e-num (value "3"))
					(e-num (value "4"))))
			(e-num (value "0"))
			(e-closure
				(e-lambda
					(args
						(p-assign (ident "acc"))
						(p-assign (ident "x")))
					(e-call
						(e-lookup-global (ident "add") (module-id "_Num") (toplevel true))
						(e-lookup-local
							(p-assign (ident "acc")))
						(e-lookup-local
							(p-assign (ident "x")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(a)")))
	(expressions
		(expr (type "Num(a)"))))
~~~
