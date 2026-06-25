# META
~~~ini
description=Issue 9372 - Formatter must not drop parentheses around an arrow-dispatched, inline lambda that is then called
type=snippet
~~~
# SOURCE
~~~roc
test1 = 10->(|x| x + 1)()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,OpArrow,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,CloseRound,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "test1"))
			(e-arrow-call
				(e-int (raw "10"))
				(e-apply
					(e-lambda
						(args
							(p-ident (raw "x")))
						(e-binop (op "+")
							(e-ident (raw "x"))
							(e-int (raw "1")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "test1"))
		(e-call (constraint-fn-var 83)
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-dispatch-call (method "plus") (constraint-fn-var 48)
					(receiver
						(e-lookup-local
							(p-assign (ident "x"))))
					(args
						(e-num (value "1")))))
			(e-num (value "10")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec"))))
~~~
