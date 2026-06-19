# META
~~~ini
description=Mono test: arithmetic expression at top-level
type=mono
~~~
# SOURCE
~~~roc
sum = 1 + 2
~~~
# MONO
~~~roc
sum : Dec
sum = 1 + 2
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
LowerIdent,OpAssign,Int,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "sum"))
			(e-binop (op "+")
				(e-int (raw "1"))
				(e-int (raw "2"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "sum"))
		(e-dispatch-call (method "plus") (constraint-fn-var 77)
			(receiver
				(e-num (value "1")))
			(args
				(e-num (value "2"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec"))))
~~~
