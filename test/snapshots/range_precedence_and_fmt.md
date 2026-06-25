# META
~~~ini
description=Ranges bind loosest and format without spaces
type=snippet
~~~
# SOURCE
~~~roc
n = 3
r = 1 ..< n + 1
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,OpDoubleDotLessThan,LowerIdent,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "n"))
			(e-int (raw "3")))
		(s-decl
			(p-ident (raw "r"))
			(e-binop (op "..<")
				(e-int (raw "1"))
				(e-binop (op "+")
					(e-ident (raw "n"))
					(e-int (raw "1")))))))
~~~
# FORMATTED
~~~roc
n = 3

r = 1..<n + 1
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "n"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "r"))
		(e-call (constraint-fn-var 242)
			(e-lookup-external
				(builtin))
			(e-num (value "1"))
			(e-dispatch-call (method "plus") (constraint-fn-var 240)
				(receiver
					(e-lookup-local
						(p-assign (ident "n"))))
				(args
					(e-num (value "1")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Iter(Dec)")))
	(expressions
		(expr (type "Dec"))
		(expr (type "Iter(Dec)"))))
~~~
