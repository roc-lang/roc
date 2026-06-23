# META
~~~ini
description=Inclusive range expression desugars to to and types as Iter
type=snippet
~~~
# SOURCE
~~~roc
r = 1..=5
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,OpDoubleDotEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "r"))
			(e-binop (op "..=")
				(e-int (raw "1"))
				(e-int (raw "5"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-call (constraint-fn-var 208)
			(e-lookup-external
				(builtin))
			(e-num (value "1"))
			(e-num (value "5")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Iter(Dec)")))
	(expressions
		(expr (type "Iter(Dec)"))))
~~~
