# META
~~~ini
description=Add a variable with spaces
type=snippet
~~~
# SOURCE
~~~roc
add2 = x +      2
~~~
# EXPECTED
NAME NOT IN SCOPE - add_var_with_spaces.md:1:8:1:9
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `x` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  add2 = x +      2                                                         │
 │         ‾                                                                  │
 └──────────────────────────────────────────────── add_var_with_spaces.md:1:8 ┘

    Is it misspelled, or is there an import missing?

# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "add2"))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
add2 = x + 2
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "add2"))
		(e-dispatch-call (method "plus") (constraint-fn-var 45)
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))
			(args
				(e-num (value "2"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
