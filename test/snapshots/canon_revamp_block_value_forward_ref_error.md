# META
~~~ini
description=Block-local value cannot be used before declaration
type=snippet
~~~
# SOURCE
~~~roc
x = {
    y + 1
    y = 5
}
~~~
# EXPECTED
NAME NOT IN SCOPE - canon_revamp_block_value_forward_ref_error.md:2:5:2:6
UNUSED VARIABLE - canon_revamp_block_value_forward_ref_error.md:3:5:3:6
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `y` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  y + 1                                                                     │
 │  ‾                                                                         │
 └───────────────────────── canon_revamp_block_value_forward_ref_error.md:2:5 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `y` is defined here and then never used. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  y = 5                                                                     │
 │  ‾                                                                         │
 └───────────────────────── canon_revamp_block_value_forward_ref_error.md:3:5 ┘

    If you don't need this variable, prefix it with an underscore like `_y` to
    suppress this warning.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-block
				(statements
					(e-binop (op "+")
						(e-ident (raw "y"))
						(e-int (raw "1")))
					(s-decl
						(p-ident (raw "y"))
						(e-int (raw "5"))))))))
~~~
# FORMATTED
~~~roc
x = {
	y + 1
	y = 5
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-block
			(s-expr
				(e-dispatch-call (method "plus") (constraint-fn-var 52)
					(receiver
						(e-runtime-error (tag "ident_not_in_scope")))
					(args
						(e-num (value "1")))))
			(s-let
				(p-assign (ident "y"))
				(e-num (value "5")))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{}")))
	(expressions
		(expr (type "{}"))))
~~~
