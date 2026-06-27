# META
~~~ini
description=A self-referential value binding (x = x) should be a diagnostic, not a crash
type=snippet
~~~
# SOURCE
~~~roc
x = x

main = x
~~~
# EXPECTED
INVALID ASSIGNMENT TO ITSELF - generalize_alias_self_reference.md:1:5:1:6
# PROBLEMS

┌──────────────────────────────┐
│ INVALID ASSIGNMENT TO ITSELF ├─ The value `x` is assigned to itself, ───────┐
└┬─────────────────────────────┘  which would cause an infinite loop at       │
 │                                runtime.                                    │
 │                                                                            │
 │  x = x                                                                     │
 │      ‾                                                                     │
 └──────────────────────────────────── generalize_alias_self_reference.md:1:5 ┘

    Only functions can reference themselves (for recursion). For non-function
    values, the right-hand side must be fully computable without referring to
    the value being assigned.

# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-ident (raw "x")))
		(s-decl
			(p-ident (raw "main"))
			(e-ident (raw "x")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "self_referential_definition")))
	(d-let
		(p-assign (ident "main"))
		(e-lookup-local
			(p-assign (ident "x")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
