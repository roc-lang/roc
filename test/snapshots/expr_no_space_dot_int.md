# META
~~~ini
description=
type=snippet
~~~
# SOURCE
~~~roc
foo = asd.0
~~~
# EXPECTED
NAME NOT IN SCOPE - expr_no_space_dot_int.md:1:7:1:10
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `asd` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  foo = asd.0                                                               │
 │        ‾‾‾                                                                 │
 └────────────────────────────────────────────── expr_no_space_dot_int.md:1:7 ┘

    Is it misspelled, or is there an import missing?

# TOKENS
~~~zig
LowerIdent,OpAssign,LowerIdent,NoSpaceDotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-tuple-access
				(e-ident (raw "asd"))
				".0"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-tuple-access (index "0")
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
