# META
~~~ini
description=unary_negation_access
type=expr
~~~
# SOURCE
~~~roc
-rec1.field
~~~
# EXPECTED
POLYMORPHIC VALUE - unary_negation_access.md:1:1:1:12
# PROBLEMS

┌───────────────────┐
│ POLYMORPHIC VALUE ├─ This top-level value still has an unresolved ──────────┐
└┬──────────────────┘  polymorphic type.                                      │
 │                                                                            │
 │  -rec1.field                                                               │
 │  ‾‾‾‾‾‾‾‾‾‾‾                                                               │
 └────────────────────────────────────────────── unary_negation_access.md:1:1 ┘

    Its type is:
    a where [a.negate : a -> a]
    Add an annotation or use this value in a way that fixes its concrete type.

# TOKENS
~~~zig
OpUnaryMinus,LowerIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "-"
	(e-field-access
		(e-ident (raw "rec1"))
		(e-ident (raw "field"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dispatch-call (method "negate") (constraint-fn-var 13)
	(receiver
		(e-field-access (field "field")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(args))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.negate : a -> a]"))
~~~
