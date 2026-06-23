# META
~~~ini
description=Record field access used in expressions (dot-access)
type=expr
~~~
# SOURCE
~~~roc
person.age + 5
~~~
# EXPECTED
POLYMORPHIC VALUE - record_access_in_expression.md:1:1:1:15
# PROBLEMS

┌───────────────────┐
│ POLYMORPHIC VALUE ├─ This top-level value still has an unresolved ──────────┐
└┬──────────────────┘  polymorphic type.                                      │
 │                                                                            │
 │  person.age + 5                                                            │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                            │
 └──────────────────────────────────────── record_access_in_expression.md:1:1 ┘

    Its type is:
    a where [a.plus : a, Dec -> a]
    Add an annotation or use this value in a way that fixes its concrete type.

# TOKENS
~~~zig
LowerIdent,NoSpaceDotLowerIdent,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "+")
	(e-field-access
		(e-ident (raw "person"))
		(e-ident (raw "age")))
	(e-int (raw "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dispatch-call (method "plus") (constraint-fn-var 47)
	(receiver
		(e-field-access (field "age")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope")))))
	(args
		(e-num (value "5"))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.plus : a, Dec -> a]"))
~~~
