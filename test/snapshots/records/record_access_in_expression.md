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
UNDEFINED VARIABLE - record_access_in_expression.md:1:1:1:7
# PROBLEMS
NIL
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
(e-binop (op "add")
	(e-dot-access (field "age")
		(receiver
			(e-lookup-local
				(p-assign (ident "person")))))
	(e-num (value "5")))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.plus : a, Dec -> a]"))
~~~
