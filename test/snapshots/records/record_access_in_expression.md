# META
~~~ini
description=Record field access used in expressions (field-access)
type=expr
~~~
# SOURCE
~~~roc
person.age + 5
~~~
# EXPECTED
NIL
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
	(e-field-access (field "age")
		(receiver
			(e-runtime-error (tag "ident_not_in_scope"))))
	(e-num (value "5")))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.plus : a, Dec -> a]"))
~~~
