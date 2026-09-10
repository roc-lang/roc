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
		(receiver
			(e-ident (raw "person")))
		(segment (mode "required") (field "age")))
	(e-int (raw "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop (op "add")
	(e-field-access
		(receiver
			(e-runtime-error (tag "ident_not_in_scope")))
		(segments
			(segment (name "age") (mode "required"))))
	(e-num (value "5")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
