# META
~~~ini
description=basic_field
type=expr
~~~
# SOURCE
~~~roc
rec.field
~~~
# EXPECTED
UNDEFINED VARIABLE - basic_field.md:1:1:1:4
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:4),NoSpaceDotLowerIdent(1:4-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-1.10
	(e-ident @1.1-1.4 (raw "rec"))
	(e-ident @1.4-1.10 (raw "field")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-1.10 (field "field")
	(receiver
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.10 (type "*"))
~~~
