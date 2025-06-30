# META
~~~ini
description=basic_field
type=expr
~~~
# SOURCE
~~~roc
rec.field
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `rec` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),NoSpaceDotLowerIdent(1:4-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-1.10
	(e-ident @1.1-1.4 (qaul "") (raw "rec"))
	(e-ident @1.4-1.10 (qaul "") (raw ".field")))
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
