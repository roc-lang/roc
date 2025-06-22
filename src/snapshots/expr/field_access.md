# META
~~~ini
description=Field access expression simple expression
type=expr
~~~
# SOURCE
~~~roc
person.name
~~~
# PROBLEMS
~~~txt
NOT IMPLEMENTED
This feature is not yet implemented: canonicalize record field_access expression
~~~
# TOKENS
~~~zig
LowerIdent(1:1-1:7),NoSpaceDotLowerIdent(1:7-1:12),EndOfFile(1:12-1:12),
~~~
# PARSE
~~~clojure
(field_access (1:1-1:12)
	(binop (1:1-1:12)
		"person"
		(ident (1:1-1:7) "" "person")
		(ident (1:7-1:12) "" ".name")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_runtime_error (1:1-1:1) "not_implemented")
~~~
# TYPES
~~~clojure
(expr 13 (type "Error"))
~~~