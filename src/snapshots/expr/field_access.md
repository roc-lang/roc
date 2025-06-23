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
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

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
(e_dot_access (1:1-1:12)
	(e_runtime_error (1:1-1:7) "ident_not_in_scope")
	"name")
~~~
# TYPES
~~~clojure
(expr 74 (type "*"))
~~~