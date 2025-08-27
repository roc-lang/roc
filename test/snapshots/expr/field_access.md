# META
~~~ini
description=Field access expression simple expression
type=expr
~~~
# SOURCE
~~~roc
person.name
~~~
# EXPECTED
UNDEFINED VARIABLE - field_access.md:1:1:1:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**field_access.md:1:1:1:7:**
```roc
person.name
```
^^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:7),NoSpaceDotLowerIdent(1:7-1:12),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-1.12
	(e-ident @1.1-1.7 (raw "person"))
	(e-ident @1.7-1.12 (raw "name")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-1.12 (field "name")
	(receiver
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.12 (type "_a"))
~~~
