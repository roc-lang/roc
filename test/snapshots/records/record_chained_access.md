# META
~~~ini
description=Chained record field (dot-access)
type=expr
~~~
# SOURCE
~~~roc
person.address.street
~~~
# EXPECTED
UNDEFINED VARIABLE - record_chained_access.md:1:1:1:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_chained_access.md:1:1:1:7:**
```roc
person.address.street
```
^^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:7),NoSpaceDotLowerIdent(1:7-1:15),NoSpaceDotLowerIdent(1:15-1:22),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-field-access @1.1-1.22
	(e-field-access @1.1-1.15
		(e-ident @1.1-1.7 (raw "person"))
		(e-ident @1.7-1.15 (raw "address")))
	(e-ident @1.15-1.22 (raw "street")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1.1-1.22 (field "street")
	(receiver
		(e-dot-access @1.1-1.15 (field "address")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope"))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.22 (type "_a"))
~~~
