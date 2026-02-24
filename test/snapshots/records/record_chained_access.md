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
LowerIdent,NoSpaceDotLowerIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-field-access
	(e-field-access
		(e-ident (raw "person"))
		(e-ident (raw "address")))
	(e-ident (raw "street")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access (field "street")
	(receiver
		(e-dot-access (field "address")
			(receiver
				(e-runtime-error (tag "ident_not_in_scope"))))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
