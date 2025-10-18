# META
~~~ini
description=Record field access with function call
type=expr
~~~
# SOURCE
~~~roc
(person.transform)(42)
~~~
# EXPECTED
UNDEFINED VARIABLE - record_access_function_call.md:1:2:1:8
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_access_function_call.md:1:2:1:8:**
```roc
(person.transform)(42)
```
 ^^^^^^


# TOKENS
~~~zig
OpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-field-access
			(e-ident (raw "person"))
			(e-ident (raw "transform"))))
	(e-int (raw "42")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-dot-access (field "transform")
		(receiver
			(e-runtime-error (tag "ident_not_in_scope"))))
	(e-num (value "42")))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
