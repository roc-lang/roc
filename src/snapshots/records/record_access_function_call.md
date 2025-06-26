# META
~~~ini
description=Record field access with function call
type=expr
~~~
# SOURCE
~~~roc
(person.transform)(42)
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `person` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:8),NoSpaceDotLowerIdent(1:8-1:18),CloseRound(1:18-1:19),NoSpaceOpenRound(1:19-1:20),Int(1:20-1:22),CloseRound(1:22-1:23),EndOfFile(1:23-1:23),
~~~
# PARSE
~~~clojure
(e-apply @1-1-1-23
	(e-tuple @1-1-1-19
		(e-field-access @1-2-1-19
			(e-ident @1-2-1-8 (qaul "") (raw "person"))
			(e-ident @1-8-1-18 (qaul "") (raw ".transform"))))
	(e-int @1-20-1-22 (raw "42")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1-1-1-23 (id 80)
	(e-tuple @1-1-1-19 (tuple-var 75)
		(elems
			(e-dot-access @1-2-1-19 (field "transform")
				(receiver
					(e-runtime-error (tag "ident_not_in_scope"))))))
	(e-int @1-20-1-22 (num-var 79) (sign-needed "false") (bits-needed "7") (value "42")))
~~~
# TYPES
~~~clojure
(expr (id 80) (type "*"))
~~~