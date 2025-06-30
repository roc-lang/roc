# META
~~~ini
description=parenthetical_var
type=expr
~~~
# SOURCE
~~~roc
(whee)
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `whee` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:6),CloseRound(1:6-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.7
	(e-ident @1.2-1.6 (qaul "") (raw "whee")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-1.7 (id 75)
	(elems
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr (id 75) (type "(Error)"))
~~~
