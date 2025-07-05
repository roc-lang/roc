# META
~~~ini
description=parenthetical_var
type=expr
~~~
# SOURCE
~~~roc
(whee)
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `whee` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:6),CloseRound(1:6-1:7),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.7
	(e-ident @1.2-1.6 (raw "whee")))
~~~
# FORMATTED
~~~roc
(whee)
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.2-1.6 (type "Error"))
~~~
