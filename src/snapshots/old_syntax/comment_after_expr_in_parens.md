# META
~~~ini
description=comment_after_expr_in_parens
type=expr
~~~
# SOURCE
~~~roc
(i#abc
)
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:3),Newline(1:4-1:7),
CloseRound(2:1-2:2),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-2.2
	(e-ident @1.2-1.3 (raw "i")))
~~~
# FORMATTED
~~~roc
(
	i, # abc
)
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.2-1.3 (type "Error"))
~~~
