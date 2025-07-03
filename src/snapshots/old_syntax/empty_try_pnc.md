# META
~~~ini
description=empty_try_pnc
type=expr
~~~
# SOURCE
~~~roc
try()t
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `try` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),NoSpaceOpenRound(1:4-1:5),CloseRound(1:5-1:6),LowerIdent(1:6-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.6
	(e-ident @1.1-1.4 (qaul "") (raw "try")))
~~~
# FORMATTED
~~~roc
try()
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.6
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "a"))
~~~
