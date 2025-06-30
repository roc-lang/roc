# META
~~~ini
description=try_subtract
type=expr
~~~
# SOURCE
~~~roc
try-w
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `try` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `w` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpBinaryMinus(1:4-1:5),LowerIdent(1:5-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.6 (op "-")
	(e-ident @1.1-1.4 (qaul "") (raw "try"))
	(e-ident @1.5-1.6 (qaul "") (raw "w")))
~~~
# FORMATTED
~~~roc
try - w
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.6 (op "sub") (id 77)
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "*"))
~~~
