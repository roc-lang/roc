# META
~~~ini
description=try_subtract
type=expr
~~~
# SOURCE
~~~roc
try-w
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `try` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `w` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpBinaryMinus(1:4-1:5),LowerIdent(1:5-1:6),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.2 (op "-")
	(e-ident @1.1-1.4 (raw "try"))
	(e-ident @1.5-1.6 (raw "w")))
~~~
# FORMATTED
~~~roc
try - w
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.2 (op "sub")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "*"))
~~~
