# META
~~~ini
description=equals_with_spaces
type=expr
~~~
# SOURCE
~~~roc
x == y
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `y` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpEquals(1:3-1:5),LowerIdent(1:6-1:7),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.2 (op "==")
	(e-ident @1.1-1.2 (raw "x"))
	(e-ident @1.6-1.7 (raw "y")))
~~~
# FORMATTED
~~~roc
x == y
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.2 (op "eq")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "*"))
~~~
