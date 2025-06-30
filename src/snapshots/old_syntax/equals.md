# META
~~~ini
description=equals
type=expr
~~~
# SOURCE
~~~roc
x==y
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `y` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpEquals(1:2-1:4),LowerIdent(1:4-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.5 (op "==")
	(e-ident @1.1-1.2 (qaul "") (raw "x"))
	(e-ident @1.4-1.5 (qaul "") (raw "y")))
~~~
# FORMATTED
~~~roc
x == y
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.5 (op "eq") (id 77)
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "*"))
~~~
