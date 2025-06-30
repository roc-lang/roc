# META
~~~ini
description=var_minus_two
type=expr
~~~
# SOURCE
~~~roc
x-2
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpBinaryMinus(1:2-1:3),Int(1:3-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.4 (op "-")
	(e-ident @1.1-1.2 (qaul "") (raw "x"))
	(e-int @1.3-1.4 (raw "2")))
~~~
# FORMATTED
~~~roc
x - 2
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.4 (op "sub") (id 76)
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-int @1.3-1.4 (value "2")))
~~~
# TYPES
~~~clojure
(expr (id 76) (type "*"))
~~~
