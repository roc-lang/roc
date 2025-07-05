# META
~~~ini
description=sub_var_with_spaces
type=expr
~~~
# SOURCE
~~~roc
x - 2
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpBinaryMinus(1:3-1:4),Int(1:5-1:6),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.2 (op "-")
	(e-ident @1.1-1.2 (raw "x"))
	(e-int @1.5-1.6 (raw "2")))
~~~
# FORMATTED
~~~roc
x - 2
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.2 (op "sub")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-int @1.5-1.6 (value "2")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "*"))
~~~
