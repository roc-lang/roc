# META
~~~ini
description=list_minus_newlines
type=expr
~~~
# SOURCE
~~~roc
[K,
]-i
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenSquare(1:1-1:2),UpperIdent(1:2-1:3),Comma(1:3-1:4),Newline(1:1-1:1),
CloseSquare(2:1-2:2),OpBinaryMinus(2:2-2:3),LowerIdent(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.4 (op "-")
	(e-list @1.1-2.2
		(e-tag @1.2-1.3 (raw "K")))
	(e-ident @2.3-2.4 (raw "i")))
~~~
# FORMATTED
~~~roc
[
	K,
] - i
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.4 (op "sub")
	(e-list @1.1-2.2
		(elems
			(e-tag @1.2-1.3 (name "K"))))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.4 (type "*"))
~~~
