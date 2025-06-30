# META
~~~ini
description=mul_comment_neg
type=expr
~~~
# SOURCE
~~~roc
n*f
#
-f
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `n` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpStar(1:2-1:3),LowerIdent(1:3-1:4),Newline(1:1-1:1),
Newline(2:2-2:2),
OpUnaryMinus(3:1-3:2),LowerIdent(3:2-3:3),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-binop @1.1-3.2 (op "*")
	(e-ident @1.1-1.2 (qaul "") (raw "n"))
	(e-ident @1.3-1.4 (qaul "") (raw "f")))
~~~
# FORMATTED
~~~roc
n * f
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-3.2 (op "mul") (id 77)
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "*"))
~~~
