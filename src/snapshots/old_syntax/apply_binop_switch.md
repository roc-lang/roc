# META
~~~ini
description=apply_binop_switch
type=expr
~~~
# SOURCE
~~~roc
i<2
(-6)
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpLessThan(1:2-1:3),Int(1:3-1:4),Newline(1:1-1:1),
OpenRound(2:1-2:2),OpBinaryMinus(2:2-2:3),Int(2:3-2:4),CloseRound(2:4-2:5),EndOfFile(2:5-2:5),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.2 (op "<")
	(e-ident @1.1-1.2 (qaul "") (raw "i"))
	(e-int @1.3-1.4 (raw "2")))
~~~
# FORMATTED
~~~roc
i < 2
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.2 (op "lt") (id 76)
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-int @1.3-1.4 (value "2")))
~~~
# TYPES
~~~clojure
(expr (id 76) (type "*"))
~~~
