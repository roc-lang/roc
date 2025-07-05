# META
~~~ini
description=stmt_parens_minus
type=expr
~~~
# SOURCE
~~~roc
i
(-2)
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),Newline(1:1-1:1),
OpenRound(2:1-2:2),OpBinaryMinus(2:2-2:3),Int(2:3-2:4),CloseRound(2:4-2:5),EndOfFile(2:5-2:5),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "i"))
~~~
# FORMATTED
~~~roc
i
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
