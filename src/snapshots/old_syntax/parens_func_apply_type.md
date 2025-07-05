# META
~~~ini
description=parens_func_apply_type
type=expr
~~~
# SOURCE
~~~roc
si:(e)(e->A)
A
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `si` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:3),OpColon(1:3-1:4),NoSpaceOpenRound(1:4-1:5),LowerIdent(1:5-1:6),CloseRound(1:6-1:7),NoSpaceOpenRound(1:7-1:8),LowerIdent(1:8-1:9),OpArrow(1:9-1:11),UpperIdent(1:11-1:12),CloseRound(1:12-1:13),Newline(1:1-1:1),
UpperIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.3 (raw "si"))
~~~
# FORMATTED
~~~roc
si
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "Error"))
~~~
