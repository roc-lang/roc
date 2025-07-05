# META
~~~ini
description=tuple_funcs_in_parens
type=expr
~~~
# SOURCE
~~~roc
f: (a, b -> c, d -> e, g)
f
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),OpenRound(1:4-1:5),LowerIdent(1:5-1:6),Comma(1:6-1:7),LowerIdent(1:8-1:9),OpArrow(1:10-1:12),LowerIdent(1:13-1:14),Comma(1:14-1:15),LowerIdent(1:16-1:17),OpArrow(1:18-1:20),LowerIdent(1:21-1:22),Comma(1:22-1:23),LowerIdent(1:24-1:25),CloseRound(1:25-1:26),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "f"))
~~~
# FORMATTED
~~~roc
f
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
