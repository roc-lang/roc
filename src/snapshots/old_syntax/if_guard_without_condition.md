# META
~~~ini
description=if_guard_without_condition fail
type=expr
~~~
# SOURCE
~~~roc
when Just 4 is
    Just if ->
        4

    _ ->
        2
~~~
# EXPECTED
UNDEFINED VARIABLE - if_guard_without_condition.md:1:1:1:5
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:5),UpperIdent(1:6-1:10),Int(1:11-1:12),LowerIdent(1:13-1:15),Newline(1:1-1:1),
UpperIdent(2:5-2:9),KwIf(2:10-2:12),OpArrow(2:13-2:15),Newline(1:1-1:1),
Int(3:9-3:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Underscore(5:5-5:6),OpArrow(5:7-5:9),Newline(1:1-1:1),
Int(6:9-6:10),EndOfFile(6:10-6:10),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
