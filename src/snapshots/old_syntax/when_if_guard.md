# META
~~~ini
description=when_if_guard
type=expr
~~~
# SOURCE
~~~roc
when x is
    _ ->
        1

    _ ->
        2

    Ok ->
        3
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
Underscore(2:5-2:6),OpArrow(2:7-2:9),Newline(1:1-1:1),
Int(3:9-3:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Underscore(5:5-5:6),OpArrow(5:7-5:9),Newline(1:1-1:1),
Int(6:9-6:10),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(8:5-8:7),OpArrow(8:8-8:10),Newline(1:1-1:1),
Int(9:9-9:10),EndOfFile(9:10-9:10),
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
