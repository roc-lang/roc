# META
~~~ini
description=empty_or_pattern fail
type=expr
~~~
# SOURCE
~~~roc
when Just 4 is
    Just 4 | ->
        4

    _ ->
        2
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),UpperIdent(1:6-1:10),Int(1:11-1:12),LowerIdent(1:13-1:15),Newline(1:1-1:1),
UpperIdent(2:5-2:9),Int(2:10-2:11),OpBar(2:12-2:13),OpArrow(2:14-2:16),Newline(1:1-1:1),
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
