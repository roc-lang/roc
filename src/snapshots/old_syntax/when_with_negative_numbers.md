# META
~~~ini
description=when_with_negative_numbers
type=expr
~~~
# SOURCE
~~~roc
when x is
 1 -> 2
 -3 -> 4
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
Int(2:2-2:3),OpArrow(2:4-2:6),Int(2:7-2:8),Newline(1:1-1:1),
Int(3:2-3:4),OpArrow(3:5-3:7),Int(3:8-3:9),EndOfFile(3:9-3:9),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (qaul "") (raw "when"))
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
