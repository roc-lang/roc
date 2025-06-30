# META
~~~ini
description=when_outdented_branch fail
type=expr
~~~
# SOURCE
~~~roc
when 4 is
    5 -> 2
 2 -> 2
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),Int(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
Int(2:5-2:6),OpArrow(2:7-2:9),Int(2:10-2:11),Newline(1:1-1:1),
Int(3:2-3:3),OpArrow(3:4-3:6),Int(3:7-3:8),EndOfFile(3:8-3:8),
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
