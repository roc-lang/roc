# META
~~~ini
description=when_missing_arrow fail
type=expr
~~~
# SOURCE
~~~roc
when 5 is
    1 -> 2
    _
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),Int(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
Int(2:5-2:6),OpArrow(2:7-2:9),Int(2:10-2:11),Newline(1:1-1:1),
Underscore(3:5-3:6),EndOfFile(3:6-3:6),
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
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
