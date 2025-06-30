# META
~~~ini
description=when_newline_after_condition
type=expr
~~~
# SOURCE
~~~roc
when n
#s
is O->1
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),Newline(1:1-1:1),
Newline(2:2-2:3),
LowerIdent(3:1-3:3),UpperIdent(3:4-3:5),OpArrow(3:5-3:7),Int(3:7-3:8),EndOfFile(3:8-3:8),
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
