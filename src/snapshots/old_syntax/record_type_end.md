# META
~~~ini
description=record_type_end fail
type=expr
~~~
# SOURCE
~~~roc
f : { a: Int,
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),OpenCurly(1:5-1:6),LowerIdent(1:7-1:8),OpColon(1:8-1:9),UpperIdent(1:10-1:13),Comma(1:13-1:14),EndOfFile(1:14-1:14),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "f"))
~~~
# FORMATTED
~~~roc
f
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
