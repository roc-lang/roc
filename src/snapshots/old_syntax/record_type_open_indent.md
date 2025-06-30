# META
~~~ini
description=record_type_open_indent fail
type=expr
~~~
# SOURCE
~~~roc
f : {
foo : I64,
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),OpenCurly(1:5-1:6),Newline(1:1-1:1),
LowerIdent(2:1-2:4),OpColon(2:5-2:6),UpperIdent(2:7-2:10),Comma(2:10-2:11),EndOfFile(2:11-2:11),
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
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
