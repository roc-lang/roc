# META
~~~ini
description=type_annotation_double_colon fail
type=expr
~~~
# SOURCE
~~~roc
f :: I64
f = 42

f
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),OpColon(1:4-1:5),UpperIdent(1:6-1:9),Newline(1:1-1:1),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),Int(2:5-2:7),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
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
