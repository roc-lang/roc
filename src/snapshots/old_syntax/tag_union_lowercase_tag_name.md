# META
~~~ini
description=tag_union_lowercase_tag_name fail
type=expr
~~~
# SOURCE
~~~roc
f : [lowercase]
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:15),CloseSquare(1:15-1:16),EndOfFile(1:16-1:16),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "f"))
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
