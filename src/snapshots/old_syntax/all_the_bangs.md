# META
~~~ini
description=all_the_bangs fail
type=expr
~~~
# SOURCE
~~~roc
p
!
.p!!
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `p` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),Newline(1:1-1:1),
OpBang(2:1-2:2),Newline(1:1-1:1),
DotLowerIdent(3:1-3:5),EndOfFile(3:5-3:5),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "p"))
~~~
# FORMATTED
~~~roc
p
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
