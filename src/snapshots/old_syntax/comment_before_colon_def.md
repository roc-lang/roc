# META
~~~ini
description=comment_before_colon_def
type=expr
~~~
# SOURCE
~~~roc
w#
:n
Q
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `w` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),Newline(1:3-1:3),
OpColon(2:1-2:2),LowerIdent(2:2-2:3),Newline(1:1-1:1),
UpperIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "w"))
~~~
# FORMATTED
~~~roc
w
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
