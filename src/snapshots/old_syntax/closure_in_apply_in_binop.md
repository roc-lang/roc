# META
~~~ini
description=closure_in_apply_in_binop
type=expr
~~~
# SOURCE
~~~roc
m0\w->w?e
/s
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `m0` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:3),OpBackslash(1:3-1:4),LowerIdent(1:4-1:5),OpArrow(1:5-1:7),LowerIdent(1:7-1:8),NoSpaceOpQuestion(1:8-1:9),LowerIdent(1:9-1:10),Newline(1:1-1:1),
OpSlash(2:1-2:2),LowerIdent(2:2-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.3 (qaul "") (raw "m0"))
~~~
# FORMATTED
~~~roc
m0
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
