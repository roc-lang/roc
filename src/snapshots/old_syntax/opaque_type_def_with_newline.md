# META
~~~ini
description=opaque_type_def_with_newline fail
type=expr
~~~
# SOURCE
~~~roc
a:e
Na:=
 e e0
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `a` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),LowerIdent(1:3-1:4),Newline(1:1-1:1),
UpperIdent(2:1-2:3),OpColonEqual(2:3-2:5),Newline(1:1-1:1),
LowerIdent(3:2-3:3),LowerIdent(3:4-3:6),EndOfFile(3:6-3:6),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "a"))
~~~
# FORMATTED
~~~roc
a
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
