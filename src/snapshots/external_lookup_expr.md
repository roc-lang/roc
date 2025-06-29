# META
~~~ini
description=External declaration lookup expression
type=expr
~~~
# SOURCE
~~~roc
Json.utf8
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `utf8` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
UpperIdent(1:1-1:5),NoSpaceDotLowerIdent(1:5-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-ident @1-1-1-10 (qaul "Json") (raw ".utf8"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
