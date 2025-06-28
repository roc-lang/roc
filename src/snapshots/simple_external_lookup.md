# META
~~~ini
description=Simple external declaration lookup
type=expr
~~~
# SOURCE
~~~roc
List.map
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `map` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
UpperIdent(1:1-1:5),NoSpaceDotLowerIdent(1:5-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-ident @1-1-1-9 (qaul "List") (raw ".map"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Error"))
~~~
