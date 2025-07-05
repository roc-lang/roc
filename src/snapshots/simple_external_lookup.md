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
(e-ident @1.1-1.9 (raw "List.map"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.9 (type "Error"))
~~~
