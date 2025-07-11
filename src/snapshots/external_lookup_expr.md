# META
~~~ini
description=External declaration lookup expression
type=expr
~~~
# SOURCE
~~~roc
Json.utf8
~~~
# EXPECTED
UNDEFINED VARIABLE - external_lookup_expr.md:1:1:1:10
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `utf8` in this scope.
Is there an `import` or `exposing` missing up-top?

**external_lookup_expr.md:1:1:1:10:**
```roc
Json.utf8
```
^^^^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:5),NoSpaceDotLowerIdent(1:5-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.10 (raw "Json.utf8"))
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
(expr @1.1-1.10 (type "Error"))
~~~
