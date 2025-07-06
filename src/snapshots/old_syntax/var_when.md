# META
~~~ini
description=var_when
type=expr
~~~
# SOURCE
~~~roc
whenever
~~~
# EXPECTED
UNDEFINED VARIABLE - var_when.md:1:1:1:9
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `whenever` in this scope.
Is there an `import` or `exposing` missing up-top?

**var_when.md:1:1:1:9:**
```roc
whenever
```
^^^^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.9 (raw "whenever"))
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
