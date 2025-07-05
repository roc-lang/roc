# META
~~~ini
description=var_is
type=expr
~~~
# SOURCE
~~~roc
isnt
~~~
# EXPECTED
UNDEFINED VARIABLE - var_is.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `isnt` in this scope.
Is there an `import` or `exposing` missing up-top?

**var_is.md:1:1:1:5:**
```roc
isnt
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "isnt"))
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
(expr @1.1-1.5 (type "Error"))
~~~
