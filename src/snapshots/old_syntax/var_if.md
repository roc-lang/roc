# META
~~~ini
description=var_if
type=expr
~~~
# SOURCE
~~~roc
iffy
~~~
# EXPECTED
UNDEFINED VARIABLE - var_if.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `iffy` in this scope.
Is there an `import` or `exposing` missing up-top?

**var_if.md:1:1:1:5:**
```roc
iffy
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "iffy"))
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
