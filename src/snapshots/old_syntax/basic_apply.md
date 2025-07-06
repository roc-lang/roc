# META
~~~ini
description=basic_apply
type=expr
~~~
# SOURCE
~~~roc
whee 1
~~~
# EXPECTED
UNDEFINED VARIABLE - basic_apply.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `whee` in this scope.
Is there an `import` or `exposing` missing up-top?

**basic_apply.md:1:1:1:5:**
```roc
whee 1
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),Int(1:6-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "whee"))
~~~
# FORMATTED
~~~roc
whee
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
