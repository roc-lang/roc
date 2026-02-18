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
DOES NOT EXIST - external_lookup_expr.md:1:1:1:10
# PROBLEMS
**DOES NOT EXIST**
`Json.utf8` does not exist.

**external_lookup_expr.md:1:1:1:10:**
```roc
Json.utf8
```
^^^^^^^^^


# TOKENS
~~~zig
UpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-ident (raw "Json.utf8"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "qualified_ident_does_not_exist"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
