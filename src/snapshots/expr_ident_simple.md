# META
~~~ini
description=Simple identifier lookup canonicalization
type=expr
~~~
# SOURCE
~~~roc
foo
~~~
# EXPECTED
UNDEFINED VARIABLE - expr_ident_simple.md:1:1:1:4
# PROBLEMS
**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'foo' is not defined:
**expr_ident_simple.md:1:1:1:4:**
```roc
foo
```
^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.4 (raw "foo"))
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
(expr @1.1-1.1 (type "Error"))
~~~
