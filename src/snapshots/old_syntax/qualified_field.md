# META
~~~ini
description=qualified_field
type=expr
~~~
# SOURCE
~~~roc
One.Two.rec.abc.def.ghi
~~~
# EXPECTED
UNDEFINED VARIABLE - qualified_field.md:1:1:1:24
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `ghi` in this scope.
Is there an `import` or `exposing` missing up-top?

**qualified_field.md:1:1:1:24:**
```roc
One.Two.rec.abc.def.ghi
```
^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:4),NoSpaceDotUpperIdent(1:4-1:8),NoSpaceDotLowerIdent(1:8-1:12),NoSpaceDotLowerIdent(1:12-1:16),NoSpaceDotLowerIdent(1:16-1:20),NoSpaceDotLowerIdent(1:20-1:24),EndOfFile(1:24-1:24),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.24 (raw "One.Two.rec.abc.def.ghi"))
~~~
# FORMATTED
~~~roc
One.ghi
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.24 (type "Error"))
~~~
