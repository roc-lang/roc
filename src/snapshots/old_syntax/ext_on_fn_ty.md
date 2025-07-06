# META
~~~ini
description=ext_on_fn_ty
type=expr
~~~
# SOURCE
~~~roc
t:(w=>p)a
t
~~~
# EXPECTED
UNDEFINED VARIABLE - ext_on_fn_ty.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `t` in this scope.
Is there an `import` or `exposing` missing up-top?

**ext_on_fn_ty.md:1:1:1:2:**
```roc
t:(w=>p)a
```
^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),OpFatArrow(1:5-1:7),LowerIdent(1:7-1:8),CloseRound(1:8-1:9),LowerIdent(1:9-1:10),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "t"))
~~~
# FORMATTED
~~~roc
t
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
