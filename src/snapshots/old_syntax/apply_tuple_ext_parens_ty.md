# META
~~~ini
description=apply_tuple_ext_parens_ty
type=expr
~~~
# SOURCE
~~~roc
i:M()(Y) c
t
~~~
# EXPECTED
UNDEFINED VARIABLE - apply_tuple_ext_parens_ty.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `i` in this scope.
Is there an `import` or `exposing` missing up-top?

**apply_tuple_ext_parens_ty.md:1:1:1:2:**
```roc
i:M()(Y) c
```
^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),UpperIdent(1:3-1:4),NoSpaceOpenRound(1:4-1:5),CloseRound(1:5-1:6),NoSpaceOpenRound(1:6-1:7),UpperIdent(1:7-1:8),CloseRound(1:8-1:9),LowerIdent(1:10-1:11),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "i"))
~~~
# FORMATTED
~~~roc
i
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
