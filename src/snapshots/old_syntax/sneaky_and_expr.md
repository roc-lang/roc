# META
~~~ini
description=sneaky_and_expr
type=expr
~~~
# SOURCE
~~~roc
a
{}=ands
d
~~~
# EXPECTED
UNDEFINED VARIABLE - sneaky_and_expr.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `a` in this scope.
Is there an `import` or `exposing` missing up-top?

**sneaky_and_expr.md:1:1:1:2:**
```roc
a
```
^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),Newline(1:1-1:1),
OpenCurly(2:1-2:2),CloseCurly(2:2-2:3),OpAssign(2:3-2:4),LowerIdent(2:4-2:8),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "a"))
~~~
# FORMATTED
~~~roc
a
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
