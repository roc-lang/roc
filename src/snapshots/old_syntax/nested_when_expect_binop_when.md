# META
~~~ini
description=nested_when_expect_binop_when fail
type=expr
~~~
# SOURCE
~~~roc
when s
is
z->expect!%when s
is z->q
~~~
# EXPECTED
UNDEFINED VARIABLE - nested_when_expect_binop_when.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

**nested_when_expect_binop_when.md:1:1:1:5:**
```roc
when s
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),Newline(1:1-1:1),
LowerIdent(2:1-2:3),Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpArrow(3:2-3:4),LowerIdent(3:4-3:11),OpPercent(3:11-3:12),LowerIdent(3:12-3:16),LowerIdent(3:17-3:18),Newline(1:1-1:1),
LowerIdent(4:1-4:3),LowerIdent(4:4-4:5),OpArrow(4:5-4:7),LowerIdent(4:7-4:8),EndOfFile(4:8-4:8),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
