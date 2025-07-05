# META
~~~ini
description=annotation_tuple_parens_newlines
type=expr
~~~
# SOURCE
~~~roc
p:(
)(
i)
{}
~~~
# EXPECTED
UNDEFINED VARIABLE - annotation_tuple_parens_newlines.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `p` in this scope.
Is there an `import` or `exposing` missing up-top?

**annotation_tuple_parens_newlines.md:1:1:1:2:**
```roc
p:(
```
^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Newline(1:1-1:1),
CloseRound(2:1-2:2),NoSpaceOpenRound(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:1-3:2),CloseRound(3:2-3:3),Newline(1:1-1:1),
OpenCurly(4:1-4:2),CloseCurly(4:2-4:3),EndOfFile(4:3-4:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "p"))
~~~
# FORMATTED
~~~roc
p
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
