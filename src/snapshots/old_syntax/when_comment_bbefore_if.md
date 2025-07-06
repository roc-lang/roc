# META
~~~ini
description=when_comment_bbefore_if
type=expr
~~~
# SOURCE
~~~roc
when 0
is S#
 if S->e
~~~
# EXPECTED
UNDEFINED VARIABLE - when_comment_bbefore_if.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

**when_comment_bbefore_if.md:1:1:1:5:**
```roc
when 0
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),Int(1:6-1:7),Newline(1:1-1:1),
LowerIdent(2:1-2:3),UpperIdent(2:4-2:5),Newline(2:6-2:6),
KwIf(3:2-3:4),UpperIdent(3:5-3:6),OpArrow(3:6-3:8),LowerIdent(3:8-3:9),EndOfFile(3:9-3:9),
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
