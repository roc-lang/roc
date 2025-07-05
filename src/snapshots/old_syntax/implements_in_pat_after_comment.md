# META
~~~ini
description=implements_in_pat_after_comment
type=expr
~~~
# SOURCE
~~~roc
s#
 implements:s
s
~~~
# EXPECTED
UNDEFINED VARIABLE - implements_in_pat_after_comment.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `s` in this scope.
Is there an `import` or `exposing` missing up-top?

**implements_in_pat_after_comment.md:1:1:1:2:**
```roc
s#
```
^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),Newline(1:3-1:3),
KwImplements(2:2-2:12),OpColon(2:12-2:13),LowerIdent(2:13-2:14),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "s"))
~~~
# FORMATTED
~~~roc
s
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
