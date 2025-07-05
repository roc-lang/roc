# META
~~~ini
description=record_type_missing_comma fail
type=expr
~~~
# SOURCE
~~~roc
f : { foo  bar }
~~~
# EXPECTED
UNDEFINED VARIABLE - record_type_missing_comma.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_type_missing_comma.md:1:1:1:2:**
```roc
f : { foo  bar }
```
^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),OpenCurly(1:5-1:6),LowerIdent(1:7-1:10),LowerIdent(1:12-1:15),CloseCurly(1:16-1:17),EndOfFile(1:17-1:17),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "f"))
~~~
# FORMATTED
~~~roc
f
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
