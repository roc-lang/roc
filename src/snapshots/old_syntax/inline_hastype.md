# META
~~~ini
description=inline_hastype fail
type=expr
~~~
# SOURCE
~~~roc
main =
    (\x -> x) : I64

    3
~~~
# EXPECTED
UNDEFINED VARIABLE - inline_hastype.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `main` in this scope.
Is there an `import` or `exposing` missing up-top?

**inline_hastype.md:1:1:1:5:**
```roc
main =
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpAssign(1:6-1:7),Newline(1:1-1:1),
OpenRound(2:5-2:6),OpBackslash(2:6-2:7),LowerIdent(2:7-2:8),OpArrow(2:9-2:11),LowerIdent(2:12-2:13),CloseRound(2:13-2:14),OpColon(2:15-2:16),UpperIdent(2:17-2:20),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(4:5-4:6),EndOfFile(4:6-4:6),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "main"))
~~~
# FORMATTED
~~~roc
main
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
