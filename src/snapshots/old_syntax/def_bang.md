# META
~~~ini
description=def_bang
type=expr
~~~
# SOURCE
~~~roc
launchTheNukes! = \{} ->
    boom

launchTheNukes! {}
~~~
# EXPECTED
UNDEFINED VARIABLE - def_bang.md:1:1:1:16
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `launchTheNukes!` in this scope.
Is there an `import` or `exposing` missing up-top?

**def_bang.md:1:1:1:16:**
```roc
launchTheNukes! = \{} ->
```
^^^^^^^^^^^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:16),OpAssign(1:17-1:18),OpBackslash(1:19-1:20),OpenCurly(1:20-1:21),CloseCurly(1:21-1:22),OpArrow(1:23-1:25),Newline(1:1-1:1),
LowerIdent(2:5-2:9),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:16),OpenCurly(4:17-4:18),CloseCurly(4:18-4:19),EndOfFile(4:19-4:19),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.16 (raw "launchTheNukes!"))
~~~
# FORMATTED
~~~roc
launchTheNukes!
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.16 (type "Error"))
~~~
