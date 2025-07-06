# META
~~~ini
description=where_clause_multiple_has
type=expr
~~~
# SOURCE
~~~roc
f : a -> (b -> c) where a implements A, b implements Eq, c implements Ord

f
~~~
# EXPECTED
UNDEFINED VARIABLE - where_clause_multiple_has.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

**where_clause_multiple_has.md:1:1:1:2:**
```roc
f : a -> (b -> c) where a implements A, b implements Eq, c implements Ord
```
^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),LowerIdent(1:5-1:6),OpArrow(1:7-1:9),OpenRound(1:10-1:11),LowerIdent(1:11-1:12),OpArrow(1:13-1:15),LowerIdent(1:16-1:17),CloseRound(1:17-1:18),KwWhere(1:19-1:24),LowerIdent(1:25-1:26),KwImplements(1:27-1:37),UpperIdent(1:38-1:39),Comma(1:39-1:40),LowerIdent(1:41-1:42),KwImplements(1:43-1:53),UpperIdent(1:54-1:56),Comma(1:56-1:57),LowerIdent(1:58-1:59),KwImplements(1:60-1:70),UpperIdent(1:71-1:74),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
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
