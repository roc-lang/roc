# META
~~~ini
description=type_tuple_where_annotation
type=expr
~~~
# SOURCE
~~~roc
nextWhileLess : List Bucket, k, U8 -> (U64, U32) where k implements Hash & Eq
nextWhileLess = \buckets, key, shifts -> foo
nextWhileLess
~~~
# EXPECTED
UNDEFINED VARIABLE - type_tuple_where_annotation.md:1:1:1:14
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `nextWhileLess` in this scope.
Is there an `import` or `exposing` missing up-top?

**type_tuple_where_annotation.md:1:1:1:14:**
```roc
nextWhileLess : List Bucket, k, U8 -> (U64, U32) where k implements Hash & Eq
```
^^^^^^^^^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:14),OpColon(1:15-1:16),UpperIdent(1:17-1:21),UpperIdent(1:22-1:28),Comma(1:28-1:29),LowerIdent(1:30-1:31),Comma(1:31-1:32),UpperIdent(1:33-1:35),OpArrow(1:36-1:38),OpenRound(1:39-1:40),UpperIdent(1:40-1:43),Comma(1:43-1:44),UpperIdent(1:45-1:48),CloseRound(1:48-1:49),KwWhere(1:50-1:55),LowerIdent(1:56-1:57),KwImplements(1:58-1:68),UpperIdent(1:69-1:73),OpAmpersand(1:74-1:75),UpperIdent(1:76-1:78),Newline(1:1-1:1),
LowerIdent(2:1-2:14),OpAssign(2:15-2:16),OpBackslash(2:17-2:18),LowerIdent(2:18-2:25),Comma(2:25-2:26),LowerIdent(2:27-2:30),Comma(2:30-2:31),LowerIdent(2:32-2:38),OpArrow(2:39-2:41),LowerIdent(2:42-2:45),Newline(1:1-1:1),
LowerIdent(3:1-3:14),EndOfFile(3:14-3:14),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.14 (raw "nextWhileLess"))
~~~
# FORMATTED
~~~roc
nextWhileLess
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.14 (type "Error"))
~~~
