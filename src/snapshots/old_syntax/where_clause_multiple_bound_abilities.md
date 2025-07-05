# META
~~~ini
description=where_clause_multiple_bound_abilities
type=expr
~~~
# SOURCE
~~~roc
f : a -> b where a implements Hash & Eq, b implements Eq & Hash & Display

f : a -> b
  where a implements Hash & Eq,
    b implements Hash & Display & Eq

f
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),LowerIdent(1:5-1:6),OpArrow(1:7-1:9),LowerIdent(1:10-1:11),KwWhere(1:12-1:17),LowerIdent(1:18-1:19),KwImplements(1:20-1:30),UpperIdent(1:31-1:35),OpAmpersand(1:36-1:37),UpperIdent(1:38-1:40),Comma(1:40-1:41),LowerIdent(1:42-1:43),KwImplements(1:44-1:54),UpperIdent(1:55-1:57),OpAmpersand(1:58-1:59),UpperIdent(1:60-1:64),OpAmpersand(1:65-1:66),UpperIdent(1:67-1:74),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),LowerIdent(3:5-3:6),OpArrow(3:7-3:9),LowerIdent(3:10-3:11),Newline(1:1-1:1),
KwWhere(4:3-4:8),LowerIdent(4:9-4:10),KwImplements(4:11-4:21),UpperIdent(4:22-4:26),OpAmpersand(4:27-4:28),UpperIdent(4:29-4:31),Comma(4:31-4:32),Newline(1:1-1:1),
LowerIdent(5:5-5:6),KwImplements(5:7-5:17),UpperIdent(5:18-5:22),OpAmpersand(5:23-5:24),UpperIdent(5:25-5:32),OpAmpersand(5:33-5:34),UpperIdent(5:35-5:37),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(7:1-7:2),EndOfFile(7:2-7:2),
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
