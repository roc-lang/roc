# META
~~~ini
description=tuple_type_ext
type=expr
~~~
# SOURCE
~~~roc
f: (Str, Str)a -> (Str, Str)a
f = \x -> x

f (1, 2)
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),OpenRound(1:4-1:5),UpperIdent(1:5-1:8),Comma(1:8-1:9),UpperIdent(1:10-1:13),CloseRound(1:13-1:14),LowerIdent(1:14-1:15),OpArrow(1:16-1:18),OpenRound(1:19-1:20),UpperIdent(1:20-1:23),Comma(1:23-1:24),UpperIdent(1:25-1:28),CloseRound(1:28-1:29),LowerIdent(1:29-1:30),Newline(1:1-1:1),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),OpBackslash(2:5-2:6),LowerIdent(2:6-2:7),OpArrow(2:8-2:10),LowerIdent(2:11-2:12),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpenRound(4:3-4:4),Int(4:4-4:5),Comma(4:5-4:6),Int(4:7-4:8),CloseRound(4:8-4:9),EndOfFile(4:9-4:9),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "f"))
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
