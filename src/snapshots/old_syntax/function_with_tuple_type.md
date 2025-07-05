# META
~~~ini
description=function_with_tuple_type
type=expr
~~~
# SOURCE
~~~roc
f : I64 -> (I64, I64)
f = \x -> (x, x + 1)

f 42
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),UpperIdent(1:5-1:8),OpArrow(1:9-1:11),OpenRound(1:12-1:13),UpperIdent(1:13-1:16),Comma(1:16-1:17),UpperIdent(1:18-1:21),CloseRound(1:21-1:22),Newline(1:1-1:1),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),OpBackslash(2:5-2:6),LowerIdent(2:6-2:7),OpArrow(2:8-2:10),OpenRound(2:11-2:12),LowerIdent(2:12-2:13),Comma(2:13-2:14),LowerIdent(2:15-2:16),OpPlus(2:17-2:18),Int(2:19-2:20),CloseRound(2:20-2:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:2),Int(4:3-4:5),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
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
