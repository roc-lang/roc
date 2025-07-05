# META
~~~ini
description=when_with_tuples
type=expr
~~~
# SOURCE
~~~roc
when (1, 2) is
 (1, x) -> x
 (_, b) -> 3 + b
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpenRound(1:6-1:7),Int(1:7-1:8),Comma(1:8-1:9),Int(1:10-1:11),CloseRound(1:11-1:12),LowerIdent(1:13-1:15),Newline(1:1-1:1),
OpenRound(2:2-2:3),Int(2:3-2:4),Comma(2:4-2:5),LowerIdent(2:6-2:7),CloseRound(2:7-2:8),OpArrow(2:9-2:11),LowerIdent(2:12-2:13),Newline(1:1-1:1),
OpenRound(3:2-3:3),Underscore(3:3-3:4),Comma(3:4-3:5),LowerIdent(3:6-3:7),CloseRound(3:7-3:8),OpArrow(3:9-3:11),Int(3:12-3:13),OpPlus(3:14-3:15),LowerIdent(3:16-3:17),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
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
