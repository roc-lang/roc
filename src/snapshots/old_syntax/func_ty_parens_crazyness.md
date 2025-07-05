# META
~~~ini
description=func_ty_parens_crazyness
type=expr
~~~
# SOURCE
~~~roc
N:(((f->(((
u)))I->*)))
I
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),NoSpaceOpenRound(1:4-1:5),NoSpaceOpenRound(1:5-1:6),LowerIdent(1:6-1:7),OpArrow(1:7-1:9),NoSpaceOpenRound(1:9-1:10),NoSpaceOpenRound(1:10-1:11),NoSpaceOpenRound(1:11-1:12),Newline(1:1-1:1),
LowerIdent(2:1-2:2),CloseRound(2:2-2:3),CloseRound(2:3-2:4),CloseRound(2:4-2:5),UpperIdent(2:5-2:6),OpArrow(2:6-2:8),OpStar(2:8-2:9),CloseRound(2:9-2:10),CloseRound(2:10-2:11),CloseRound(2:11-2:12),Newline(1:1-1:1),
UpperIdent(3:1-3:2),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "N"))
~~~
# FORMATTED
~~~roc
N
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "N"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[N]*"))
~~~
