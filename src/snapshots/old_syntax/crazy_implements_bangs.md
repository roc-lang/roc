# META
~~~ini
description=crazy_implements_bangs
type=expr
~~~
# SOURCE
~~~roc
P:=p#
  implements[]
n
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColonEqual(1:2-1:4),LowerIdent(1:4-1:5),Newline(1:6-1:6),
KwImplements(2:3-2:13),OpenSquare(2:13-2:14),CloseSquare(2:14-2:15),Newline(1:1-1:1),
LowerIdent(3:1-3:2),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "P"))
~~~
# FORMATTED
~~~roc
P
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "P"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[P]*"))
~~~
