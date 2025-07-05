# META
~~~ini
description=alias_parens_comment
type=expr
~~~
# SOURCE
~~~roc
K:(#
s)
K
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Newline(1:5-1:5),
LowerIdent(2:1-2:2),CloseRound(2:2-2:3),Newline(1:1-1:1),
UpperIdent(3:1-3:2),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "K"))
~~~
# FORMATTED
~~~roc
K
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "K"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[K]*"))
~~~
