# META
~~~ini
description=quotes_in_parens_in_pat malformed
type=expr
~~~
# SOURCE
~~~roc
Q (""""""""):a
q
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenRound(1:3-1:4),MultilineStringStart(1:4-1:7),StringPart(1:7-1:7),MultilineStringEnd(1:7-1:10),StringStart(1:10-1:11),StringPart(1:11-1:11),StringEnd(1:11-1:12),CloseRound(1:12-1:13),OpColon(1:13-1:14),LowerIdent(1:14-1:15),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "Q"))
~~~
# FORMATTED
~~~roc
Q
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "Q"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[Q]*"))
~~~
