# META
~~~ini
description=h_parens_as_parens_h_ann
type=expr
~~~
# SOURCE
~~~roc
N:A((H)as H) H
I
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),UpperIdent(1:3-1:4),NoSpaceOpenRound(1:4-1:5),NoSpaceOpenRound(1:5-1:6),UpperIdent(1:6-1:7),CloseRound(1:7-1:8),KwAs(1:8-1:10),UpperIdent(1:11-1:12),CloseRound(1:12-1:13),UpperIdent(1:14-1:15),Newline(1:1-1:1),
UpperIdent(2:1-2:2),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
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
