# META
~~~ini
description=if_in_record_field_opt_pat malformed
type=expr
~~~
# SOURCE
~~~roc
O{p?if
a
then
A
else&m}#
:e
i
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),NoSpaceOpQuestion(1:4-1:5),KwIf(1:5-1:7),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
LowerIdent(3:1-3:5),Newline(1:1-1:1),
UpperIdent(4:1-4:2),Newline(1:1-1:1),
KwElse(5:1-5:5),OpAmpersand(5:5-5:6),LowerIdent(5:6-5:7),CloseCurly(5:7-5:8),Newline(5:9-5:9),
OpColon(6:1-6:2),LowerIdent(6:2-6:3),Newline(1:1-1:1),
LowerIdent(7:1-7:2),EndOfFile(7:2-7:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "O"))
~~~
# FORMATTED
~~~roc
O
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "O"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[O]*"))
~~~
