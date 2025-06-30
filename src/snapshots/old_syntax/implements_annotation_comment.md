# META
~~~ini
description=implements_annotation_comment
type=expr
~~~
# SOURCE
~~~roc
S implements i:(a#
#
)
0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),KwImplements(1:3-1:13),LowerIdent(1:14-1:15),OpColon(1:15-1:16),NoSpaceOpenRound(1:16-1:17),LowerIdent(1:17-1:18),Newline(1:19-1:19),
Newline(2:2-2:2),
CloseRound(3:1-3:2),Newline(1:1-1:1),
Int(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "S"))
~~~
# FORMATTED
~~~roc
S
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "S") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[S]*"))
~~~
