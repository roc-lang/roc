# META
~~~ini
description=ann_pattern_comment_before_body
type=expr
~~~
# SOURCE
~~~roc
H:p
(#
s)=p
d#
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),LowerIdent(1:3-1:4),Newline(1:1-1:1),
OpenRound(2:1-2:2),Newline(2:3-2:3),
LowerIdent(3:1-3:2),CloseRound(3:2-3:3),OpAssign(3:3-3:4),LowerIdent(3:4-3:5),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:3-4:3),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "H"))
~~~
# FORMATTED
~~~roc
H
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (ext-var 73) (name "H") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[H]*"))
~~~
