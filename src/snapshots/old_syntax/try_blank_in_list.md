# META
~~~ini
description=try_blank_in_list
type=expr
~~~
# SOURCE
~~~roc
L[try#[then2[#
]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenSquare(1:2-1:3),LowerIdent(1:3-1:6),Newline(1:7-1:15),
CloseSquare(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "L"))
~~~
# FORMATTED
~~~roc
L
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "L"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[L]*"))
~~~
