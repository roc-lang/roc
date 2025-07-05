# META
~~~ini
description=annotated_empty_record_destructure
type=expr
~~~
# SOURCE
~~~roc
E:B
{}=B
B
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),UpperIdent(1:3-1:4),Newline(1:1-1:1),
OpenCurly(2:1-2:2),CloseCurly(2:2-2:3),OpAssign(2:3-2:4),UpperIdent(2:4-2:5),Newline(1:1-1:1),
UpperIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "E"))
~~~
# FORMATTED
~~~roc
E
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "E"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[E]*"))
~~~
