# META
~~~ini
description=ann_extra_indented_implements
type=expr
~~~
# SOURCE
~~~roc
2 :
    r where e
    implements
    P
u
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:3-1:4),Newline(1:1-1:1),
LowerIdent(2:5-2:6),KwWhere(2:7-2:12),LowerIdent(2:13-2:14),Newline(1:1-1:1),
KwImplements(3:5-3:15),Newline(1:1-1:1),
UpperIdent(4:5-4:6),Newline(1:1-1:1),
LowerIdent(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "2"))
~~~
# FORMATTED
~~~roc
2
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.2 (value "2"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(*)"))
~~~
