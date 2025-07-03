# META
~~~ini
description=ann_apply_record_with_newlines
type=expr
~~~
# SOURCE
~~~roc
8:O
 {
}
Q
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),UpperIdent(1:3-1:4),Newline(1:1-1:1),
OpenCurly(2:2-2:3),Newline(1:1-1:1),
CloseCurly(3:1-3:2),Newline(1:1-1:1),
UpperIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "8"))
~~~
# FORMATTED
~~~roc
8
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.2 (value "8"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(a)"))
~~~
