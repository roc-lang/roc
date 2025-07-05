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
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),UpperIdent(1:3-1:4),Newline(1:1-1:1),
OpenCurly(2:2-2:3),Newline(1:1-1:1),
CloseCurly(3:1-3:2),Newline(1:1-1:1),
UpperIdent(4:1-4:2),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
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
(expr @1.1-1.2 (type "Num(*)"))
~~~
