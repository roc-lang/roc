# META
~~~ini
description=opt_field_newline_in_ty
type=expr
~~~
# SOURCE
~~~roc
0:{i
?d}
O
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),OpenCurly(1:3-1:4),LowerIdent(1:4-1:5),Newline(1:1-1:1),
OpQuestion(2:1-2:2),LowerIdent(2:2-2:3),CloseCurly(2:3-2:4),Newline(1:1-1:1),
UpperIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "0"))
~~~
# FORMATTED
~~~roc
0
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.2 (value "0"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(*)"))
~~~
