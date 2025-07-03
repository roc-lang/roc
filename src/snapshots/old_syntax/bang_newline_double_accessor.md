# META
~~~ini
description=bang_newline_double_accessor
type=expr
~~~
# SOURCE
~~~roc
0
!
.d
.d
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),Newline(1:1-1:1),
OpBang(2:1-2:2),Newline(1:1-1:1),
DotLowerIdent(3:1-3:3),Newline(1:1-1:1),
DotLowerIdent(4:1-4:3),EndOfFile(4:3-4:3),
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
(expr @1.1-1.2 (type "Num(a)"))
~~~
