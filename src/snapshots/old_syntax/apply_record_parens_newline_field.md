# META
~~~ini
description=apply_record_parens_newline_field
type=expr
~~~
# SOURCE
~~~roc
0{l,xt,l:(se#
)}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),Comma(1:4-1:5),LowerIdent(1:5-1:7),Comma(1:7-1:8),LowerIdent(1:8-1:9),OpColon(1:9-1:10),NoSpaceOpenRound(1:10-1:11),LowerIdent(1:11-1:13),Newline(1:14-1:14),
CloseRound(2:1-2:2),CloseCurly(2:2-2:3),EndOfFile(2:3-2:3),
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
