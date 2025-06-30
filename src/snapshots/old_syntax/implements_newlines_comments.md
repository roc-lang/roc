# META
~~~ini
description=implements_newlines_comments
type=expr
~~~
# SOURCE
~~~roc
M#
 im#
 implements
 de:J 
 
s
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),Newline(1:3-1:3),
LowerIdent(2:2-2:4),Newline(2:5-2:5),
KwImplements(3:2-3:12),Newline(1:1-1:1),
LowerIdent(4:2-4:4),OpColon(4:4-4:5),UpperIdent(4:5-4:6),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "M"))
~~~
# FORMATTED
~~~roc
M
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (ext-var 73) (name "M") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[M]*"))
~~~
