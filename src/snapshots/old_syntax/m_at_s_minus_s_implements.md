# META
~~~ini
description=m_at_s_minus_s_implements fail
type=expr
~~~
# SOURCE
~~~roc
M@S -S implements
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpaqueName(1:2-1:4),OpUnaryMinus(1:5-1:6),UpperIdent(1:6-1:7),KwImplements(1:8-1:18),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
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
(e-tag @1.1-1.2 (name "M"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[M]*"))
~~~
