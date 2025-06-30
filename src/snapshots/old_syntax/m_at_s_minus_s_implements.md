# META
~~~ini
description=m_at_s_minus_s_implements fail
type=expr
~~~
# SOURCE
~~~roc
M@S -S implements
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpaqueName(1:2-1:4),OpUnaryMinus(1:5-1:6),UpperIdent(1:6-1:7),KwImplements(1:8-1:18),EndOfFile(1:18-1:18),
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
