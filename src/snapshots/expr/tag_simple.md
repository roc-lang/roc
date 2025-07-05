# META
~~~ini
description=Simple tag literal
type=expr
~~~
# SOURCE
~~~roc
Ok
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:3),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.3 (raw "Ok"))
~~~
# FORMATTED
~~~roc
Ok
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.3 (name "Ok"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "[Ok]*"))
~~~
