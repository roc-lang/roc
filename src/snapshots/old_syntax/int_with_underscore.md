# META
~~~ini
description=int_with_underscore
type=expr
~~~
# SOURCE
~~~roc
1__23
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:6),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.6 (raw "1__23"))
~~~
# FORMATTED
~~~roc
1__23
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.6 (value "123"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "Num(*)"))
~~~
