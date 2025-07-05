# META
~~~ini
description=parens_record_updater
type=expr
~~~
# SOURCE
~~~roc
T
&n
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),Newline(1:1-1:1),
OpAmpersand(2:1-2:2),LowerIdent(2:2-2:3),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "T"))
~~~
# FORMATTED
~~~roc
T
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "T"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[T]*"))
~~~
