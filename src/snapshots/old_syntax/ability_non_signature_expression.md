# META
~~~ini
description=ability_non_signature_expression fail
type=expr
~~~
# SOURCE
~~~roc
MEq implements
    123

1
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),KwImplements(1:5-1:15),Newline(1:1-1:1),
Int(2:5-2:8),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(4:1-4:2),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.4 (raw "MEq"))
~~~
# FORMATTED
~~~roc
MEq
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.4 (name "MEq"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "[MEq]*"))
~~~
