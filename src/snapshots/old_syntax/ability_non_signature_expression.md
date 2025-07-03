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
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),KwImplements(1:5-1:15),Newline(1:1-1:1),
Int(2:5-2:8),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(4:1-4:2),EndOfFile(4:2-4:2),
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
(e-tag @1.1-1.4 (name "MEq") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "[MEq]a"))
~~~
