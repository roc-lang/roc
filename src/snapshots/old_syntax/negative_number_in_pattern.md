# META
~~~ini
description=negative_number_in_pattern malformed
type=expr
~~~
# SOURCE
~~~roc
N -0 T:A
zT
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),Int(1:3-1:5),UpperIdent(1:6-1:7),OpColon(1:7-1:8),UpperIdent(1:8-1:9),Newline(1:1-1:1),
LowerIdent(2:1-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "N"))
~~~
# FORMATTED
~~~roc
N
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (ext-var 73) (name "N") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[N]*"))
~~~
