# META
~~~ini
description=Simple tag literal
type=expr
~~~
# SOURCE
~~~roc
MyTag
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:6),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.6 (raw "MyTag"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.6 (name "MyTag"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "[MyTag]_others"))
~~~
