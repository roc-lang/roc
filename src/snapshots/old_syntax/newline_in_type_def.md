# META
~~~ini
description=newline_in_type_def
type=expr
~~~
# SOURCE
~~~roc
R
:D
a
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),Newline(1:1-1:1),
OpColon(2:1-2:2),UpperIdent(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "R"))
~~~
# FORMATTED
~~~roc
R
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "R"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[R]*"))
~~~
