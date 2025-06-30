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
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),Newline(1:1-1:1),
OpAmpersand(2:1-2:2),LowerIdent(2:2-2:3),EndOfFile(2:3-2:3),
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
(e-tag @1.1-1.2 (ext-var 73) (name "T") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[T]*"))
~~~
