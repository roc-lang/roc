# META
~~~ini
description=if_def
type=expr
~~~
# SOURCE
~~~roc
iffy=5

42
~~~
# EXPECTED
UNDEFINED VARIABLE - if_def.md:1:1:1:5
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpAssign(1:5-1:6),Int(1:6-1:7),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(3:1-3:3),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "iffy"))
~~~
# FORMATTED
~~~roc
iffy
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
