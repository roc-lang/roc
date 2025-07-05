# META
~~~ini
description=not_record_updater
type=expr
~~~
# SOURCE
~~~roc
e
!
&s
~~~
# EXPECTED
UNDEFINED VARIABLE - not_record_updater.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),Newline(1:1-1:1),
OpBang(2:1-2:2),Newline(1:1-1:1),
OpAmpersand(3:1-3:2),LowerIdent(3:2-3:3),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "e"))
~~~
# FORMATTED
~~~roc
e
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
