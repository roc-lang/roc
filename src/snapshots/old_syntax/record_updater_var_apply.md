# META
~~~ini
description=record_updater_var_apply
type=expr
~~~
# SOURCE
~~~roc
foo&bar  5
~~~
# EXPECTED
UNDEFINED VARIABLE - record_updater_var_apply.md:1:1:1:4
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpAmpersand(1:4-1:5),LowerIdent(1:5-1:8),Int(1:10-1:11),EndOfFile(1:11-1:11),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.4 (raw "foo"))
~~~
# FORMATTED
~~~roc
foo
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Error"))
~~~
