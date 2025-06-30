# META
~~~ini
description=bad_opaque_ref malformed
type=expr
~~~
# SOURCE
~~~roc
I@
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),MalformedOpaqueNameWithoutName(1:2-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "I"))
~~~
# FORMATTED
~~~roc
I
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "I") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[I]*"))
~~~
