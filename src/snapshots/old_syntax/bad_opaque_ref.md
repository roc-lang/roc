# META
~~~ini
description=bad_opaque_ref malformed
type=expr
~~~
# SOURCE
~~~roc
I@
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),MalformedOpaqueNameWithoutName(1:2-1:3),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
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
(e-tag @1.1-1.2 (name "I"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[I]*"))
~~~
