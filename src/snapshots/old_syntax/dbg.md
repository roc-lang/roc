# META
~~~ini
description=dbg
type=expr
~~~
# SOURCE
~~~roc
dbg 1
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize dbg expression
Let us know if you want to help!

# TOKENS
~~~zig
KwDbg(1:1-1:4),Int(1:5-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-int @1.5-1.6 (raw "1")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
