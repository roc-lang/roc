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
This feature is not yet implemented: canonicalize dbg expression

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
(e-runtime-error (tag "not_implemented") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
