# META
~~~ini
description=Debug expression stmt
type=expr
~~~
# SOURCE
~~~roc
dbg x
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize dbg expression

# TOKENS
~~~zig
KwDbg(1:1-1:4),LowerIdent(1:5-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-ident @1-5-1-6 (qaul "") (raw "x")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Error"))
~~~
