# META
~~~ini
description=dbg_double_newline
type=expr
~~~
# SOURCE
~~~roc
dbg dbg
 a g
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize dbg expression
Let us know if you want to help!

# TOKENS
~~~zig
KwDbg(1:1-1:4),KwDbg(1:5-1:8),Newline(1:1-1:1),
LowerIdent(2:2-2:3),LowerIdent(2:4-2:5),EndOfFile(2:5-2:5),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-dbg
		(e-ident @2.2-2.3 (qaul "") (raw "a"))))
~~~
# FORMATTED
~~~roc
dbg dbg
	a
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
