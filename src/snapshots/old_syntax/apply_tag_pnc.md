# META
~~~ini
description=apply_tag_pnc
type=expr
~~~
# SOURCE
~~~roc
Whee(12, 34)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:5),NoSpaceOpenRound(1:5-1:6),Int(1:6-1:8),Comma(1:8-1:9),Int(1:10-1:12),CloseRound(1:12-1:13),EndOfFile(1:13-1:13),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.13
	(e-tag @1.1-1.5 (raw "Whee"))
	(e-int @1.6-1.8 (raw "12"))
	(e-int @1.10-1.12 (raw "34")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.13 (name "Whee") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.13 (type "a"))
~~~
