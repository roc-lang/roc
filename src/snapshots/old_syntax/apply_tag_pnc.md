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
(e-call @1.1-1.13 (id 78)
	(e-tag @1.1-1.5 (ext-var 73) (name "Whee") (args "TODO"))
	(e-int @1.6-1.8 (value "12"))
	(e-int @1.10-1.12 (value "34")))
~~~
# TYPES
~~~clojure
(expr (id 78) (type "*"))
~~~
