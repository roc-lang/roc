# META
~~~ini
description=If expression with numeric comparison
type=expr
~~~
# SOURCE
~~~roc
if 5 > 3 1 else 2
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwIf(1:1-1:3),Int(1:4-1:5),OpGreaterThan(1:6-1:7),Int(1:8-1:9),Int(1:10-1:11),KwElse(1:12-1:16),Int(1:17-1:18),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-1.18
	(e-binop @1.4-1.9 (op ">")
		(e-int @1.4-1.5 (raw "5"))
		(e-int @1.8-1.9 (raw "3")))
	(e-int @1.10-1.11 (raw "1"))
	(e-int @1.17-1.18 (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-1.18
	(if-branches
		(if-branch
			(e-binop @1.4-1.9 (op "gt")
				(e-int @1.4-1.5 (value "5"))
				(e-int @1.8-1.9 (value "3")))
			(e-int @1.10-1.11 (value "1"))))
	(if-else
		(e-int @1.17-1.18 (value "2"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.18 (type "Num(_size)"))
~~~
