# META
~~~ini
description=Test evaluation of nested if expressions
type=expr
~~~
# SOURCE
~~~roc
if 5 > 3 (if 1 > 2 3 else 4) else 5
~~~
# EXPECTED
~~~
4
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwIf(1:1-1:3),Int(1:4-1:5),OpGreaterThan(1:6-1:7),Int(1:8-1:9),OpenRound(1:10-1:11),KwIf(1:11-1:13),Int(1:14-1:15),OpGreaterThan(1:16-1:17),Int(1:18-1:19),Int(1:20-1:21),KwElse(1:22-1:26),Int(1:27-1:28),CloseRound(1:28-1:29),KwElse(1:30-1:34),Int(1:35-1:36),EndOfFile(1:36-1:36),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-1.36
	(e-binop @1.4-1.9 (op ">")
		(e-int @1.4-1.5 (raw "5"))
		(e-int @1.8-1.9 (raw "3")))
	(e-tuple @1.10-1.29
		(e-if-then-else @1.11-1.28
			(e-binop @1.14-1.19 (op ">")
				(e-int @1.14-1.15 (raw "1"))
				(e-int @1.18-1.19 (raw "2")))
			(e-int @1.20-1.21 (raw "3"))
			(e-int @1.27-1.28 (raw "4"))))
	(e-int @1.35-1.36 (raw "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-1.36
	(if-branches
		(if-branch
			(e-binop @1.4-1.9 (op "gt")
				(e-int @1.4-1.5 (value "5"))
				(e-int @1.8-1.9 (value "3")))
			(e-if @1.11-1.28
				(if-branches
					(if-branch
						(e-binop @1.14-1.19 (op "gt")
							(e-int @1.14-1.15 (value "1"))
							(e-int @1.18-1.19 (value "2")))
						(e-int @1.20-1.21 (value "3"))))
				(if-else
					(e-int @1.27-1.28 (value "4"))))))
	(if-else
		(e-int @1.35-1.36 (value "5"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.36 (type "Num(_size)"))
~~~
