# META
~~~ini
description=Test evaluation of 3-level deep nested if expressions
type=expr
~~~
# SOURCE
~~~roc
if 10 > 5 (if 4 < 8 (if 2 == 2 100 else 200) else 300) else 400
~~~
# EXPECTED
~~~
100
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwIf(1:1-1:3),Int(1:4-1:6),OpGreaterThan(1:7-1:8),Int(1:9-1:10),OpenRound(1:11-1:12),KwIf(1:12-1:14),Int(1:15-1:16),OpLessThan(1:17-1:18),Int(1:19-1:20),OpenRound(1:21-1:22),KwIf(1:22-1:24),Int(1:25-1:26),OpEquals(1:27-1:29),Int(1:30-1:31),Int(1:32-1:35),KwElse(1:36-1:40),Int(1:41-1:44),CloseRound(1:44-1:45),KwElse(1:46-1:50),Int(1:51-1:54),CloseRound(1:54-1:55),KwElse(1:56-1:60),Int(1:61-1:64),EndOfFile(1:64-1:64),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-1.64
	(e-binop @1.4-1.10 (op ">")
		(e-int @1.4-1.6 (raw "10"))
		(e-int @1.9-1.10 (raw "5")))
	(e-tuple @1.11-1.55
		(e-if-then-else @1.12-1.54
			(e-binop @1.15-1.20 (op "<")
				(e-int @1.15-1.16 (raw "4"))
				(e-int @1.19-1.20 (raw "8")))
			(e-tuple @1.21-1.45
				(e-if-then-else @1.22-1.44
					(e-binop @1.25-1.31 (op "==")
						(e-int @1.25-1.26 (raw "2"))
						(e-int @1.30-1.31 (raw "2")))
					(e-int @1.32-1.35 (raw "100"))
					(e-int @1.41-1.44 (raw "200"))))
			(e-int @1.51-1.54 (raw "300"))))
	(e-int @1.61-1.64 (raw "400")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-1.64
	(if-branches
		(if-branch
			(e-binop @1.4-1.10 (op "gt")
				(e-int @1.4-1.6 (value "10"))
				(e-int @1.9-1.10 (value "5")))
			(e-if @1.12-1.54
				(if-branches
					(if-branch
						(e-binop @1.15-1.20 (op "lt")
							(e-int @1.15-1.16 (value "4"))
							(e-int @1.19-1.20 (value "8")))
						(e-if @1.22-1.44
							(if-branches
								(if-branch
									(e-binop @1.25-1.31 (op "eq")
										(e-int @1.25-1.26 (value "2"))
										(e-int @1.30-1.31 (value "2")))
									(e-int @1.32-1.35 (value "100"))))
							(if-else
								(e-int @1.41-1.44 (value "200"))))))
				(if-else
					(e-int @1.51-1.54 (value "300"))))))
	(if-else
		(e-int @1.61-1.64 (value "400"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.64 (type "Num(_size)"))
~~~
