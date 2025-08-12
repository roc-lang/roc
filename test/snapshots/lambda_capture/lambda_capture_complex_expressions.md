# META
~~~ini
description=Complex expressions with captures - lambda with conditionals and captures
type=expr
~~~
# SOURCE
~~~roc
(|outer| |inner| if outer > 0 (outer + inner) else inner)(1)(-2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),LowerIdent(1:3-1:8),OpBar(1:8-1:9),OpBar(1:10-1:11),LowerIdent(1:11-1:16),OpBar(1:16-1:17),KwIf(1:18-1:20),LowerIdent(1:21-1:26),OpGreaterThan(1:27-1:28),Int(1:29-1:30),OpenRound(1:31-1:32),LowerIdent(1:32-1:37),OpPlus(1:38-1:39),LowerIdent(1:40-1:45),CloseRound(1:45-1:46),KwElse(1:47-1:51),LowerIdent(1:52-1:57),CloseRound(1:57-1:58),NoSpaceOpenRound(1:58-1:59),Int(1:59-1:60),CloseRound(1:60-1:61),NoSpaceOpenRound(1:61-1:62),Int(1:62-1:64),CloseRound(1:64-1:65),EndOfFile(1:65-1:65),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.65
	(e-apply @1.1-1.61
		(e-tuple @1.1-1.58
			(e-lambda @1.2-1.57
				(args
					(p-ident @1.3-1.8 (raw "outer")))
				(e-lambda @1.10-1.57
					(args
						(p-ident @1.11-1.16 (raw "inner")))
					(e-if-then-else @1.18-1.57
						(e-binop @1.21-1.30 (op ">")
							(e-ident @1.21-1.26 (raw "outer"))
							(e-int @1.29-1.30 (raw "0")))
						(e-tuple @1.31-1.46
							(e-binop @1.32-1.45 (op "+")
								(e-ident @1.32-1.37 (raw "outer"))
								(e-ident @1.40-1.45 (raw "inner"))))
						(e-ident @1.52-1.57 (raw "inner"))))))
		(e-int @1.59-1.60 (raw "1")))
	(e-int @1.62-1.64 (raw "-2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.65
	(e-call @1.1-1.61
		(e-lambda @1.2-1.57
			(args
				(p-assign @1.3-1.8 (ident "outer")))
			(e-closure @1.10-1.57
				(captures
					(capture @1.3-1.8 (ident "outer")))
				(e-lambda @1.10-1.57
					(args
						(p-assign @1.11-1.16 (ident "inner")))
					(e-if @1.18-1.57
						(if-branches
							(if-branch
								(e-binop @1.21-1.30 (op "gt")
									(e-lookup-local @1.21-1.26
										(p-assign @1.3-1.8 (ident "outer")))
									(e-int @1.29-1.30 (value "0")))
								(e-binop @1.32-1.45 (op "add")
									(e-lookup-local @1.32-1.37
										(p-assign @1.3-1.8 (ident "outer")))
									(e-lookup-local @1.40-1.45
										(p-assign @1.11-1.16 (ident "inner"))))))
						(if-else
							(e-lookup-local @1.52-1.57
								(p-assign @1.11-1.16 (ident "inner"))))))))
		(e-int @1.59-1.60 (value "1")))
	(e-int @1.62-1.64 (value "-2")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.65 (type "Num(_size)"))
~~~
