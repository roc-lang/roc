# META
~~~ini
description=Fibonacci fn
type=snippet
~~~
# SOURCE
~~~roc
fib = |n| if n <= 1 n else fib(n - 1) + fib(n - 2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpAssign(1:5-1:6),OpBar(1:7-1:8),LowerIdent(1:8-1:9),OpBar(1:9-1:10),KwIf(1:11-1:13),LowerIdent(1:14-1:15),OpLessThanOrEq(1:16-1:18),Int(1:19-1:20),LowerIdent(1:21-1:22),KwElse(1:23-1:27),LowerIdent(1:28-1:31),NoSpaceOpenRound(1:31-1:32),LowerIdent(1:32-1:33),OpBinaryMinus(1:34-1:35),Int(1:36-1:37),CloseRound(1:37-1:38),OpPlus(1:39-1:40),LowerIdent(1:41-1:44),NoSpaceOpenRound(1:44-1:45),LowerIdent(1:45-1:46),OpBinaryMinus(1:47-1:48),Int(1:49-1:50),CloseRound(1:50-1:51),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.51
	(type-module @1.1-1.4)
	(statements
		(s-decl @1.1-1.51
			(p-ident @1.1-1.4 (raw "fib"))
			(e-lambda @1.7-1.51
				(args
					(p-ident @1.8-1.9 (raw "n")))
				(e-if-then-else @1.11-1.51
					(e-binop @1.14-1.20 (op "<=")
						(e-ident @1.14-1.15 (raw "n"))
						(e-int @1.19-1.20 (raw "1")))
					(e-ident @1.21-1.22 (raw "n"))
					(e-binop @1.28-1.51 (op "+")
						(e-apply @1.28-1.38
							(e-ident @1.28-1.31 (raw "fib"))
							(e-binop @1.32-1.37 (op "-")
								(e-ident @1.32-1.33 (raw "n"))
								(e-int @1.36-1.37 (raw "1"))))
						(e-apply @1.41-1.51
							(e-ident @1.41-1.44 (raw "fib"))
							(e-binop @1.45-1.50 (op "-")
								(e-ident @1.45-1.46 (raw "n"))
								(e-int @1.49-1.50 (raw "2"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.4 (ident "fib"))
		(e-closure @1.7-1.51
			(captures
				(capture @1.1-1.4 (ident "fib")))
			(e-lambda @1.7-1.51
				(args
					(p-assign @1.8-1.9 (ident "n")))
				(e-if @1.11-1.51
					(if-branches
						(if-branch
							(e-binop @1.14-1.20 (op "le")
								(e-lookup-local @1.14-1.15
									(p-assign @1.8-1.9 (ident "n")))
								(e-num @1.19-1.20 (value "1")))
							(e-lookup-local @1.21-1.22
								(p-assign @1.8-1.9 (ident "n")))))
					(if-else
						(e-binop @1.28-1.51 (op "add")
							(e-call @1.28-1.38
								(e-lookup-local @1.28-1.31
									(p-assign @1.1-1.4 (ident "fib")))
								(e-binop @1.32-1.37 (op "sub")
									(e-lookup-local @1.32-1.33
										(p-assign @1.8-1.9 (ident "n")))
									(e-num @1.36-1.37 (value "1"))))
							(e-call @1.41-1.51
								(e-lookup-local @1.41-1.44
									(p-assign @1.1-1.4 (ident "fib")))
								(e-binop @1.45-1.50 (op "sub")
									(e-lookup-local @1.45-1.46
										(p-assign @1.8-1.9 (ident "n")))
									(e-num @1.49-1.50 (value "2")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.4 (type "Num(_size) -> Num(_size2)")))
	(expressions
		(expr @1.7-1.51 (type "Num(_size) -> Num(_size2)"))))
~~~
