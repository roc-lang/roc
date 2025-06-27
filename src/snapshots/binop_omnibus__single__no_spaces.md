# META
~~~ini
description=Binop omnibus - singleline - no spaces
type=expr
~~~
# SOURCE
~~~roc
Err(foo)??12>5*5 or 13+2<5 and 10-1>=16 or 12<=3/5
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
UpperIdent(1:1-1:4),NoSpaceOpenRound(1:4-1:5),LowerIdent(1:5-1:8),CloseRound(1:8-1:9),OpDoubleQuestion(1:9-1:11),Int(1:11-1:13),OpGreaterThan(1:13-1:14),Int(1:14-1:15),OpStar(1:15-1:16),Int(1:16-1:17),OpOr(1:18-1:20),Int(1:21-1:23),OpPlus(1:23-1:24),Int(1:24-1:25),OpLessThan(1:25-1:26),Int(1:26-1:27),OpAnd(1:28-1:31),Int(1:32-1:34),OpBinaryMinus(1:34-1:35),Int(1:35-1:36),OpGreaterThanOrEq(1:36-1:38),Int(1:38-1:40),OpOr(1:41-1:43),Int(1:44-1:46),OpLessThanOrEq(1:46-1:48),Int(1:48-1:49),OpSlash(1:49-1:50),Int(1:50-1:51),EndOfFile(1:51-1:51),
~~~
# PARSE
~~~clojure
(e-binop @1-1-1-51 (op "or")
	(e-binop @1-1-1-43 (op "or")
		(e-binop @1-1-1-20 (op ">")
			(e-binop @1-1-1-14 (op "??")
				(e-apply @1-1-1-9
					(e-tag @1-1-1-4 (raw "Err"))
					(e-ident @1-5-1-8 (qaul "") (raw "foo")))
				(e-int @1-11-1-13 (raw "12")))
			(e-binop @1-14-1-20 (op "*")
				(e-int @1-14-1-15 (raw "5"))
				(e-int @1-16-1-17 (raw "5"))))
		(e-binop @1-21-1-43 (op "and")
			(e-binop @1-21-1-31 (op "<")
				(e-binop @1-21-1-26 (op "+")
					(e-int @1-21-1-23 (raw "13"))
					(e-int @1-24-1-25 (raw "2")))
				(e-int @1-26-1-27 (raw "5")))
			(e-binop @1-32-1-43 (op ">=")
				(e-binop @1-32-1-38 (op "-")
					(e-int @1-32-1-34 (raw "10"))
					(e-int @1-35-1-36 (raw "1")))
				(e-int @1-38-1-40 (raw "16")))))
	(e-binop @1-44-1-51 (op "<=")
		(e-int @1-44-1-46 (raw "12"))
		(e-binop @1-48-1-51 (op "/")
			(e-int @1-48-1-49 (raw "3"))
			(e-int @1-50-1-51 (raw "5")))))
~~~
# FORMATTED
~~~roc
Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
~~~
# CANONICALIZE
~~~clojure
(e-binop @1-1-1-51 (op "or") (id 112)
	(e-binop @1-1-1-43 (op "or")
		(e-binop @1-1-1-20 (op "gt")
			(e-binop @1-1-1-14 (op "null_coalesce")
				(e-call @1-1-1-9
					(e-tag @1-1-1-4 (ext-var 0) (name "Err") (args "TODO"))
					(e-runtime-error (tag "ident_not_in_scope")))
				(e-int @1-11-1-13 (num-var 78) (value "12")))
			(e-binop @1-14-1-20 (op "mul")
				(e-int @1-14-1-15 (num-var 81) (value "5"))
				(e-int @1-16-1-17 (num-var 83) (value "5"))))
		(e-binop @1-21-1-43 (op "and")
			(e-binop @1-21-1-31 (op "lt")
				(e-binop @1-21-1-26 (op "add")
					(e-int @1-21-1-23 (num-var 87) (value "13"))
					(e-int @1-24-1-25 (num-var 89) (value "2")))
				(e-int @1-26-1-27 (num-var 92) (value "5")))
			(e-binop @1-32-1-43 (op "ge")
				(e-binop @1-32-1-38 (op "sub")
					(e-int @1-32-1-34 (num-var 95) (value "10"))
					(e-int @1-35-1-36 (num-var 97) (value "1")))
				(e-int @1-38-1-40 (num-var 100) (value "16")))))
	(e-binop @1-44-1-51 (op "le")
		(e-int @1-44-1-46 (num-var 105) (value "12"))
		(e-binop @1-48-1-51 (op "div")
			(e-int @1-48-1-49 (num-var 107) (value "3"))
			(e-int @1-50-1-51 (num-var 109) (value "5")))))
~~~
# TYPES
~~~clojure
(expr (id 112) (type "*"))
~~~