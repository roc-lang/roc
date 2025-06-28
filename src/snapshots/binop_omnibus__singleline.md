# META
~~~ini
description=Binop omnibus - singleline
type=expr
~~~
# SOURCE
~~~roc
Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
UpperIdent(1:1-1:4),NoSpaceOpenRound(1:4-1:5),LowerIdent(1:5-1:8),CloseRound(1:8-1:9),OpDoubleQuestion(1:10-1:12),Int(1:13-1:15),OpGreaterThan(1:16-1:17),Int(1:18-1:19),OpStar(1:20-1:21),Int(1:22-1:23),OpOr(1:24-1:26),Int(1:27-1:29),OpPlus(1:30-1:31),Int(1:32-1:33),OpLessThan(1:34-1:35),Int(1:36-1:37),OpAnd(1:38-1:41),Int(1:42-1:44),OpBinaryMinus(1:45-1:46),Int(1:47-1:48),OpGreaterThanOrEq(1:49-1:51),Int(1:52-1:54),OpOr(1:55-1:57),Int(1:58-1:60),OpLessThanOrEq(1:61-1:63),Int(1:64-1:65),OpSlash(1:66-1:67),Int(1:68-1:69),EndOfFile(1:69-1:69),
~~~
# PARSE
~~~clojure
(e-binop @1-1-1-69 (op "or")
	(e-binop @1-1-1-57 (op "or")
		(e-binop @1-1-1-26 (op ">")
			(e-binop @1-1-1-17 (op "??")
				(e-apply @1-1-1-9
					(e-tag @1-1-1-4 (raw "Err"))
					(e-ident @1-5-1-8 (qaul "") (raw "foo")))
				(e-int @1-13-1-15 (raw "12")))
			(e-binop @1-18-1-26 (op "*")
				(e-int @1-18-1-19 (raw "5"))
				(e-int @1-22-1-23 (raw "5"))))
		(e-binop @1-27-1-57 (op "and")
			(e-binop @1-27-1-41 (op "<")
				(e-binop @1-27-1-35 (op "+")
					(e-int @1-27-1-29 (raw "13"))
					(e-int @1-32-1-33 (raw "2")))
				(e-int @1-36-1-37 (raw "5")))
			(e-binop @1-42-1-57 (op ">=")
				(e-binop @1-42-1-51 (op "-")
					(e-int @1-42-1-44 (raw "10"))
					(e-int @1-47-1-48 (raw "1")))
				(e-int @1-52-1-54 (raw "16")))))
	(e-binop @1-58-1-69 (op "<=")
		(e-int @1-58-1-60 (raw "12"))
		(e-binop @1-64-1-69 (op "/")
			(e-int @1-64-1-65 (raw "3"))
			(e-int @1-68-1-69 (raw "5")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop @1-1-1-69 (op "or") (id 101)
	(e-binop @1-1-1-57 (op "or")
		(e-binop @1-1-1-26 (op "gt")
			(e-binop @1-1-1-17 (op "null_coalesce")
				(e-call @1-1-1-9
					(e-tag @1-1-1-4 (ext-var 0) (name "Err") (args "TODO"))
					(e-runtime-error (tag "ident_not_in_scope")))
				(e-int @1-13-1-15 (value "12")))
			(e-binop @1-18-1-26 (op "mul")
				(e-int @1-18-1-19 (value "5"))
				(e-int @1-22-1-23 (value "5"))))
		(e-binop @1-27-1-57 (op "and")
			(e-binop @1-27-1-41 (op "lt")
				(e-binop @1-27-1-35 (op "add")
					(e-int @1-27-1-29 (value "13"))
					(e-int @1-32-1-33 (value "2")))
				(e-int @1-36-1-37 (value "5")))
			(e-binop @1-42-1-57 (op "ge")
				(e-binop @1-42-1-51 (op "sub")
					(e-int @1-42-1-44 (value "10"))
					(e-int @1-47-1-48 (value "1")))
				(e-int @1-52-1-54 (value "16")))))
	(e-binop @1-58-1-69 (op "le")
		(e-int @1-58-1-60 (value "12"))
		(e-binop @1-64-1-69 (op "div")
			(e-int @1-64-1-65 (value "3"))
			(e-int @1-68-1-69 (value "5")))))
~~~
# TYPES
~~~clojure
(expr (id 101) (type "*"))
~~~
