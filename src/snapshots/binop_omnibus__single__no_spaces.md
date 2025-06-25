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
(e-binop @1-1-1-51 (op "or") (id 124)
	(e-binop @1-1-1-43 (op "or")
		(e-binop @1-1-1-20 (op "gt")
			(e-binop @1-1-1-14 (op "null_coalesce")
				(e-call @1-1-1-9
					(e-tag @1-1-1-4 (ext-var 0) (name "Err") (args "TODO"))
					(e-runtime-error (tag "ident_not_in_scope")))
				(e-int @1-11-1-13 (int-var 78) (precision-var 77) (literal "12") (value "TODO") (bound "u8")))
			(e-binop @1-14-1-20 (op "mul")
				(e-int @1-14-1-15 (int-var 82) (precision-var 81) (literal "5") (value "TODO") (bound "u8"))
				(e-int @1-16-1-17 (int-var 85) (precision-var 84) (literal "5") (value "TODO") (bound "u8"))))
		(e-binop @1-21-1-43 (op "and")
			(e-binop @1-21-1-31 (op "lt")
				(e-binop @1-21-1-26 (op "add")
					(e-int @1-21-1-23 (int-var 90) (precision-var 89) (literal "13") (value "TODO") (bound "u8"))
					(e-int @1-24-1-25 (int-var 93) (precision-var 92) (literal "2") (value "TODO") (bound "u8")))
				(e-int @1-26-1-27 (int-var 97) (precision-var 96) (literal "5") (value "TODO") (bound "u8")))
			(e-binop @1-32-1-43 (op "ge")
				(e-binop @1-32-1-38 (op "sub")
					(e-int @1-32-1-34 (int-var 101) (precision-var 100) (literal "10") (value "TODO") (bound "u8"))
					(e-int @1-35-1-36 (int-var 104) (precision-var 103) (literal "1") (value "TODO") (bound "u8")))
				(e-int @1-38-1-40 (int-var 108) (precision-var 107) (literal "16") (value "TODO") (bound "u8")))))
	(e-binop @1-44-1-51 (op "le")
		(e-int @1-44-1-46 (int-var 114) (precision-var 113) (literal "12") (value "TODO") (bound "u8"))
		(e-binop @1-48-1-51 (op "div")
			(e-int @1-48-1-49 (int-var 117) (precision-var 116) (literal "3") (value "TODO") (bound "u8"))
			(e-int @1-50-1-51 (int-var 120) (precision-var 119) (literal "5") (value "TODO") (bound "u8")))))
~~~
# TYPES
~~~clojure
(expr (id 124) (type "*"))
~~~