# META
~~~ini
description=Tuple containing variations on boolean values
type=expr
~~~
# SOURCE
~~~roc
(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),UpperIdent(1:2-1:6),Comma(1:6-1:7),UpperIdent(1:8-1:13),Comma(1:13-1:14),UpperIdent(1:15-1:19),NoSpaceDotUpperIdent(1:19-1:24),Comma(1:24-1:25),UpperIdent(1:26-1:30),NoSpaceDotUpperIdent(1:30-1:36),Comma(1:36-1:37),OpBang(1:38-1:39),UpperIdent(1:39-1:43),Comma(1:43-1:44),OpBang(1:45-1:46),UpperIdent(1:46-1:51),Comma(1:51-1:52),UpperIdent(1:53-1:57),OpAnd(1:58-1:61),UpperIdent(1:62-1:67),Comma(1:67-1:68),OpBang(1:69-1:70),UpperIdent(1:70-1:74),OpOr(1:75-1:77),OpBang(1:78-1:79),UpperIdent(1:79-1:83),CloseRound(1:83-1:84),EndOfFile(1:84-1:84),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.84
	(e-tag @1.2-1.6 (raw "True"))
	(e-tag @1.8-1.13 (raw "False"))
	(e-tag @1.15-1.24 (raw "Bool.True"))
	(e-tag @1.26-1.36 (raw "Bool.False"))
	(unary "!"
		(e-tag @1.39-1.43 (raw "True")))
	(unary "!"
		(e-tag @1.46-1.51 (raw "False")))
	(e-binop @1.53-1.67 (op "and")
		(e-tag @1.53-1.57 (raw "True"))
		(e-tag @1.62-1.67 (raw "False")))
	(e-binop @1.69-1.83 (op "or")
		(unary "!"
			(e-tag @1.70-1.74 (raw "True")))
		(unary "!"
			(e-tag @1.79-1.83 (raw "True")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-1.84
	(elems
		(e-nominal @1.2-1.6 (nominal "Bool")
			(e-tag @1.2-1.6 (name "True")))
		(e-nominal @1.8-1.13 (nominal "Bool")
			(e-tag @1.8-1.13 (name "False")))
		(e-nominal @1.15-1.24 (nominal "Bool")
			(e-tag @1.15-1.24 (name "True")))
		(e-nominal @1.26-1.36 (nominal "Bool")
			(e-tag @1.26-1.36 (name "False")))
		(e-unary-not @1.38-1.43
			(e-nominal @1.39-1.43 (nominal "Bool")
				(e-tag @1.39-1.43 (name "True"))))
		(e-unary-not @1.45-1.51
			(e-nominal @1.46-1.51 (nominal "Bool")
				(e-tag @1.46-1.51 (name "False"))))
		(e-binop @1.53-1.67 (op "and")
			(e-nominal @1.53-1.57 (nominal "Bool")
				(e-tag @1.53-1.57 (name "True")))
			(e-nominal @1.62-1.67 (nominal "Bool")
				(e-tag @1.62-1.67 (name "False"))))
		(e-binop @1.69-1.83 (op "or")
			(e-unary-not @1.69-1.74
				(e-nominal @1.70-1.74 (nominal "Bool")
					(e-tag @1.70-1.74 (name "True"))))
			(e-unary-not @1.78-1.83
				(e-nominal @1.79-1.83 (nominal "Bool")
					(e-tag @1.79-1.83 (name "True")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.84 (type "(Bool, Bool, Bool, Bool, Bool, Bool, _field, _field2)"))
~~~
