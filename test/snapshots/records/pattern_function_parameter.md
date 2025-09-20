# META
~~~ini
description=Record pattern destructuring in function parameter
type=statement
~~~
# SOURCE
~~~roc
formatUser = |{ name, age, email }| "User: ${name} (${age.toStr()} years old) - Contact: ${email.display()}"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:11),OpAssign(1:12-1:13),OpBar(1:14-1:15),OpenCurly(1:15-1:16),LowerIdent(1:17-1:21),Comma(1:21-1:22),LowerIdent(1:23-1:26),Comma(1:26-1:27),LowerIdent(1:28-1:33),CloseCurly(1:34-1:35),OpBar(1:35-1:36),StringStart(1:37-1:38),StringPart(1:38-1:44),OpenStringInterpolation(1:44-1:46),LowerIdent(1:46-1:50),CloseStringInterpolation(1:50-1:51),StringPart(1:51-1:53),OpenStringInterpolation(1:53-1:55),LowerIdent(1:55-1:58),NoSpaceDotLowerIdent(1:58-1:64),NoSpaceOpenRound(1:64-1:65),CloseRound(1:65-1:66),CloseStringInterpolation(1:66-1:67),StringPart(1:67-1:90),OpenStringInterpolation(1:90-1:92),LowerIdent(1:92-1:97),NoSpaceDotLowerIdent(1:97-1:105),NoSpaceOpenRound(1:105-1:106),CloseRound(1:106-1:107),CloseStringInterpolation(1:107-1:108),StringPart(1:108-1:108),StringEnd(1:108-1:109),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(s-decl @1.1-1.109
	(p-ident @1.1-1.11 (raw "formatUser"))
	(e-lambda @1.14-1.109
		(args
			(p-record @1.15-1.35
				(field @1.17-1.21 (name "name") (rest false))
				(field @1.23-1.26 (name "age") (rest false))
				(field @1.28-1.33 (name "email") (rest false))))
		(e-string @1.37-1.109
			(e-string-part @1.38-1.44 (raw "User: "))
			(e-ident @1.46-1.50 (raw "name"))
			(e-string-part @1.51-1.53 (raw " ("))
			(e-static-dispatch @1.55-1.66
				subject
				(e-ident @1.55-1.58 (raw "age"))
				method
				"toStr"
				args)
			(e-string-part @1.67-1.90 (raw " years old) - Contact: "))
			(e-static-dispatch @1.92-1.107
				subject
				(e-ident @1.92-1.97 (raw "email"))
				method
				"display"
				args)
			(e-string-part @1.108-1.108 (raw "")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-let @1.1-1.109
		(p-assign @1.1-1.11 (ident "formatUser"))
		(e-lambda @1.14-1.109
			(args
				(p-record-destructure @1.15-1.35
					(destructs
						(record-destruct @1.17-1.21 (label "name") (ident "name")
							(required
								(p-assign @1.17-1.21 (ident "name"))))
						(record-destruct @1.23-1.26 (label "age") (ident "age")
							(required
								(p-assign @1.23-1.26 (ident "age"))))
						(record-destruct @1.28-1.33 (label "email") (ident "email")
							(required
								(p-assign @1.28-1.33 (ident "email")))))))
			(e-string @1.37-1.109
				(e-literal @1.38-1.44 (string "User: "))
				(e-lookup-local @1.46-1.50
					(p-assign @1.17-1.21 (ident "name")))
				(e-literal @1.51-1.53 (string " ("))
				(e-call @1.55-1.66
					(e-lookup-local @1.55-1.58
						(p-assign @1.23-1.26 (ident "age"))))
				(e-literal @1.67-1.90 (string " years old) - Contact: "))
				(e-call @1.92-1.107
					(e-lookup-local @1.92-1.97
						(p-assign @1.28-1.33 (ident "email"))))
				(e-literal @1.108-1.108 (string ""))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
