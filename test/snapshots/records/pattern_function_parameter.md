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
LowerIdent,OpAssign,OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseCurly,OpBar,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-decl
	(p-ident (raw "formatUser"))
	(e-lambda
		(args
			(p-record
				(field (name "name") (rest false))
				(field (name "age") (rest false))
				(field (name "email") (rest false))))
		(e-string
			(e-string-part (raw "User: "))
			(e-ident (raw "name"))
			(e-string-part (raw " ("))
			(e-field-access
				(e-ident (raw "age"))
				(e-apply
					(e-ident (raw "toStr"))))
			(e-string-part (raw " years old) - Contact: "))
			(e-field-access
				(e-ident (raw "email"))
				(e-apply
					(e-ident (raw "display"))))
			(e-string-part (raw "")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-let
		(p-assign (ident "formatUser"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs
						(record-destruct (label "name") (ident "name")
							(required
								(p-assign (ident "name"))))
						(record-destruct (label "age") (ident "age")
							(required
								(p-assign (ident "age"))))
						(record-destruct (label "email") (ident "email")
							(required
								(p-assign (ident "email")))))))
			(e-string
				(e-literal (string "User: "))
				(e-lookup-local
					(p-assign (ident "name")))
				(e-literal (string " ("))
				(e-dot-access (field "toStr")
					(receiver
						(e-lookup-local
							(p-assign (ident "age"))))
					(args))
				(e-literal (string " years old) - Contact: "))
				(e-dot-access (field "display")
					(receiver
						(e-lookup-local
							(p-assign (ident "email"))))
					(args))
				(e-literal (string ""))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
