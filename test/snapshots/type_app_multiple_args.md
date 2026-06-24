# META
~~~ini
description=Multiple type arguments application in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processDict : Dict(Str, U64) -> List(Str)
processDict = |_dict| []

main! = |_| processDict(Dict.empty().insert("one", 1))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenSquare,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,Int,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno (name "processDict")
			(ty-fn
				(ty-apply
					(ty (name "Dict"))
					(ty (name "Str"))
					(ty (name "U64")))
				(ty-apply
					(ty (name "List"))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "processDict"))
			(e-lambda
				(args
					(p-ident (raw "_dict")))
				(e-list)))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ident (raw "processDict"))
					(e-method-call (method ".insert")
						(receiver
							(e-apply
								(e-ident (raw "Dict.empty"))))
						(args
							(e-string
								(e-string-part (raw "one")))
							(e-int (raw "1")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processDict"))
		(e-lambda
			(args
				(p-assign (ident "_dict")))
			(e-empty_list))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Dict") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-lookup (name "U64") (builtin)))
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-call (constraint-fn-var 357)
				(e-lookup-local
					(p-assign (ident "processDict")))
				(e-dispatch-call (method "insert") (constraint-fn-var 251)
					(receiver
						(e-call (constraint-fn-var 201)
							(e-lookup-external
								(builtin))))
					(args
						(e-string
							(e-literal (string "one")))
						(e-num (value "1"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dict(Str, U64) -> List(Str)"))
		(patt (type "_arg -> List(Str)")))
	(expressions
		(expr (type "Dict(Str, U64) -> List(Str)"))
		(expr (type "_arg -> List(Str)"))))
~~~
