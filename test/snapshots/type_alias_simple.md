# META
~~~ini
description=Simple type alias usage in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

UserId : U64

getUser : UserId -> Str
getUser = |id| if (id > 10) "big" else "small"

main! = |_| getUser(100)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
UpperIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,OpenRound,LowerIdent,OpGreaterThan,Int,CloseRound,StringStart,StringPart,StringEnd,KwElse,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
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
		(s-type-decl
			(header (name "UserId")
				(args))
			(ty (name "U64")))
		(s-type-anno (name "getUser")
			(ty-fn
				(ty (name "UserId"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "getUser"))
			(e-lambda
				(args
					(p-ident (raw "id")))
				(e-if-then-else
					(e-tuple
						(e-binop (op ">")
							(e-ident (raw "id"))
							(e-int (raw "10"))))
					(e-string
						(e-string-part (raw "big")))
					(e-string
						(e-string-part (raw "small"))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ident (raw "getUser"))
					(e-int (raw "100")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "getUser"))
		(e-lambda
			(args
				(p-assign (ident "id")))
			(e-if
				(if-branches
					(if-branch
						(e-binop (op "gt")
							(e-lookup-local
								(p-assign (ident "id")))
							(e-num (value "10")))
						(e-string
							(e-literal (string "big")))))
				(if-else
					(e-string
						(e-literal (string "small"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "UserId") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "getUser")))
			(e-lambda
				(args
					(p-underscore))
				(e-call
					(e-lookup-local
						(p-assign (ident "getUser")))
					(e-num (value "100"))))))
	(s-alias-decl
		(ty-header (name "UserId"))
		(ty-lookup (name "U64") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "UserId -> Str"))
		(patt (type "_arg -> Str")))
	(type_decls
		(alias (type "UserId")
			(ty-header (name "UserId"))))
	(expressions
		(expr (type "UserId -> Str"))
		(expr (type "_arg -> Str"))))
~~~
