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
DOES NOT EXIST - type_app_multiple_args.md:6:25:6:35
TOO MANY ARGS - type_app_multiple_args.md:3:15:3:29
# PROBLEMS
**DOES NOT EXIST**
`Dict.empty` does not exist.

**type_app_multiple_args.md:6:25:6:35:**
```roc
main! = |_| processDict(Dict.empty().insert("one", 1))
```
                        ^^^^^^^^^^


**TOO MANY ARGS**
The type _Dict_ expects  argument, but got  instead.
**type_app_multiple_args.md:3:15:3:29:**
```roc
processDict : Dict(Str, U64) -> List(Str)
```
              ^^^^^^^^^^^^^^



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
					(e-field-access
						(e-apply
							(e-ident (raw "Dict.empty")))
						(e-apply
							(e-ident (raw "insert"))
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
				(ty-apply (name "Dict") (external (module-idx "0") (target-node-idx "1"))
					(ty-lookup (name "Str") (builtin))
					(ty-lookup (name "U64") (builtin)))
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "processDict")))
			(e-lambda
				(args
					(p-underscore))
				(e-call
					(e-lookup-local
						(p-assign (ident "processDict")))
					(e-dot-access (field "insert")
						(receiver
							(e-call
								(e-runtime-error (tag "qualified_ident_does_not_exist"))))
						(args
							(e-string
								(e-literal (string "one")))
							(e-num (value "1")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> List(Str)"))
		(patt (type "_arg -> Error")))
	(expressions
		(expr (type "Error -> List(Str)"))
		(expr (type "_arg -> Error"))))
~~~
