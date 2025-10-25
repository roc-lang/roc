# META
~~~ini
description=Single type argument application in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processList : List(Str) -> U64
processList = |list| list.len()

main! = |_| processList(["one","two"])
~~~
# EXPECTED
TYPE DOES NOT HAVE METHODS - type_app_single_arg.md:4:22:4:32
# PROBLEMS
**TYPE DOES NOT HAVE METHODS**
You're trying to call the `len` method on a `List(Str)`:
**type_app_single_arg.md:4:22:4:32:**
```roc
processList = |list| list.len()
```
                     ^^^^^^^^^^

But `List(Str)` doesn't support methods.

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,CloseRound,
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
		(s-type-anno (name "processList")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "Str")))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "processList"))
			(e-lambda
				(args
					(p-ident (raw "list")))
				(e-field-access
					(e-ident (raw "list"))
					(e-apply
						(e-ident (raw "len"))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ident (raw "processList"))
					(e-list
						(e-string
							(e-string-part (raw "one")))
						(e-string
							(e-string-part (raw "two")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processList : List(Str) -> U64
processList = |list| list.len()

main! = |_| processList(["one", "two"])
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processList"))
		(e-lambda
			(args
				(p-assign (ident "list")))
			(e-dot-access (field "len")
				(receiver
					(e-lookup-local
						(p-assign (ident "list"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Str") (builtin)))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "processList")))
			(e-lambda
				(args
					(p-underscore))
				(e-call
					(e-lookup-local
						(p-assign (ident "processList")))
					(e-list
						(elems
							(e-string
								(e-literal (string "one")))
							(e-string
								(e-literal (string "two"))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Str) -> Error"))
		(patt (type "_arg -> Num(Int(Unsigned64))")))
	(expressions
		(expr (type "List(Str) -> Error"))
		(expr (type "_arg -> Num(Int(Unsigned64))"))))
~~~
