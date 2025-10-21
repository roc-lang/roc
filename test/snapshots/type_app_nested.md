# META
~~~ini
description=Nested type applications in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processNested : List(Result(Str, Err)) -> List(Str)
processNested = |_list| ["one","two"]

main! = |_| processNested([])
~~~
# EXPECTED
UNDECLARED TYPE - type_app_nested.md:3:34:3:37
# PROBLEMS
**UNDECLARED TYPE**
The type _Err_ is not declared in this scope.

This type is referenced here:
**type_app_nested.md:3:34:3:37:**
```roc
processNested : List(Result(Str, Err)) -> List(Str)
```
                                 ^^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,OpenSquare,CloseSquare,CloseRound,
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
		(s-type-anno (name "processNested")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-apply
						(ty (name "Result"))
						(ty (name "Str"))
						(ty (name "Err"))))
				(ty-apply
					(ty (name "List"))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "processNested"))
			(e-lambda
				(args
					(p-ident (raw "_list")))
				(e-list
					(e-string
						(e-string-part (raw "one")))
					(e-string
						(e-string-part (raw "two"))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ident (raw "processNested"))
					(e-list))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processNested : List(Result(Str, Err)) -> List(Str)
processNested = |_list| ["one", "two"]

main! = |_| processNested([])
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processNested"))
		(e-lambda
			(args
				(p-assign (ident "_list")))
			(e-list
				(elems
					(e-string
						(e-literal (string "one")))
					(e-string
						(e-literal (string "two"))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "List") (builtin)
						(ty-apply (name "Result") (module "Result")
							(ty-lookup (name "Str") (builtin))
							(ty-malformed)))
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "Str") (builtin)))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "processNested")))
			(e-lambda
				(args
					(p-underscore))
				(e-call
					(e-lookup-local
						(p-assign (ident "processNested")))
					(e-empty_list))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Result(Str, Error)) -> List(Str)"))
		(patt (type "_arg -> List(Str)")))
	(expressions
		(expr (type "List(Result(Str, Error)) -> List(Str)"))
		(expr (type "_arg -> List(Str)"))))
~~~
