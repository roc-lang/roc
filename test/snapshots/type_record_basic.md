# META
~~~ini
description=Basic record type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getName : { name: Str, age: U64 } -> Str
getName = |_person| "hello"

main! = |_| getName({namee: "luke", age:21})
~~~
# EXPECTED
TYPE MISMATCH - type_record_basic.md:6:13:6:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 6 13) (end 6 45))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-underlines
				(display (file "type_record_basic.md") (start 6 13) (end 6 45) (annotation dim) (line-text "main! = |_| getName({namee: \"luke\", age:21})"))
				(underline (start 6 21) (end 6 44) (annotation error)))
			(line-break)
			(reflow "This argument has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ age: U64, namee: a }")
			(line-break)
			(indent 1)
			(text "  where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But")
			(reflow " ")
			(annotated code "getName")
			(reflow " ")
			(reflow "needs the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ age: U64, name: Str }")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Maybe")
			(reflow " ")
			(annotated code "namee")
			(reflow " ")
			(reflow "should be")
			(reflow " ")
			(annotated code "name")
			(reflow "?"))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseRound,
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
		(s-type-anno (name "getName")
			(ty-fn
				(ty-record
					(anno-record-field (name "name")
						(ty (name "Str")))
					(anno-record-field (name "age")
						(ty (name "U64"))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "getName"))
			(e-lambda
				(args
					(p-ident (raw "_person")))
				(e-string
					(e-string-part (raw "hello")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-ident (raw "getName"))
					(e-record
						(field (field "namee")
							(e-string
								(e-string-part (raw "luke"))))
						(field (field "age")
							(e-int (raw "21")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getName : { name : Str, age : U64 } -> Str
getName = |_person| "hello"

main! = |_| getName({ namee: "luke", age: 21 })
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "getName"))
		(e-lambda
			(args
				(p-assign (ident "_person")))
			(e-string
				(e-literal (string "hello"))))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "name")
						(ty-lookup (name "Str") (builtin)))
					(field (field "age")
						(ty-lookup (name "U64") (builtin))))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "main!"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ age: U64, name: Str } -> Str"))
		(patt (type "_arg -> Error")))
	(expressions
		(expr (type "{ age: U64, name: Str } -> Str"))
		(expr (type "_arg -> Error"))))
~~~
