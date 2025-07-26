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
UNDEFINED VARIABLE - type_app_multiple_args.md:6:25:6:35
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `empty` in this scope.
Is there an `import` or `exposing` missing up-top?

**type_app_multiple_args.md:6:25:6:35:**
```roc
main! = |_| processDict(Dict.empty().insert("one", 1))
```
                        ^^^^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:12),OpColon(3:13-3:14),UpperIdent(3:15-3:19),NoSpaceOpenRound(3:19-3:20),UpperIdent(3:20-3:23),Comma(3:23-3:24),UpperIdent(3:25-3:28),CloseRound(3:28-3:29),OpArrow(3:30-3:32),UpperIdent(3:33-3:37),NoSpaceOpenRound(3:37-3:38),UpperIdent(3:38-3:41),CloseRound(3:41-3:42),
LowerIdent(4:1-4:12),OpAssign(4:13-4:14),OpBar(4:15-4:16),NamedUnderscore(4:16-4:21),OpBar(4:21-4:22),OpenSquare(4:23-4:24),CloseSquare(4:24-4:25),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),LowerIdent(6:13-6:24),NoSpaceOpenRound(6:24-6:25),UpperIdent(6:25-6:29),NoSpaceDotLowerIdent(6:29-6:35),NoSpaceOpenRound(6:35-6:36),CloseRound(6:36-6:37),NoSpaceDotLowerIdent(6:37-6:44),NoSpaceOpenRound(6:44-6:45),StringStart(6:45-6:46),StringPart(6:46-6:49),StringEnd(6:49-6:50),Comma(6:50-6:51),Int(6:52-6:53),CloseRound(6:53-6:54),CloseRound(6:54-6:55),EndOfFile(6:55-6:55),
~~~
# PARSE
~~~clojure
(file @1.1-6.55
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @3.1-3.42 (name "processDict")
			(ty-fn @3.15-3.42
				(ty-apply @3.15-3.29
					(ty @3.15-3.19 (name "Dict"))
					(ty @3.20-3.23 (name "Str"))
					(ty @3.25-3.28 (name "U64")))
				(ty-apply @3.33-3.42
					(ty @3.33-3.37 (name "List"))
					(ty @3.38-3.41 (name "Str")))))
		(s-decl @4.1-4.25
			(p-ident @4.1-4.12 (raw "processDict"))
			(e-lambda @4.15-4.25
				(args
					(p-ident @4.16-4.21 (raw "_dict")))
				(e-list @4.23-4.25)))
		(s-decl @6.1-6.55
			(p-ident @6.1-6.6 (raw "main!"))
			(e-lambda @6.9-6.55
				(args
					(p-underscore))
				(e-apply @6.13-6.55
					(e-ident @6.13-6.24 (raw "processDict"))
					(e-field-access @6.25-6.54
						(e-apply @6.25-6.37
							(e-ident @6.25-6.35 (raw "Dict.empty")))
						(e-apply @6.37-6.54
							(e-ident @6.37-6.44 (raw "insert"))
							(e-string @6.45-6.50
								(e-string-part @6.46-6.49 (raw "one")))
							(e-int @6.52-6.53 (raw "1")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.12 (ident "processDict"))
		(e-lambda @4.15-4.25
			(args
				(p-assign @4.16-4.21 (ident "_dict")))
			(e-empty_list @4.23-4.25))
		(annotation @4.1-4.12
			(declared-type
				(ty-fn @3.15-3.42 (effectful false)
					(ty-apply @3.15-3.29 (symbol "Dict")
						(ty @3.20-3.23 (name "Str"))
						(ty @3.25-3.28 (name "U64")))
					(ty-apply @3.33-3.42 (symbol "List")
						(ty @3.38-3.41 (name "Str")))))))
	(d-let
		(p-assign @6.1-6.6 (ident "main!"))
		(e-closure @6.9-6.55
			(captures
				(capture @4.1-4.12 (ident "processDict")))
			(e-lambda @6.9-6.55
				(args
					(p-underscore @6.10-6.11))
				(e-call @6.13-6.55
					(e-lookup-local @6.13-6.24
						(p-assign @4.1-4.12 (ident "processDict")))
					(e-dot-access @6.25-6.54 (field "insert")
						(receiver
							(e-call @6.25-6.37
								(e-runtime-error (tag "ident_not_in_scope"))))
						(args
							(e-string @6.45-6.50
								(e-literal @6.46-6.49 (string "one")))
							(e-int @6.52-6.53 (value "1")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.12 (type "Error -> Error"))
		(patt @6.1-6.6 (type "_arg -> Error")))
	(expressions
		(expr @4.15-4.25 (type "Error -> Error"))
		(expr @6.9-6.55 (type "_arg -> Error"))))
~~~
