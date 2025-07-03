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
# PROBLEMS
**UNDECLARED TYPE**
The type ``Err`` is not declared in this scope.

This type is referenced here:
**type_app_nested.md:3:34:3:37:**
```roc
processNested : List(Result(Str, Err)) -> List(Str)
```
                                 ^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:14),OpColon(3:15-3:16),UpperIdent(3:17-3:21),NoSpaceOpenRound(3:21-3:22),UpperIdent(3:22-3:28),NoSpaceOpenRound(3:28-3:29),UpperIdent(3:29-3:32),Comma(3:32-3:33),UpperIdent(3:34-3:37),CloseRound(3:37-3:38),CloseRound(3:38-3:39),OpArrow(3:40-3:42),UpperIdent(3:43-3:47),NoSpaceOpenRound(3:47-3:48),UpperIdent(3:48-3:51),CloseRound(3:51-3:52),Newline(1:1-1:1),
LowerIdent(4:1-4:14),OpAssign(4:15-4:16),OpBar(4:17-4:18),NamedUnderscore(4:18-4:23),OpBar(4:23-4:24),OpenSquare(4:25-4:26),StringStart(4:26-4:27),StringPart(4:27-4:30),StringEnd(4:30-4:31),Comma(4:31-4:32),StringStart(4:32-4:33),StringPart(4:33-4:36),StringEnd(4:36-4:37),CloseSquare(4:37-4:38),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),LowerIdent(6:13-6:26),NoSpaceOpenRound(6:26-6:27),OpenSquare(6:27-6:28),CloseSquare(6:28-6:29),CloseRound(6:29-6:30),EndOfFile(6:30-6:30),
~~~
# PARSE
~~~clojure
(file @1.1-6.30
	(app @1.1-1.53
		(provides @1.6-1.12
			(exposed-lower-ident (text "main!")))
		(record-field @1.15-1.53 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.53 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @3.1-4.14 (name "processNested")
			(ty-fn @3.17-3.52
				(ty-apply @3.17-3.39
					(ty (name "List"))
					(ty-apply @3.22-3.38
						(ty (name "Result"))
						(ty (name "Str"))
						(ty (name "Err"))))
				(ty-apply @3.43-3.52
					(ty (name "List"))
					(ty (name "Str")))))
		(s-decl @4.1-4.38
			(p-ident @4.1-4.14 (raw "processNested"))
			(e-lambda @4.17-4.38
				(args
					(p-ident @4.18-4.23 (raw "_list")))
				(e-list @4.25-4.38
					(e-string @4.26-4.31
						(e-string-part @4.27-4.30 (raw "one")))
					(e-string @4.32-4.37
						(e-string-part @4.33-4.36 (raw "two"))))))
		(s-decl @6.1-6.30
			(p-ident @6.1-6.6 (raw "main!"))
			(e-lambda @6.9-6.30
				(args
					(p-underscore))
				(e-apply @6.13-6.30
					(e-ident @6.13-6.26 (qaul "") (raw "processNested"))
					(e-list @6.27-6.29))))))
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
		(p-assign @4.1-4.14 (ident "processNested"))
		(e-lambda @4.17-4.38
			(args
				(p-assign @4.18-4.23 (ident "_list")))
			(e-list @4.25-4.38
				(elems
					(e-string @4.26-4.31
						(e-literal @4.27-4.30 (string "one")))
					(e-string @4.32-4.37
						(e-literal @4.33-4.36 (string "two"))))))
		(annotation @4.1-4.14
			(declared-type
				(ty-fn @3.17-3.52 (effectful false)
					(ty-apply @3.17-3.39 (symbol "List")
						(ty-apply @3.22-3.38 (symbol "Result")
							(ty @3.29-3.32 (name "Str"))
							(ty @3.34-3.37 (name "Err"))))
					(ty-apply @3.43-3.52 (symbol "List")
						(ty @3.48-3.51 (name "Str")))))))
	(d-let
		(p-assign @6.1-6.6 (ident "main!"))
		(e-lambda @6.9-6.30
			(args
				(p-underscore @6.10-6.11))
			(e-call @6.13-6.30
				(e-lookup-local @6.13-6.26
					(pattern @4.1-4.14))
				(e-empty_list @6.27-6.29)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.14 (type "Error -> Error"))
		(patt @6.1-6.6 (type "a -> Error")))
	(expressions
		(expr @4.17-4.38 (type "Error -> Error"))
		(expr @6.9-6.30 (type "a -> Error"))))
~~~
