# META
~~~ini
description=Type mismatch - pure annotation with effectful body
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

# This should be a type error: pure annotation but effectful body
bad_function : Str -> {}
bad_function = |msg| Stdout.line!(msg)

main! = bad_function("This should fail")
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),
KwImport(3:1-3:7),LowerIdent(3:8-3:10),NoSpaceDotUpperIdent(3:10-3:17),
LowerIdent(6:1-6:13),OpColon(6:14-6:15),UpperIdent(6:16-6:19),OpArrow(6:20-6:22),OpenCurly(6:23-6:24),CloseCurly(6:24-6:25),
LowerIdent(7:1-7:13),OpAssign(7:14-7:15),OpBar(7:16-7:17),LowerIdent(7:17-7:20),OpBar(7:20-7:21),UpperIdent(7:22-7:28),NoSpaceDotLowerIdent(7:28-7:34),NoSpaceOpenRound(7:34-7:35),LowerIdent(7:35-7:38),CloseRound(7:38-7:39),
LowerIdent(9:1-9:6),OpAssign(9:7-9:8),LowerIdent(9:9-9:21),NoSpaceOpenRound(9:21-9:22),StringStart(9:22-9:23),StringPart(9:23-9:39),StringEnd(9:39-9:40),CloseRound(9:40-9:41),EndOfFile(9:41-9:41),
~~~
# PARSE
~~~clojure
(file @1.1-9.41
	(app @1.1-1.57
		(provides @1.6-1.12
			(exposed-lower-ident (text "main!")))
		(record-field @1.15-1.57 (name "pf")
			(e-string @1.28-1.55
				(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))
		(packages @1.13-1.57
			(record-field @1.15-1.57 (name "pf")
				(e-string @1.28-1.55
					(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-import @3.1-3.17 (raw "pf.Stdout"))
		(s-type-anno @6.1-7.13 (name "bad_function")
			(ty-fn @6.16-6.25
				(ty @6.16-6.19 (name "Str"))
				(ty-record @6.23-6.25)))
		(s-decl @7.1-7.39
			(p-ident @7.1-7.13 (raw "bad_function"))
			(e-lambda @7.16-7.39
				(args
					(p-ident @7.17-7.20 (raw "msg")))
				(e-apply @7.22-7.39
					(e-ident @7.22-7.34 (raw "Stdout.line!"))
					(e-ident @7.35-7.38 (raw "msg")))))
		(s-decl @9.1-9.41
			(p-ident @9.1-9.6 (raw "main!"))
			(e-apply @9.9-9.41
				(e-ident @9.9-9.21 (raw "bad_function"))
				(e-string @9.22-9.40
					(e-string-part @9.23-9.39 (raw "This should fail")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @7.1-7.13 (ident "bad_function"))
		(e-lambda @7.16-7.39
			(args
				(p-assign @7.17-7.20 (ident "msg")))
			(e-call @7.22-7.39
				(e-lookup-external @7.22-7.34
					(module-idx "0")
					(target-node-idx "0"))
				(e-lookup-local @7.35-7.38
					(p-assign @7.17-7.20 (ident "msg")))))
		(annotation @7.1-7.13
			(declared-type
				(ty-fn @6.16-6.25 (effectful false)
					(ty @6.16-6.19 (name "Str"))
					(ty-record @6.23-6.25)))))
	(d-let
		(p-assign @9.1-9.6 (ident "main!"))
		(e-call @9.9-9.41
			(e-lookup-local @9.9-9.21
				(p-assign @7.1-7.13 (ident "bad_function")))
			(e-string @9.22-9.40
				(e-literal @9.23-9.39 (string "This should fail")))))
	(s-import @3.1-3.17 (module "pf.Stdout") (qualifier "pf")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.13 (type "Str -> {  }"))
		(patt @9.1-9.6 (type "{  }")))
	(expressions
		(expr @7.16-7.39 (type "Str -> {  }"))
		(expr @9.9-9.41 (type "{  }"))))
~~~
