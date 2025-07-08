# META
~~~ini
description=Effectful function with effectful annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

# Function with effectful annotation using fat arrow
print_msg! : Str => {}
print_msg! = |msg| Stdout.line!(msg)

main! = print_msg!("Hello, world!")
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:10),NoSpaceDotUpperIdent(3:10-3:17),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(5:2-5:53),
LowerIdent(6:1-6:11),OpColon(6:12-6:13),UpperIdent(6:14-6:17),OpFatArrow(6:18-6:20),OpenCurly(6:21-6:22),CloseCurly(6:22-6:23),Newline(1:1-1:1),
LowerIdent(7:1-7:11),OpAssign(7:12-7:13),OpBar(7:14-7:15),LowerIdent(7:15-7:18),OpBar(7:18-7:19),UpperIdent(7:20-7:26),NoSpaceDotLowerIdent(7:26-7:32),NoSpaceOpenRound(7:32-7:33),LowerIdent(7:33-7:36),CloseRound(7:36-7:37),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(9:1-9:6),OpAssign(9:7-9:8),LowerIdent(9:9-9:19),NoSpaceOpenRound(9:19-9:20),StringStart(9:20-9:21),StringPart(9:21-9:34),StringEnd(9:34-9:35),CloseRound(9:35-9:36),EndOfFile(9:36-9:36),
~~~
# PARSE
~~~clojure
(file @1.1-9.36
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
		(s-type-anno @6.1-7.11 (name "print_msg!")
			(ty-fn @6.14-6.23
				(ty @6.14-6.17 (name "Str"))
				(ty-record @6.21-6.23)))
		(s-decl @7.1-7.37
			(p-ident @7.1-7.11 (raw "print_msg!"))
			(e-lambda @7.14-7.37
				(args
					(p-ident @7.15-7.18 (raw "msg")))
				(e-apply @7.20-7.37
					(e-ident @7.20-7.32 (raw "Stdout.line!"))
					(e-ident @7.33-7.36 (raw "msg")))))
		(s-decl @9.1-9.36
			(p-ident @9.1-9.6 (raw "main!"))
			(e-apply @9.9-9.36
				(e-ident @9.9-9.19 (raw "print_msg!"))
				(e-string @9.20-9.35
					(e-string-part @9.21-9.34 (raw "Hello, world!")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @7.1-7.11 (ident "print_msg!"))
		(e-lambda @7.14-7.37
			(args
				(p-assign @7.15-7.18 (ident "msg")))
			(e-call @7.20-7.37
				(e-lookup-external @7.20-7.32
					(module-idx "0")
					(field "line!")
					(target-node-idx "0"))
				(e-lookup-local @7.33-7.36
					(pattern @7.15-7.18))))
		(annotation @7.1-7.11
			(declared-type
				(ty-fn @6.14-6.23 (effectful true)
					(ty @6.14-6.17 (name "Str"))
					(ty-record @6.21-6.23)))))
	(d-let
		(p-assign @9.1-9.6 (ident "main!"))
		(e-call @9.9-9.36
			(e-lookup-local @9.9-9.19
				(pattern @7.1-7.11))
			(e-string @9.20-9.35
				(e-literal @9.21-9.34 (string "Hello, world!")))))
	(s-import @3.1-3.17 (module "pf.Stdout") (qualifier "pf")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.11 (type "Str => {  }"))
		(patt @9.1-9.6 (type "{  }")))
	(expressions
		(expr @7.14-7.37 (type "Str => {  }"))
		(expr @9.9-9.36 (type "{  }"))))
~~~
