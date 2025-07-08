# META
~~~ini
description=Hello world with a block
type=file
~~~
# SOURCE
~~~roc
# Hello world!

# Multiline comments?
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

main! = |_| {
	name = "World"
	# Hello
	Stdout.line!("Hello, ${name}!")
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Newline(1:2-1:15),
Newline(1:1-1:1),
Newline(3:2-3:22),
KwApp(4:1-4:4),OpenSquare(4:5-4:6),LowerIdent(4:6-4:11),CloseSquare(4:11-4:12),OpenCurly(4:13-4:14),LowerIdent(4:15-4:17),OpColon(4:17-4:18),KwPlatform(4:19-4:27),StringStart(4:28-4:29),StringPart(4:29-4:54),StringEnd(4:54-4:55),CloseCurly(4:56-4:57),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(6:1-6:7),LowerIdent(6:8-6:10),NoSpaceDotUpperIdent(6:10-6:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(8:1-8:6),OpAssign(8:7-8:8),OpBar(8:9-8:10),Underscore(8:10-8:11),OpBar(8:11-8:12),OpenCurly(8:13-8:14),Newline(1:1-1:1),
LowerIdent(9:2-9:6),OpAssign(9:7-9:8),StringStart(9:9-9:10),StringPart(9:10-9:15),StringEnd(9:15-9:16),Newline(1:1-1:1),
Newline(10:3-10:9),
UpperIdent(11:2-11:8),NoSpaceDotLowerIdent(11:8-11:14),NoSpaceOpenRound(11:14-11:15),StringStart(11:15-11:16),StringPart(11:16-11:23),OpenStringInterpolation(11:23-11:25),LowerIdent(11:25-11:29),CloseStringInterpolation(11:29-11:30),StringPart(11:30-11:31),StringEnd(11:31-11:32),CloseRound(11:32-11:33),Newline(1:1-1:1),
CloseCurly(12:1-12:2),EndOfFile(12:2-12:2),
~~~
# PARSE
~~~clojure
(file @1.2-12.2
	(app @4.1-4.57
		(provides @4.6-4.12
			(exposed-lower-ident (text "main!")))
		(record-field @4.15-4.57 (name "pf")
			(e-string @4.28-4.55
				(e-string-part @4.29-4.54 (raw "../basic-cli/platform.roc"))))
		(packages @4.13-4.57
			(record-field @4.15-4.57 (name "pf")
				(e-string @4.28-4.55
					(e-string-part @4.29-4.54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-import @6.1-6.17 (raw "pf.Stdout"))
		(s-decl @8.1-12.2
			(p-ident @8.1-8.6 (raw "main!"))
			(e-lambda @8.9-12.2
				(args
					(p-underscore))
				(e-block @8.13-12.2
					(statements
						(s-decl @9.2-9.16
							(p-ident @9.2-9.6 (raw "name"))
							(e-string @9.9-9.16
								(e-string-part @9.10-9.15 (raw "World"))))
						(e-apply @11.2-11.33
							(e-ident @11.2-11.14 (raw "Stdout.line!"))
							(e-string @11.15-11.32
								(e-string-part @11.16-11.23 (raw "Hello, "))
								(e-ident @11.25-11.29 (raw "name"))
								(e-string-part @11.30-11.31 (raw "!"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @8.1-8.6 (ident "main!"))
		(e-lambda @8.9-12.2
			(args
				(p-underscore @8.10-8.11))
			(e-block @8.13-12.2
				(s-var @9.2-9.16
					(p-assign @9.2-9.6 (ident "name"))
					(e-str @9.9-9.16
						(e-literal @9.10-9.15 (string "World"))))
				(e-call @11.2-11.33
					(e-lookup-external
						(ext-decl (ident "pf.Stdout.line!") (kind "value")))
					(e-str @11.15-11.32
						(e-literal @11.16-11.23 (string "Hello, "))
						(e-lookup-local @11.25-11.29
							(p-assign @9.2-9.6 (ident "name")))
						(e-literal @11.30-11.31 (string "!")))))))
	(s-import @6.1-6.17 (module "pf.Stdout") (qualifier "pf")
		(exposes))
	(ext-decl (ident "pf.Stdout.line!") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-12.2 (type "* -> *")))
	(expressions
		(expr (type "* -> *"))))
~~~
