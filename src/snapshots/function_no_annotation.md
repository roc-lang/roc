# META
~~~ini
description=Function with no type annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout

# Pure function with no annotation
multiply = |x, y| x * y

# Function with no type annotation - should infer effectfulness from body
print_number! = |n| Stdout.line!(n)

# Another effectful function with no annotation
process! = |x| print_number!(multiply(x, 2))

main! = process!(42)
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
Newline(5:2-5:35),
LowerIdent(6:1-6:9),OpAssign(6:10-6:11),OpBar(6:12-6:13),LowerIdent(6:13-6:14),Comma(6:14-6:15),LowerIdent(6:16-6:17),OpBar(6:17-6:18),LowerIdent(6:19-6:20),OpStar(6:21-6:22),LowerIdent(6:23-6:24),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(8:2-8:74),
LowerIdent(9:1-9:14),OpAssign(9:15-9:16),OpBar(9:17-9:18),LowerIdent(9:18-9:19),OpBar(9:19-9:20),UpperIdent(9:21-9:27),NoSpaceDotLowerIdent(9:27-9:33),NoSpaceOpenRound(9:33-9:34),LowerIdent(9:34-9:35),CloseRound(9:35-9:36),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(11:2-11:48),
LowerIdent(12:1-12:9),OpAssign(12:10-12:11),OpBar(12:12-12:13),LowerIdent(12:13-12:14),OpBar(12:14-12:15),LowerIdent(12:16-12:29),NoSpaceOpenRound(12:29-12:30),LowerIdent(12:30-12:38),NoSpaceOpenRound(12:38-12:39),LowerIdent(12:39-12:40),Comma(12:40-12:41),Int(12:42-12:43),CloseRound(12:43-12:44),CloseRound(12:44-12:45),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(14:1-14:6),OpAssign(14:7-14:8),LowerIdent(14:9-14:17),NoSpaceOpenRound(14:17-14:18),Int(14:18-14:20),CloseRound(14:20-14:21),EndOfFile(14:21-14:21),
~~~
# PARSE
~~~clojure
(file @1.1-14.21
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
		(s-decl @6.1-9.14
			(p-ident @6.1-6.9 (raw "multiply"))
			(e-lambda @6.12-9.14
				(args
					(p-ident @6.13-6.14 (raw "x"))
					(p-ident @6.16-6.17 (raw "y")))
				(e-binop @6.19-9.14 (op "*")
					(e-ident @6.19-6.20 (raw "x"))
					(e-ident @6.23-6.24 (raw "y")))))
		(s-decl @9.1-9.36
			(p-ident @9.1-9.14 (raw "print_number!"))
			(e-lambda @9.17-9.36
				(args
					(p-ident @9.18-9.19 (raw "n")))
				(e-apply @9.21-9.36
					(e-ident @9.21-9.33 (raw "Stdout.line!"))
					(e-ident @9.34-9.35 (raw "n")))))
		(s-decl @12.1-12.45
			(p-ident @12.1-12.9 (raw "process!"))
			(e-lambda @12.12-12.45
				(args
					(p-ident @12.13-12.14 (raw "x")))
				(e-apply @12.16-12.45
					(e-ident @12.16-12.29 (raw "print_number!"))
					(e-apply @12.30-12.44
						(e-ident @12.30-12.38 (raw "multiply"))
						(e-ident @12.39-12.40 (raw "x"))
						(e-int @12.42-12.43 (raw "2"))))))
		(s-decl @14.1-14.21
			(p-ident @14.1-14.6 (raw "main!"))
			(e-apply @14.9-14.21
				(e-ident @14.9-14.17 (raw "process!"))
				(e-int @14.18-14.20 (raw "42"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.9 (ident "multiply"))
		(e-lambda @6.12-9.14
			(args
				(p-assign @6.13-6.14 (ident "x"))
				(p-assign @6.16-6.17 (ident "y")))
			(e-binop @6.19-9.14 (op "mul")
				(e-lookup-local @6.19-6.20
					(p-assign @6.13-6.14 (ident "x")))
				(e-lookup-local @6.23-6.24
					(p-assign @6.16-6.17 (ident "y"))))))
	(d-let
		(p-assign @9.1-9.14 (ident "print_number!"))
		(e-lambda @9.17-9.36
			(args
				(p-assign @9.18-9.19 (ident "n")))
			(e-call @9.21-9.36
				(e-lookup-external
					(ext-decl @9.21-9.33 (ident "pf.Stdout.line!") (kind "value")))
				(e-lookup-local @9.34-9.35
					(p-assign @9.18-9.19 (ident "n"))))))
	(d-let
		(p-assign @12.1-12.9 (ident "process!"))
		(e-lambda @12.12-12.45
			(args
				(p-assign @12.13-12.14 (ident "x")))
			(e-call @12.16-12.45
				(e-lookup-local @12.16-12.29
					(p-assign @9.1-9.14 (ident "print_number!")))
				(e-call @12.30-12.44
					(e-lookup-local @12.30-12.38
						(p-assign @6.1-6.9 (ident "multiply")))
					(e-lookup-local @12.39-12.40
						(p-assign @12.13-12.14 (ident "x")))
					(e-int @12.42-12.43 (value "2"))))))
	(d-let
		(p-assign @14.1-14.6 (ident "main!"))
		(e-call @14.9-14.21
			(e-lookup-local @14.9-14.17
				(p-assign @12.1-12.9 (ident "process!")))
			(e-int @14.18-14.20 (value "42"))))
	(s-import @3.1-3.17 (module "pf.Stdout") (qualifier "pf")
		(exposes))
	(ext-decl @9.21-9.33 (ident "pf.Stdout.line!") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.9 (type "*, * -> *"))
		(patt @9.1-9.14 (type "* -> *"))
		(patt @12.1-12.9 (type "* -> *"))
		(patt @14.1-14.6 (type "*")))
	(expressions
		(expr @6.12-9.14 (type "*, * -> *"))
		(expr @9.17-9.36 (type "* -> *"))
		(expr @12.12-12.45 (type "* -> *"))
		(expr @14.9-14.21 (type "*"))))
~~~
