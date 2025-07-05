# META
~~~ini
description=Pure function with pure annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Function with pure annotation using thin arrow
add : I32, I32 -> I32
add = |x, y| { x: x, y: y }.x

# Another pure function that calls a pure function
double : I32 -> I32
double = |x| add(x, x)

main! = add(1, 2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:49),
LowerIdent(4:1-4:4),OpColon(4:5-4:6),UpperIdent(4:7-4:10),Comma(4:10-4:11),UpperIdent(4:12-4:15),OpArrow(4:16-4:18),UpperIdent(4:19-4:22),Newline(1:1-1:1),
LowerIdent(5:1-5:4),OpAssign(5:5-5:6),OpBar(5:7-5:8),LowerIdent(5:8-5:9),Comma(5:9-5:10),LowerIdent(5:11-5:12),OpBar(5:12-5:13),OpenCurly(5:14-5:15),LowerIdent(5:16-5:17),OpColon(5:17-5:18),LowerIdent(5:19-5:20),Comma(5:20-5:21),LowerIdent(5:22-5:23),OpColon(5:23-5:24),LowerIdent(5:25-5:26),CloseCurly(5:27-5:28),NoSpaceDotLowerIdent(5:28-5:30),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(7:2-7:51),
LowerIdent(8:1-8:7),OpColon(8:8-8:9),UpperIdent(8:10-8:13),OpArrow(8:14-8:16),UpperIdent(8:17-8:20),Newline(1:1-1:1),
LowerIdent(9:1-9:7),OpAssign(9:8-9:9),OpBar(9:10-9:11),LowerIdent(9:11-9:12),OpBar(9:12-9:13),LowerIdent(9:14-9:17),NoSpaceOpenRound(9:17-9:18),LowerIdent(9:18-9:19),Comma(9:19-9:20),LowerIdent(9:21-9:22),CloseRound(9:22-9:23),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(11:1-11:6),OpAssign(11:7-11:8),LowerIdent(11:9-11:12),NoSpaceOpenRound(11:12-11:13),Int(11:13-11:14),Comma(11:14-11:15),Int(11:16-11:17),CloseRound(11:17-11:18),EndOfFile(11:18-11:18),
~~~
# PARSE
~~~clojure
(file @1.1-11.18
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
		(s-type-anno @1.1-1.1 (name "add")
			(ty-fn @4.7-4.22
				(ty @4.7-4.10 (name "I32"))
				(ty @4.12-4.15 (name "I32"))
				(ty @4.19-4.22 (name "I32"))))
		(s-decl @5.1-8.7
			(p-ident @5.1-5.4 (raw "add"))
			(e-lambda @5.7-8.7
				(args
					(p-ident @5.8-5.9 (raw "x"))
					(p-ident @5.11-5.12 (raw "y")))
				(e-field-access @5.14-8.7
					(e-record @5.14-5.28
						(field (field "x") (optional false)
							(e-ident @5.19-5.20 (raw "x")))
						(field (field "y") (optional false)
							(e-ident @5.25-5.26 (raw "y"))))
					(e-ident @5.28-5.30 (raw "x")))))
		(s-type-anno @1.1-1.1 (name "double")
			(ty-fn @8.10-8.20
				(ty @8.10-8.13 (name "I32"))
				(ty @8.17-8.20 (name "I32"))))
		(s-decl @9.1-9.23
			(p-ident @9.1-9.7 (raw "double"))
			(e-lambda @9.10-9.23
				(args
					(p-ident @9.11-9.12 (raw "x")))
				(e-apply @9.14-9.23
					(e-ident @9.14-9.17 (raw "add"))
					(e-ident @9.18-9.19 (raw "x"))
					(e-ident @9.21-9.22 (raw "x")))))
		(s-decl @11.1-11.18
			(p-ident @11.1-11.6 (raw "main!"))
			(e-apply @11.9-11.18
				(e-ident @11.9-11.12 (raw "add"))
				(e-int @11.13-11.14 (raw "1"))
				(e-int @11.16-11.17 (raw "2"))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Function with pure annotation using thin arrow
add : I32, I32 -> I32
add = |x, y| {x: x, y: y}.x

# Another pure function that calls a pure function
double : I32 -> I32
double = |x| add(x, x)

main! = add(1, 2)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.4 (ident "add"))
		(e-lambda @5.7-8.7
			(args
				(p-assign @5.8-5.9 (ident "x"))
				(p-assign @5.11-5.12 (ident "y")))
			(e-dot-access @5.14-8.7 (field "x")
				(receiver
					(e-record @5.14-5.28
						(fields
							(field (name "x")
								(e-lookup-local @5.19-5.20
									(pattern @5.8-5.9)))
							(field (name "y")
								(e-lookup-local @5.25-5.26
									(pattern @5.11-5.12))))))))
		(annotation @5.1-5.4
			(declared-type
				(ty-fn @4.7-4.22 (effectful false)
					(ty @4.7-4.10 (name "I32"))
					(ty @4.12-4.15 (name "I32"))
					(ty @4.19-4.22 (name "I32"))))))
	(d-let
		(p-assign @9.1-9.7 (ident "double"))
		(e-lambda @9.10-9.23
			(args
				(p-assign @9.11-9.12 (ident "x")))
			(e-call @9.14-9.23
				(e-lookup-local @9.14-9.17
					(pattern @5.1-5.4))
				(e-lookup-local @9.18-9.19
					(pattern @9.11-9.12))
				(e-lookup-local @9.21-9.22
					(pattern @9.11-9.12))))
		(annotation @9.1-9.7
			(declared-type
				(ty-fn @8.10-8.20 (effectful false)
					(ty @8.10-8.13 (name "I32"))
					(ty @8.17-8.20 (name "I32"))))))
	(d-let
		(p-assign @11.1-11.6 (ident "main!"))
		(e-call @11.9-11.18
			(e-lookup-local @11.9-11.12
				(pattern @5.1-5.4))
			(e-int @11.13-11.14 (value "1"))
			(e-int @11.16-11.17 (value "2")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.4 (type "I32, I32 -> I32"))
		(patt @9.1-9.7 (type "I32 -> I32"))
		(patt @11.1-11.6 (type "I32")))
	(expressions
		(expr @5.7-8.7 (type "I32, I32 -> I32"))
		(expr @9.10-9.23 (type "I32 -> I32"))
		(expr @11.9-11.18 (type "I32"))))
~~~
