# META
~~~ini
description=Effectful function type with record parameter
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

import pf.Stdout

printName : { name: Str, age: U64 } => Str
printName = |person| {
    Stdout.line!(person.name)
    person.name
}
main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:10),NoSpaceDotUpperIdent(3:10-3:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:10),OpColon(5:11-5:12),OpenCurly(5:13-5:14),LowerIdent(5:15-5:19),OpColon(5:19-5:20),UpperIdent(5:21-5:24),Comma(5:24-5:25),LowerIdent(5:26-5:29),OpColon(5:29-5:30),UpperIdent(5:31-5:34),CloseCurly(5:35-5:36),OpFatArrow(5:37-5:39),UpperIdent(5:40-5:43),Newline(1:1-1:1),
LowerIdent(6:1-6:10),OpAssign(6:11-6:12),OpBar(6:13-6:14),LowerIdent(6:14-6:20),OpBar(6:20-6:21),OpenCurly(6:22-6:23),Newline(1:1-1:1),
UpperIdent(7:5-7:11),NoSpaceDotLowerIdent(7:11-7:17),NoSpaceOpenRound(7:17-7:18),LowerIdent(7:18-7:24),NoSpaceDotLowerIdent(7:24-7:29),CloseRound(7:29-7:30),Newline(1:1-1:1),
LowerIdent(8:5-8:11),NoSpaceDotLowerIdent(8:11-8:16),Newline(1:1-1:1),
CloseCurly(9:1-9:2),Newline(1:1-1:1),
LowerIdent(10:1-10:6),OpAssign(10:7-10:8),OpBar(10:9-10:10),Underscore(10:10-10:11),OpBar(10:11-10:12),OpenCurly(10:13-10:14),CloseCurly(10:14-10:15),EndOfFile(10:15-10:15),
~~~
# PARSE
~~~clojure
(file @1.1-10.15
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
		(s-import @3.1-3.17 (raw "pf.Stdout"))
		(s-type-anno @1.1-1.1 (name "printName")
			(ty-fn @5.13-5.43
				(ty-record @5.13-5.36
					(anno-record-field @5.15-5.25 (name "name")
						(ty @5.21-5.24 (name "Str")))
					(anno-record-field @5.26-5.36 (name "age")
						(ty @5.31-5.34 (name "U64"))))
				(ty @5.40-5.43 (name "Str"))))
		(s-decl @6.1-9.2
			(p-ident @6.1-6.10 (raw "printName"))
			(e-lambda @6.13-9.2
				(args
					(p-ident @6.14-6.20 (raw "person")))
				(e-block @6.22-9.2
					(statements
						(e-apply @7.5-7.30
							(e-ident @7.5-7.17 (raw "Stdout.line!"))
							(e-field-access @7.18-7.30
								(e-ident @7.18-7.24 (raw "person"))
								(e-ident @7.24-7.29 (raw "name"))))
						(e-field-access @8.5-9.2
							(e-ident @8.5-8.11 (raw "person"))
							(e-ident @8.11-8.16 (raw "name")))))))
		(s-decl @10.1-10.15
			(p-ident @10.1-10.6 (raw "main!"))
			(e-lambda @10.9-10.15
				(args
					(p-underscore))
				(e-record @10.13-10.15)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

import pf.Stdout

printName : { name : Str, age : U64 } => Str
printName = |person| {
	Stdout.line!(person.name)
	person.name
}
main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.10 (ident "printName"))
		(e-lambda @6.13-9.2
			(args
				(p-assign @6.14-6.20 (ident "person")))
			(e-block @6.22-9.2
				(s-expr @7.5-8.11
					(e-call @7.5-7.30
						(e-lookup-external @7.5-7.17
							(module-idx "0")
							(target-node-idx "0"))
						(e-dot-access @7.18-7.30 (field "name")
							(receiver
								(e-lookup-local @7.18-7.24
									(pattern @6.14-6.20))))))
				(e-dot-access @8.5-9.2 (field "name")
					(receiver
						(e-lookup-local @8.5-8.11
							(pattern @6.14-6.20))))))
		(annotation @6.1-6.10
			(declared-type
				(ty-fn @5.13-5.43 (effectful true)
					(ty-record @5.13-5.36
						(field (field "name")
							(ty @5.21-5.24 (name "Str")))
						(field (field "age")
							(ty @5.31-5.34 (name "U64"))))
					(ty @5.40-5.43 (name "Str"))))))
	(d-let
		(p-assign @10.1-10.6 (ident "main!"))
		(e-lambda @10.9-10.15
			(args
				(p-underscore @10.10-10.11))
			(e-empty_record @10.13-10.15)))
	(s-import @3.1-3.17 (module "pf.Stdout") (qualifier "pf")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.10 (type "{ name: Str, age: U64 } => Str"))
		(patt @10.1-10.6 (type "* -> {}")))
	(expressions
		(expr @6.13-9.2 (type "{ name: Str, age: U64 } => Str"))
		(expr @10.9-10.15 (type "* -> {}"))))
~~~
