# META
~~~ini
description=External declaration lookup from json module
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout
import json.Json

main! = |_| {
    # This should create an external declaration for json.Json.utf8
    result = Json.utf8("Hello from external module!")
    Stdout.line!(result)
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:10),NoSpaceDotUpperIdent(3:10-3:17),Newline(1:1-1:1),
KwImport(4:1-4:7),LowerIdent(4:8-4:12),NoSpaceDotUpperIdent(4:12-4:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),Newline(1:1-1:1),
Newline(7:6-7:68),
LowerIdent(8:5-8:11),OpAssign(8:12-8:13),UpperIdent(8:14-8:18),NoSpaceDotLowerIdent(8:18-8:23),NoSpaceOpenRound(8:23-8:24),StringStart(8:24-8:25),StringPart(8:25-8:52),StringEnd(8:52-8:53),CloseRound(8:53-8:54),Newline(1:1-1:1),
UpperIdent(9:5-9:11),NoSpaceDotLowerIdent(9:11-9:17),NoSpaceOpenRound(9:17-9:18),LowerIdent(9:18-9:24),CloseRound(9:24-9:25),Newline(1:1-1:1),
CloseCurly(10:1-10:2),EndOfFile(10:2-10:2),
~~~
# PARSE
~~~clojure
(file @1-1-10-2
	(app @1-1-1-57
		(provides @1-6-1-12
			(exposed-lower-ident (text "main!")))
		(record-field @1-15-1-57 (name "pf")
			(e-string @1-28-1-55
				(e-string-part @1-29-1-54 (raw "../basic-cli/platform.roc"))))
		(packages @1-13-1-57
			(record-field @1-15-1-57 (name "pf")
				(e-string @1-28-1-55
					(e-string-part @1-29-1-54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-import @3-1-3-17 (module ".Stdout") (qualifier "pf"))
		(s-import @4-1-4-17 (module ".Json") (qualifier "json"))
		(s-decl @6-1-10-2
			(p-ident @6-1-6-6 (raw "main!"))
			(e-lambda @6-9-10-2
				(args
					(p-underscore))
				(e-block @6-13-10-2
					(statements
						(s-decl @8-5-8-54
							(p-ident @8-5-8-11 (raw "result"))
							(e-apply @8-14-8-54
								(e-ident @8-14-8-23 (qaul "Json") (raw ".utf8"))
								(e-string @8-24-8-53
									(e-string-part @8-25-8-52 (raw "Hello from external module!")))))
						(e-apply @9-5-9-25
							(e-ident @9-5-9-17 (qaul "Stdout") (raw ".line!"))
							(e-ident @9-18-9-24 (qaul "") (raw "result")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout
import json.Json

main! = |_| {
	# This should create an external declaration for json.Json.utf8
	result = Json.utf8("Hello from external module!")
	Stdout.line!(result)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 89)
		(p-assign @6-1-6-6 (ident "main!") (id 74))
		(e-lambda @6-9-10-2 (id 88)
			(args
				(p-underscore @6-10-6-11 (id 75)))
			(e-block @6-13-10-2
				(s-let @8-5-8-54
					(p-assign @8-5-8-11 (ident "result") (id 76))
					(e-call @8-14-8-54 (id 81)
						(e-lookup-external
							(ext-decl @8-14-8-23 (qualified "json.Json.utf8") (module "json.Json") (local "utf8") (kind "value") (type-var 77)))
						(e-string @8-24-8-53
							(e-literal @8-25-8-52 (string "Hello from external module!")))))
				(e-call @9-5-9-25
					(e-lookup-external
						(ext-decl @9-5-9-17 (qualified "pf.Stdout.line!") (module "pf.Stdout") (local "line!") (kind "value") (type-var 83)))
					(e-lookup-local @9-18-9-24
						(pattern (id 76)))))))
	(s-import @3-1-3-17 (module "pf.Stdout") (id 72)
		(exposes))
	(s-import @4-1-4-17 (module "json.Json") (id 73)
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "main!") (type "*")))
	(expressions
		(expr @6-9-10-2 (type "*"))))
~~~