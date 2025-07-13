# META
~~~ini
description=Import with exposing clause using aliases
type=file
~~~
# SOURCE
~~~roc
module [main]

import json.Json exposing [decode as fromJson, encode as toJson]

main = {
	data = { name: "Bob", age: 25 }
	encoded = toJson(data)
	decoded = fromJson(encoded)
	decoded
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),KwExposing(3:18-3:26),OpenSquare(3:27-3:28),LowerIdent(3:28-3:34),KwAs(3:35-3:37),LowerIdent(3:38-3:46),Comma(3:46-3:47),LowerIdent(3:48-3:54),KwAs(3:55-3:57),LowerIdent(3:58-3:64),CloseSquare(3:64-3:65),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),OpenCurly(5:8-5:9),
LowerIdent(6:2-6:6),OpAssign(6:7-6:8),OpenCurly(6:9-6:10),LowerIdent(6:11-6:15),OpColon(6:15-6:16),StringStart(6:17-6:18),StringPart(6:18-6:21),StringEnd(6:21-6:22),Comma(6:22-6:23),LowerIdent(6:24-6:27),OpColon(6:27-6:28),Int(6:29-6:31),CloseCurly(6:32-6:33),
LowerIdent(7:2-7:9),OpAssign(7:10-7:11),LowerIdent(7:12-7:18),NoSpaceOpenRound(7:18-7:19),LowerIdent(7:19-7:23),CloseRound(7:23-7:24),
LowerIdent(8:2-8:9),OpAssign(8:10-8:11),LowerIdent(8:12-8:20),NoSpaceOpenRound(8:20-8:21),LowerIdent(8:21-8:28),CloseRound(8:28-8:29),
LowerIdent(9:2-9:9),
CloseCurly(10:1-10:2),EndOfFile(10:2-10:2),
~~~
# PARSE
~~~clojure
(file @1.1-10.2
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident @1.9-1.13 (text "main"))))
	(statements
		(s-import @3.1-3.65 (raw "json.Json")
			(exposing
				(exposed-lower-ident @3.28-3.46 (text "decode") (as "fromJson"))
				(exposed-lower-ident @3.48-3.64 (text "encode") (as "toJson"))))
		(s-decl @5.1-10.2
			(p-ident @5.1-5.5 (raw "main"))
			(e-block @5.8-10.2
				(statements
					(s-decl @6.2-6.33
						(p-ident @6.2-6.6 (raw "data"))
						(e-record @6.9-6.33
							(field (field "name")
								(e-string @6.17-6.22
									(e-string-part @6.18-6.21 (raw "Bob"))))
							(field (field "age")
								(e-int @6.29-6.31 (raw "25")))))
					(s-decl @7.2-7.24
						(p-ident @7.2-7.9 (raw "encoded"))
						(e-apply @7.12-7.24
							(e-ident @7.12-7.18 (raw "toJson"))
							(e-ident @7.19-7.23 (raw "data"))))
					(s-decl @8.2-8.29
						(p-ident @8.2-8.9 (raw "decoded"))
						(e-apply @8.12-8.29
							(e-ident @8.12-8.20 (raw "fromJson"))
							(e-ident @8.21-8.28 (raw "encoded"))))
					(e-ident @9.2-9.9 (raw "decoded")))))))
~~~
# FORMATTED
~~~roc
module [main]

import json.Json exposing [decode as fromJson, encode as toJson]

main = {
	data = {name: "Bob", age: 25}
	encoded = toJson(data)
	decoded = fromJson(encoded)
	decoded
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.5 (ident "main"))
		(e-block @5.8-10.2
			(s-let @6.2-6.33
				(p-assign @6.2-6.6 (ident "data"))
				(e-record @6.9-6.33
					(fields
						(field (name "name")
							(e-string @6.17-6.22
								(e-literal @6.18-6.21 (string "Bob"))))
						(field (name "age")
							(e-int @6.29-6.31 (value "25"))))))
			(s-let @7.2-7.24
				(p-assign @7.2-7.9 (ident "encoded"))
				(e-call @7.12-7.24
					(e-lookup-external @7.12-7.18
						(module-idx "0")
						(target-node-idx "0"))
					(e-lookup-local @7.19-7.23
						(p-assign @6.2-6.6 (ident "data")))))
			(s-let @8.2-8.29
				(p-assign @8.2-8.9 (ident "decoded"))
				(e-call @8.12-8.29
					(e-lookup-external @8.12-8.20
						(module-idx "0")
						(target-node-idx "0"))
					(e-lookup-local @8.21-8.28
						(p-assign @7.2-7.9 (ident "encoded")))))
			(e-lookup-local @9.2-9.9
				(p-assign @8.2-8.9 (ident "decoded")))))
	(s-import @3.1-3.65 (module "json.Json") (qualifier "json")
		(exposes
			(exposed (name "decode") (alias "fromJson") (wildcard false))
			(exposed (name "encode") (alias "toJson") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "a")))
	(expressions
		(expr @5.8-10.2 (type "a"))))
~~~
