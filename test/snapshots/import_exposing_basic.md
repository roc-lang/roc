# META
~~~ini
description=Import with exposing clause and usage of exposed items
type=file
~~~
# SOURCE
~~~roc
module [main]

import json.Json exposing [decode, encode]

main = {
    data = { name: "Alice", age: 30 }
    encoded = encode(data)
    decoded = decode(encoded)
    decoded
}
~~~
# EXPECTED
MODULE NOT FOUND - import_exposing_basic.md:3:1:3:43
# PROBLEMS
**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**import_exposing_basic.md:3:1:3:43:**
```roc
import json.Json exposing [decode, encode]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),KwExposing(3:18-3:26),OpenSquare(3:27-3:28),LowerIdent(3:28-3:34),Comma(3:34-3:35),LowerIdent(3:36-3:42),CloseSquare(3:42-3:43),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),OpenCurly(5:8-5:9),
LowerIdent(6:5-6:9),OpAssign(6:10-6:11),OpenCurly(6:12-6:13),LowerIdent(6:14-6:18),OpColon(6:18-6:19),StringStart(6:20-6:21),StringPart(6:21-6:26),StringEnd(6:26-6:27),Comma(6:27-6:28),LowerIdent(6:29-6:32),OpColon(6:32-6:33),Int(6:34-6:36),CloseCurly(6:37-6:38),
LowerIdent(7:5-7:12),OpAssign(7:13-7:14),LowerIdent(7:15-7:21),NoSpaceOpenRound(7:21-7:22),LowerIdent(7:22-7:26),CloseRound(7:26-7:27),
LowerIdent(8:5-8:12),OpAssign(8:13-8:14),LowerIdent(8:15-8:21),NoSpaceOpenRound(8:21-8:22),LowerIdent(8:22-8:29),CloseRound(8:29-8:30),
LowerIdent(9:5-9:12),
CloseCurly(10:1-10:2),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(file @1.1-10.2
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident @1.9-1.13
				(text "main"))))
	(statements
		(s-import @3.1-3.43 (raw "json.Json")
			(exposing
				(exposed-lower-ident @3.28-3.34
					(text "decode"))
				(exposed-lower-ident @3.36-3.42
					(text "encode"))))
		(s-decl @5.1-10.2
			(p-ident @5.1-5.5 (raw "main"))
			(e-block @5.8-10.2
				(statements
					(s-decl @6.5-6.38
						(p-ident @6.5-6.9 (raw "data"))
						(e-record @6.12-6.38
							(field (field "name")
								(e-string @6.20-6.27
									(e-string-part @6.21-6.26 (raw "Alice"))))
							(field (field "age")
								(e-int @6.34-6.36 (raw "30")))))
					(s-decl @7.5-7.27
						(p-ident @7.5-7.12 (raw "encoded"))
						(e-apply @7.15-7.27
							(e-ident @7.15-7.21 (raw "encode"))
							(e-ident @7.22-7.26 (raw "data"))))
					(s-decl @8.5-8.30
						(p-ident @8.5-8.12 (raw "decoded"))
						(e-apply @8.15-8.30
							(e-ident @8.15-8.21 (raw "decode"))
							(e-ident @8.22-8.29 (raw "encoded"))))
					(e-ident @9.5-9.12 (raw "decoded")))))))
~~~
# FORMATTED
~~~roc
module [main]

import json.Json exposing [decode, encode]

main = {
	data = { name: "Alice", age: 30 }
	encoded = encode(data)
	decoded = decode(encoded)
	decoded
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.5 (ident "main"))
		(e-block @5.8-10.2
			(s-let @6.5-6.38
				(p-assign @6.5-6.9 (ident "data"))
				(e-record @6.12-6.38
					(fields
						(field (name "name")
							(e-string @6.20-6.27
								(e-literal @6.21-6.26 (string "Alice"))))
						(field (name "age")
							(e-int @6.34-6.36 (value "30"))))))
			(s-let @7.5-7.27
				(p-assign @7.5-7.12 (ident "encoded"))
				(e-call @7.15-7.27
					(e-lookup-external @7.15-7.21
						(module-idx "0")
						(target-node-idx "0"))
					(e-lookup-local @7.22-7.26
						(p-assign @6.5-6.9 (ident "data")))))
			(s-let @8.5-8.30
				(p-assign @8.5-8.12 (ident "decoded"))
				(e-call @8.15-8.30
					(e-lookup-external @8.15-8.21
						(module-idx "0")
						(target-node-idx "0"))
					(e-lookup-local @8.22-8.29
						(p-assign @7.5-7.12 (ident "encoded")))))
			(e-lookup-local @9.5-9.12
				(p-assign @8.5-8.12 (ident "decoded")))))
	(s-import @3.1-3.43 (module "json.Json") (qualifier "json")
		(exposes
			(exposed (name "decode") (wildcard false))
			(exposed (name "encode") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "_a")))
	(expressions
		(expr @5.8-10.2 (type "_a"))))
~~~
