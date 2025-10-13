# META
~~~ini
description=Import with exposing clause and usage of exposed items
type=snippet
~~~
# SOURCE
~~~roc
import json.Json exposing [decode, encode]

main = {
    data = { name: "Alice", age: 30 }
    encoded = encode(data)
    decoded = decode(encoded)
    decoded
}
~~~
# EXPECTED
MODULE NOT FOUND - import_exposing_basic.md:1:1:1:43
UNDEFINED VARIABLE - import_exposing_basic.md:5:15:5:21
UNDEFINED VARIABLE - import_exposing_basic.md:6:15:6:21
# PROBLEMS
**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**import_exposing_basic.md:1:1:1:43:**
```roc
import json.Json exposing [decode, encode]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `encode` in this scope.
Is there an `import` or `exposing` missing up-top?

**import_exposing_basic.md:5:15:5:21:**
```roc
    encoded = encode(data)
```
              ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `decode` in this scope.
Is there an `import` or `exposing` missing up-top?

**import_exposing_basic.md:6:15:6:21:**
```roc
    decoded = decode(encoded)
```
              ^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:17),KwExposing(1:18-1:26),OpenSquare(1:27-1:28),LowerIdent(1:28-1:34),Comma(1:34-1:35),LowerIdent(1:36-1:42),CloseSquare(1:42-1:43),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),OpenCurly(3:8-3:9),
LowerIdent(4:5-4:9),OpAssign(4:10-4:11),OpenCurly(4:12-4:13),LowerIdent(4:14-4:18),OpColon(4:18-4:19),StringStart(4:20-4:21),StringPart(4:21-4:26),StringEnd(4:26-4:27),Comma(4:27-4:28),LowerIdent(4:29-4:32),OpColon(4:32-4:33),Int(4:34-4:36),CloseCurly(4:37-4:38),
LowerIdent(5:5-5:12),OpAssign(5:13-5:14),LowerIdent(5:15-5:21),NoSpaceOpenRound(5:21-5:22),LowerIdent(5:22-5:26),CloseRound(5:26-5:27),
LowerIdent(6:5-6:12),OpAssign(6:13-6:14),LowerIdent(6:15-6:21),NoSpaceOpenRound(6:21-6:22),LowerIdent(6:22-6:29),CloseRound(6:29-6:30),
LowerIdent(7:5-7:12),
CloseCurly(8:1-8:2),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @1.1-8.2
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.43 (raw "json.Json")
			(exposing
				(exposed-lower-ident @1.28-1.34
					(text "decode"))
				(exposed-lower-ident @1.36-1.42
					(text "encode"))))
		(s-decl @3.1-8.2
			(p-ident @3.1-3.5 (raw "main"))
			(e-block @3.8-8.2
				(statements
					(s-decl @4.5-4.38
						(p-ident @4.5-4.9 (raw "data"))
						(e-record @4.12-4.38
							(field (field "name")
								(e-string @4.20-4.27
									(e-string-part @4.21-4.26 (raw "Alice"))))
							(field (field "age")
								(e-int @4.34-4.36 (raw "30")))))
					(s-decl @5.5-5.27
						(p-ident @5.5-5.12 (raw "encoded"))
						(e-apply @5.15-5.27
							(e-ident @5.15-5.21 (raw "encode"))
							(e-ident @5.22-5.26 (raw "data"))))
					(s-decl @6.5-6.30
						(p-ident @6.5-6.12 (raw "decoded"))
						(e-apply @6.15-6.30
							(e-ident @6.15-6.21 (raw "decode"))
							(e-ident @6.22-6.29 (raw "encoded"))))
					(e-ident @7.5-7.12 (raw "decoded")))))))
~~~
# FORMATTED
~~~roc
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
		(p-assign @3.1-3.5 (ident "main"))
		(e-block @3.8-8.2
			(s-let @4.5-4.38
				(p-assign @4.5-4.9 (ident "data"))
				(e-record @4.12-4.38
					(fields
						(field (name "name")
							(e-string @4.20-4.27
								(e-literal @4.21-4.26 (string "Alice"))))
						(field (name "age")
							(e-num @4.34-4.36 (value "30"))))))
			(s-let @5.5-5.27
				(p-assign @5.5-5.12 (ident "encoded"))
				(e-call @5.15-5.27
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-lookup-local @5.22-5.26
						(p-assign @4.5-4.9 (ident "data")))))
			(s-let @6.5-6.30
				(p-assign @6.5-6.12 (ident "decoded"))
				(e-call @6.15-6.30
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-lookup-local @6.22-6.29
						(p-assign @5.5-5.12 (ident "encoded")))))
			(e-lookup-local @7.5-7.12
				(p-assign @6.5-6.12 (ident "decoded")))))
	(s-import @1.1-1.43 (module "json.Json")
		(exposes
			(exposed (name "decode") (wildcard false))
			(exposed (name "encode") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "_a")))
	(expressions
		(expr @3.8-8.2 (type "_a"))))
~~~
