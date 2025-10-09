# META
~~~ini
description=Import with exposing clause using aliases
type=snippet
~~~
# SOURCE
~~~roc
import json.Json exposing [decode as fromJson, encode as toJson]

main = {
	data = { name: "Bob", age: 25 }
	encoded = toJson(data)
	decoded = fromJson(encoded)
	decoded
}
~~~
# EXPECTED
MODULE NOT FOUND - import_exposing_alias.md:1:1:1:65
UNDEFINED VARIABLE - import_exposing_alias.md:5:12:5:18
UNDEFINED VARIABLE - import_exposing_alias.md:6:12:6:20
# PROBLEMS
**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**import_exposing_alias.md:1:1:1:65:**
```roc
import json.Json exposing [decode as fromJson, encode as toJson]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toJson` in this scope.
Is there an `import` or `exposing` missing up-top?

**import_exposing_alias.md:5:12:5:18:**
```roc
	encoded = toJson(data)
```
	          ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `fromJson` in this scope.
Is there an `import` or `exposing` missing up-top?

**import_exposing_alias.md:6:12:6:20:**
```roc
	decoded = fromJson(encoded)
```
	          ^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:17),KwExposing(1:18-1:26),OpenSquare(1:27-1:28),LowerIdent(1:28-1:34),KwAs(1:35-1:37),LowerIdent(1:38-1:46),Comma(1:46-1:47),LowerIdent(1:48-1:54),KwAs(1:55-1:57),LowerIdent(1:58-1:64),CloseSquare(1:64-1:65),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),OpenCurly(3:8-3:9),
LowerIdent(4:2-4:6),OpAssign(4:7-4:8),OpenCurly(4:9-4:10),LowerIdent(4:11-4:15),OpColon(4:15-4:16),StringStart(4:17-4:18),StringPart(4:18-4:21),StringEnd(4:21-4:22),Comma(4:22-4:23),LowerIdent(4:24-4:27),OpColon(4:27-4:28),Int(4:29-4:31),CloseCurly(4:32-4:33),
LowerIdent(5:2-5:9),OpAssign(5:10-5:11),LowerIdent(5:12-5:18),NoSpaceOpenRound(5:18-5:19),LowerIdent(5:19-5:23),CloseRound(5:23-5:24),
LowerIdent(6:2-6:9),OpAssign(6:10-6:11),LowerIdent(6:12-6:20),NoSpaceOpenRound(6:20-6:21),LowerIdent(6:21-6:28),CloseRound(6:28-6:29),
LowerIdent(7:2-7:9),
CloseCurly(8:1-8:2),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @1.1-8.2
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.65 (raw "json.Json")
			(exposing
				(exposed-lower-ident @1.28-1.46
					(text "decode")
					(as "fromJson"))
				(exposed-lower-ident @1.48-1.64
					(text "encode")
					(as "toJson"))))
		(s-decl @3.1-8.2
			(p-ident @3.1-3.5 (raw "main"))
			(e-block @3.8-8.2
				(statements
					(s-decl @4.2-4.33
						(p-ident @4.2-4.6 (raw "data"))
						(e-record @4.9-4.33
							(field (field "name")
								(e-string @4.17-4.22
									(e-string-part @4.18-4.21 (raw "Bob"))))
							(field (field "age")
								(e-int @4.29-4.31 (raw "25")))))
					(s-decl @5.2-5.24
						(p-ident @5.2-5.9 (raw "encoded"))
						(e-apply @5.12-5.24
							(e-ident @5.12-5.18 (raw "toJson"))
							(e-ident @5.19-5.23 (raw "data"))))
					(s-decl @6.2-6.29
						(p-ident @6.2-6.9 (raw "decoded"))
						(e-apply @6.12-6.29
							(e-ident @6.12-6.20 (raw "fromJson"))
							(e-ident @6.21-6.28 (raw "encoded"))))
					(e-ident @7.2-7.9 (raw "decoded")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.5 (ident "main"))
		(e-block @3.8-8.2
			(s-let @4.2-4.33
				(p-assign @4.2-4.6 (ident "data"))
				(e-record @4.9-4.33
					(fields
						(field (name "name")
							(e-string @4.17-4.22
								(e-literal @4.18-4.21 (string "Bob"))))
						(field (name "age")
							(e-num @4.29-4.31 (value "25"))))))
			(s-let @5.2-5.24
				(p-assign @5.2-5.9 (ident "encoded"))
				(e-call @5.12-5.24
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-lookup-local @5.19-5.23
						(p-assign @4.2-4.6 (ident "data")))))
			(s-let @6.2-6.29
				(p-assign @6.2-6.9 (ident "decoded"))
				(e-call @6.12-6.29
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-lookup-local @6.21-6.28
						(p-assign @5.2-5.9 (ident "encoded")))))
			(e-lookup-local @7.2-7.9
				(p-assign @6.2-6.9 (ident "decoded")))))
	(s-import @1.1-1.65 (module "json.Json") (qualifier "json")
		(exposes
			(exposed (name "decode") (alias "fromJson") (wildcard false))
			(exposed (name "encode") (alias "toJson") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "_a")))
	(expressions
		(expr @3.8-8.2 (type "_a"))))
~~~
