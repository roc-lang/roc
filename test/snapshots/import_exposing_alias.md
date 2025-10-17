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
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,KwAs,LowerIdent,Comma,LowerIdent,KwAs,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "json.Json")
			(exposing
				(exposed-lower-ident
					(text "decode")
					(as "fromJson"))
				(exposed-lower-ident
					(text "encode")
					(as "toJson"))))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "data"))
						(e-record
							(field (field "name")
								(e-string
									(e-string-part (raw "Bob"))))
							(field (field "age")
								(e-int (raw "25")))))
					(s-decl
						(p-ident (raw "encoded"))
						(e-apply
							(e-ident (raw "toJson"))
							(e-ident (raw "data"))))
					(s-decl
						(p-ident (raw "decoded"))
						(e-apply
							(e-ident (raw "fromJson"))
							(e-ident (raw "encoded"))))
					(e-ident (raw "decoded")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "data"))
				(e-record
					(fields
						(field (name "name")
							(e-string
								(e-literal (string "Bob"))))
						(field (name "age")
							(e-num (value "25"))))))
			(s-let
				(p-assign (ident "encoded"))
				(e-call
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-lookup-local
						(p-assign (ident "data")))))
			(s-let
				(p-assign (ident "decoded"))
				(e-call
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-lookup-local
						(p-assign (ident "encoded")))))
			(e-lookup-local
				(p-assign (ident "decoded")))))
	(s-import (module "json.Json") (qualifier "json")
		(exposes
			(exposed (name "decode") (alias "fromJson") (wildcard false))
			(exposed (name "encode") (alias "toJson") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a")))
	(expressions
		(expr (type "_a"))))
~~~
