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
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
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
					(text "decode"))
				(exposed-lower-ident
					(text "encode"))))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "data"))
						(e-record
							(field (field "name")
								(e-string
									(e-string-part (raw "Alice"))))
							(field (field "age")
								(e-int (raw "30")))))
					(s-decl
						(p-ident (raw "encoded"))
						(e-apply
							(e-ident (raw "encode"))
							(e-ident (raw "data"))))
					(s-decl
						(p-ident (raw "decoded"))
						(e-apply
							(e-ident (raw "decode"))
							(e-ident (raw "encoded"))))
					(e-ident (raw "decoded")))))))
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
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "data"))
				(e-record
					(fields
						(field (name "name")
							(e-string
								(e-literal (string "Alice"))))
						(field (name "age")
							(e-num (value "30"))))))
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
	(s-import (module "json.Json")
		(exposes
			(exposed (name "decode") (wildcard false))
			(exposed (name "encode") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a")))
	(expressions
		(expr (type "_a"))))
~~~
