# META
~~~ini
description=Test multi-level qualified imports and type annotations
type=snippet
~~~
# SOURCE
~~~roc
import json.Core.Utf8 exposing [Encoder]

json_encoder : Encoder
json_encoder = Json.Core.Utf8.defaultEncoder

# Test with qualified type in annotation
process : json.Core.Utf8.Encoder -> Str
process = |encoder| "processing"

# Test with multiple qualifiers
data : json.Core.Utf8.EncodedData
data = json.Core.Utf8.encode("hello")
~~~
# EXPECTED
PARSE ERROR - multi_qualified_import.md:12:12:12:17
PARSE ERROR - multi_qualified_import.md:12:17:12:22
PARSE ERROR - multi_qualified_import.md:12:22:12:29
PARSE ERROR - multi_qualified_import.md:12:29:12:30
PARSE ERROR - multi_qualified_import.md:12:30:12:31
PARSE ERROR - multi_qualified_import.md:12:31:12:36
PARSE ERROR - multi_qualified_import.md:12:36:12:37
PARSE ERROR - multi_qualified_import.md:12:37:12:38
UNDECLARED TYPE - multi_qualified_import.md:3:16:3:23
DOES NOT EXIST - multi_qualified_import.md:4:16:4:45
MODULE NOT IMPORTED - multi_qualified_import.md:7:11:7:33
UNUSED VARIABLE - multi_qualified_import.md:8:12:8:19
MODULE NOT IMPORTED - multi_qualified_import.md:11:8:11:34
NAME NOT IN SCOPE - multi_qualified_import.md:12:8:12:12
# PROBLEMS

┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  data = json.Core.Utf8.encode("hello")                                     │
 │             ‾‾‾‾‾                                                          │
 └─────────────────────────────────────────── multi_qualified_import.md:12:12 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  data = json.Core.Utf8.encode("hello")                                     │
 │                  ‾‾‾‾‾                                                     │
 └─────────────────────────────────────────── multi_qualified_import.md:12:17 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  data = json.Core.Utf8.encode("hello")                                     │
 │                       ‾‾‾‾‾‾‾                                              │
 └─────────────────────────────────────────── multi_qualified_import.md:12:22 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  data = json.Core.Utf8.encode("hello")                                     │
 │                              ‾                                             │
 └─────────────────────────────────────────── multi_qualified_import.md:12:29 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  data = json.Core.Utf8.encode("hello")                                     │
 │                               ‾                                            │
 └─────────────────────────────────────────── multi_qualified_import.md:12:30 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  data = json.Core.Utf8.encode("hello")                                     │
 │                                ‾‾‾‾‾                                       │
 └─────────────────────────────────────────── multi_qualified_import.md:12:31 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  data = json.Core.Utf8.encode("hello")                                     │
 │                                     ‾                                      │
 └─────────────────────────────────────────── multi_qualified_import.md:12:36 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────┐
│ PARSE ERROR ├─ A parsing error occurred: statement_unexpected_token ────────┐
└┬────────────┘                                                               │
 │                                                                            │
 │  data = json.Core.Utf8.encode("hello")                                     │
 │                                      ‾                                     │
 └─────────────────────────────────────────── multi_qualified_import.md:12:37 ┘

    This is an unexpected parsing error. Please check your syntax.


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Encoder` is not declared in this scope. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  json_encoder : Encoder                                                    │
 │                 ‾‾‾‾‾‾‾                                                    │
 └──────────────────────────────────────────── multi_qualified_import.md:3:16 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `Json.defaultEncoder` does not exist. ────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  json_encoder = Json.Core.Utf8.defaultEncoder                              │
 │                 ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                              │
 └──────────────────────────────────────────── multi_qualified_import.md:4:16 ┘

    `Json` is in scope, but it has no associated `defaultEncoder`.


┌─────────────────────┐
│ MODULE NOT IMPORTED ├─ There is no module with the name `json.Core.Utf8` ───┐
└┬────────────────────┘  imported into this Roc file.                         │
 │                                                                            │
 │  process : json.Core.Utf8.Encoder -> Str                                   │
 │            ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                          │
 └──────────────────────────────────────────── multi_qualified_import.md:7:11 ┘



┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `encoder` is defined here and then never used. ─┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  process = |encoder| "processing"                                          │
 │             ‾‾‾‾‾‾‾                                                        │
 └──────────────────────────────────────────── multi_qualified_import.md:8:12 ┘

    If you don't need this variable, prefix it with an underscore like
    `_encoder` to suppress this warning.


┌─────────────────────┐
│ MODULE NOT IMPORTED ├─ There is no module with the name `json.Core.Utf8` ───┐
└┬────────────────────┘  imported into this Roc file.                         │
 │                                                                            │
 │  data : json.Core.Utf8.EncodedData                                         │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                         │
 └──────────────────────────────────────────── multi_qualified_import.md:11:8 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `json` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  data = json.Core.Utf8.encode("hello")                                     │
 │         ‾‾‾‾                                                               │
 └──────────────────────────────────────────── multi_qualified_import.md:12:8 ┘

    Is it misspelled, or is there an import missing?

# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "json.Core.Utf8")
			(exposing
				(exposed-upper-ident (text "Encoder"))))
		(s-type-anno (name "json_encoder")
			(ty (name "Encoder")))
		(s-decl
			(p-ident (raw "json_encoder"))
			(e-ident (raw "Json.Core.Utf8.defaultEncoder")))
		(s-type-anno (name "process")
			(ty-fn
				(ty (name "json.Core.Utf8.Encoder"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "process"))
			(e-lambda
				(args
					(p-ident (raw "encoder")))
				(e-string
					(e-string-part (raw "processing")))))
		(s-type-anno (name "data")
			(ty (name "json.Core.Utf8.EncodedData")))
		(s-decl
			(p-ident (raw "data"))
			(e-ident (raw "json")))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
import json.Core.Utf8 exposing [Encoder]

json_encoder : Encoder
json_encoder = Json.Core.Utf8.defaultEncoder

# Test with qualified type in annotation
process : json.Core.Utf8.Encoder -> Str
process = |encoder| "processing"

# Test with multiple qualifiers
data : json.Core.Utf8.EncodedData
data = json
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "json_encoder"))
		(e-runtime-error (tag "nested_value_not_found"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "process"))
		(e-lambda
			(args
				(p-assign (ident "encoder")))
			(e-string
				(e-literal (string "processing"))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "data"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation
			(ty-malformed)))
	(s-import (module "json.Core")
		(exposes
			(exposed (name "Encoder") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error -> Str"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error -> Str"))
		(expr (type "Error"))))
~~~
