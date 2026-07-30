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
EXPECTED RECORD ACCESSOR - multi_qualified_import.md:12:12:12:17
EXPECTED RECORD ACCESSOR - multi_qualified_import.md:12:17:12:22
UNDECLARED TYPE - multi_qualified_import.md:3:16:3:23
DOES NOT EXIST - multi_qualified_import.md:4:16:4:45
MOD NOT IMPORTED - multi_qualified_import.md:7:11:7:33
UNUSED VARIABLE - multi_qualified_import.md:8:12:8:19
MOD NOT IMPORTED - multi_qualified_import.md:11:8:11:34
UNRECOGNIZED SYNTAX - multi_qualified_import.md:12:8:12:38
# PROBLEMS

┌──────────────────────────┐
│ EXPECTED RECORD ACCESSOR ├─ I was parsing access after `.`, and I ──────────┐
└┬─────────────────────────┘  expected a field name or tuple index.           │
 │                                                                            │
 │  data = json.Core.Utf8.encode("hello")                                     │
 │             ‾‾‾‾‾                                                          │
 └─────────────────────────────────────────── multi_qualified_import.md:12:12 ┘

    Record access uses a lowercase field name like `.name`. Tuple access uses a
    number like `.0`. Uppercase names, malformed names, and a bare `.` are not
    valid accessors.

    For example:
        person.name
        pair.0

    I found `.Core` here.
    Names that start with uppercase letters are used for tags, type names, and
    mod names in Roc.


┌──────────────────────────┐
│ EXPECTED RECORD ACCESSOR ├─ I was parsing access after `.`, and I ──────────┐
└┬─────────────────────────┘  expected a field name or tuple index.           │
 │                                                                            │
 │  data = json.Core.Utf8.encode("hello")                                     │
 │                  ‾‾‾‾‾                                                     │
 └─────────────────────────────────────────── multi_qualified_import.md:12:17 ┘

    Record access uses a lowercase field name like `.name`. Tuple access uses a
    number like `.0`. Uppercase names, malformed names, and a bare `.` are not
    valid accessors.

    For example:
        person.name
        pair.0

    I found `.Utf8` here.
    Names that start with uppercase letters are used for tags, type names, and
    mod names in Roc.


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
│ MOD NOT IMPORTED ├─ There is no mod with the name `json.Core.Utf8` ───┐
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
│ MOD NOT IMPORTED ├─ There is no mod with the name `json.Core.Utf8` ───┐
└┬────────────────────┘  imported into this Roc file.                         │
 │                                                                            │
 │  data : json.Core.Utf8.EncodedData                                         │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                         │
 └──────────────────────────────────────────── multi_qualified_import.md:11:8 ┘



┌─────────────────────┐
│ UNRECOGNIZED SYNTAX ├─ I don't recognize this syntax. ──────────────────────┐
└┬────────────────────┘                                                       │
 │                                                                            │
 │  data = json.Core.Utf8.encode("hello")                                     │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                     │
 └──────────────────────────────────────────── multi_qualified_import.md:12:8 ┘

    This might be a syntax error, an unsupported language feature, or a typo.

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
	(type-mod)
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
			(e-method-call (method ".encode")
				(receiver
					(e-malformed (reason "expr_dot_suffix_not_allowed")))
				(args
					(e-string
						(e-string-part (raw "hello"))))))))
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
data = .encode("hello")
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
		(e-runtime-error (tag "expr_not_canonicalized"))
		(annotation
			(ty-malformed)))
	(s-import (mod "json.Core")
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
