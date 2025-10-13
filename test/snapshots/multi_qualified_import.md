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
PARSE ERROR - multi_qualified_import.md:1:17:1:22
PARSE ERROR - multi_qualified_import.md:1:23:1:31
PARSE ERROR - multi_qualified_import.md:1:32:1:33
PARSE ERROR - multi_qualified_import.md:1:40:1:41
PARSE ERROR - multi_qualified_import.md:12:12:12:17
PARSE ERROR - multi_qualified_import.md:12:17:12:22
PARSE ERROR - multi_qualified_import.md:12:22:12:29
PARSE ERROR - multi_qualified_import.md:12:29:12:30
PARSE ERROR - multi_qualified_import.md:12:30:12:31
PARSE ERROR - multi_qualified_import.md:12:31:12:36
PARSE ERROR - multi_qualified_import.md:12:36:12:37
PARSE ERROR - multi_qualified_import.md:12:37:12:38
MODULE NOT FOUND - multi_qualified_import.md:1:1:1:17
UNDECLARED TYPE - multi_qualified_import.md:3:16:3:23
UNDEFINED VARIABLE - multi_qualified_import.md:4:16:4:45
MODULE NOT IMPORTED - multi_qualified_import.md:7:11:7:33
UNUSED VARIABLE - multi_qualified_import.md:8:12:8:19
MODULE NOT IMPORTED - multi_qualified_import.md:11:8:11:34
UNDEFINED VARIABLE - multi_qualified_import.md:12:8:12:12
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:1:17:1:22:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:1:23:1:31:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                      ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:1:32:1:33:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                               ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**multi_qualified_import.md:1:40:1:41:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                                       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:12:12:12:17:**
```roc
data = json.Core.Utf8.encode("hello")
```
           ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:12:17:12:22:**
```roc
data = json.Core.Utf8.encode("hello")
```
                ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:12:22:12:29:**
```roc
data = json.Core.Utf8.encode("hello")
```
                     ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:12:29:12:30:**
```roc
data = json.Core.Utf8.encode("hello")
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:12:30:12:31:**
```roc
data = json.Core.Utf8.encode("hello")
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:12:31:12:36:**
```roc
data = json.Core.Utf8.encode("hello")
```
                              ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:12:36:12:37:**
```roc
data = json.Core.Utf8.encode("hello")
```
                                   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:12:37:12:38:**
```roc
data = json.Core.Utf8.encode("hello")
```
                                    ^


**MODULE NOT FOUND**
The module `json.Core` was not found in this Roc project.

You're attempting to use this module here:
**multi_qualified_import.md:1:1:1:17:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Encoder_ is not declared in this scope.

This type is referenced here:
**multi_qualified_import.md:3:16:3:23:**
```roc
json_encoder : Encoder
```
               ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `defaultEncoder` in this scope.
Is there an `import` or `exposing` missing up-top?

**multi_qualified_import.md:4:16:4:45:**
```roc
json_encoder = Json.Core.Utf8.defaultEncoder
```
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `json.Core.Utf8` imported into this Roc file.

You're attempting to use this module here:
**multi_qualified_import.md:7:11:7:33:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
          ^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `encoder` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_encoder` to suppress this warning.
The unused variable is declared here:
**multi_qualified_import.md:8:12:8:19:**
```roc
process = |encoder| "processing"
```
           ^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `json.Core.Utf8` imported into this Roc file.

You're attempting to use this module here:
**multi_qualified_import.md:11:8:11:34:**
```roc
data : json.Core.Utf8.EncodedData
```
       ^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `json` in this scope.
Is there an `import` or `exposing` missing up-top?

**multi_qualified_import.md:12:8:12:12:**
```roc
data = json.Core.Utf8.encode("hello")
```
       ^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:17),NoSpaceDotUpperIdent(1:17-1:22),KwExposing(1:23-1:31),OpenSquare(1:32-1:33),UpperIdent(1:33-1:40),CloseSquare(1:40-1:41),
LowerIdent(3:1-3:13),OpColon(3:14-3:15),UpperIdent(3:16-3:23),
LowerIdent(4:1-4:13),OpAssign(4:14-4:15),UpperIdent(4:16-4:20),NoSpaceDotUpperIdent(4:20-4:25),NoSpaceDotUpperIdent(4:25-4:30),NoSpaceDotLowerIdent(4:30-4:45),
LowerIdent(7:1-7:8),OpColon(7:9-7:10),LowerIdent(7:11-7:15),NoSpaceDotUpperIdent(7:15-7:20),NoSpaceDotUpperIdent(7:20-7:25),NoSpaceDotUpperIdent(7:25-7:33),OpArrow(7:34-7:36),UpperIdent(7:37-7:40),
LowerIdent(8:1-8:8),OpAssign(8:9-8:10),OpBar(8:11-8:12),LowerIdent(8:12-8:19),OpBar(8:19-8:20),StringStart(8:21-8:22),StringPart(8:22-8:32),StringEnd(8:32-8:33),
LowerIdent(11:1-11:5),OpColon(11:6-11:7),LowerIdent(11:8-11:12),NoSpaceDotUpperIdent(11:12-11:17),NoSpaceDotUpperIdent(11:17-11:22),NoSpaceDotUpperIdent(11:22-11:34),
LowerIdent(12:1-12:5),OpAssign(12:6-12:7),LowerIdent(12:8-12:12),NoSpaceDotUpperIdent(12:12-12:17),NoSpaceDotUpperIdent(12:17-12:22),NoSpaceDotLowerIdent(12:22-12:29),NoSpaceOpenRound(12:29-12:30),StringStart(12:30-12:31),StringPart(12:31-12:36),StringEnd(12:36-12:37),CloseRound(12:37-12:38),
EndOfFile(13:1-13:1),
~~~
# PARSE
~~~clojure
(file @1.1-12.38
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.17 (raw "json.Core"))
		(s-malformed @1.17-1.22 (tag "statement_unexpected_token"))
		(s-malformed @1.23-1.31 (tag "statement_unexpected_token"))
		(s-malformed @1.32-1.33 (tag "statement_unexpected_token"))
		(s-malformed @1.40-1.41 (tag "expected_colon_after_type_annotation"))
		(s-type-anno @3.1-3.23 (name "json_encoder")
			(ty @3.16-3.23 (name "Encoder")))
		(s-decl @4.1-4.45
			(p-ident @4.1-4.13 (raw "json_encoder"))
			(e-ident @4.16-4.45 (raw "Json.Core.Utf8.defaultEncoder")))
		(s-type-anno @7.1-7.40 (name "process")
			(ty-fn @7.11-7.40
				(ty @7.11-7.33 (name "json.Core.Utf8.Encoder"))
				(ty @7.37-7.40 (name "Str"))))
		(s-decl @8.1-8.33
			(p-ident @8.1-8.8 (raw "process"))
			(e-lambda @8.11-8.33
				(args
					(p-ident @8.12-8.19 (raw "encoder")))
				(e-string @8.21-8.33
					(e-string-part @8.22-8.32 (raw "processing")))))
		(s-type-anno @11.1-11.34 (name "data")
			(ty @11.8-11.34 (name "json.Core.Utf8.EncodedData")))
		(s-decl @12.1-12.12
			(p-ident @12.1-12.5 (raw "data"))
			(e-ident @12.8-12.12 (raw "json")))
		(s-malformed @12.12-12.17 (tag "statement_unexpected_token"))
		(s-malformed @12.17-12.22 (tag "statement_unexpected_token"))
		(s-malformed @12.22-12.29 (tag "statement_unexpected_token"))
		(s-malformed @12.29-12.30 (tag "statement_unexpected_token"))
		(s-malformed @12.30-12.31 (tag "statement_unexpected_token"))
		(s-malformed @12.31-12.36 (tag "statement_unexpected_token"))
		(s-malformed @12.36-12.37 (tag "statement_unexpected_token"))
		(s-malformed @12.37-12.38 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
import json.Core


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
		(p-assign @4.1-4.13 (ident "json_encoder"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation @4.1-4.13
			(declared-type
				(ty-malformed @3.16-3.23))))
	(d-let
		(p-assign @8.1-8.8 (ident "process"))
		(e-lambda @8.11-8.33
			(args
				(p-assign @8.12-8.19 (ident "encoder")))
			(e-string @8.21-8.33
				(e-literal @8.22-8.32 (string "processing"))))
		(annotation @8.1-8.8
			(declared-type
				(ty-fn @7.11-7.40 (effectful false)
					(ty-malformed @7.11-7.33)
					(ty-lookup @7.37-7.40 (name "Str") (builtin))))))
	(d-let
		(p-assign @12.1-12.5 (ident "data"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation @12.1-12.5
			(declared-type
				(ty-malformed @11.8-11.34))))
	(s-import @1.1-1.17 (module "json.Core")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.13 (type "Error"))
		(patt @8.1-8.8 (type "Error -> Str"))
		(patt @12.1-12.5 (type "Error")))
	(expressions
		(expr @4.16-4.45 (type "Error"))
		(expr @8.11-8.33 (type "Error -> Str"))
		(expr @12.8-12.12 (type "Error"))))
~~~
