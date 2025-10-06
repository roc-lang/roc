# META
~~~ini
description=Test multi-level qualified imports and type annotations
type=file:MultiQualifiedImport.roc
~~~
# SOURCE
~~~roc
MultiQualifiedImport := {}

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
PARSE ERROR - multi_qualified_import.md:3:17:3:22
PARSE ERROR - multi_qualified_import.md:3:23:3:31
PARSE ERROR - multi_qualified_import.md:3:32:3:33
PARSE ERROR - multi_qualified_import.md:3:40:3:41
PARSE ERROR - multi_qualified_import.md:14:12:14:17
PARSE ERROR - multi_qualified_import.md:14:17:14:22
PARSE ERROR - multi_qualified_import.md:14:22:14:29
PARSE ERROR - multi_qualified_import.md:14:29:14:30
PARSE ERROR - multi_qualified_import.md:14:30:14:31
PARSE ERROR - multi_qualified_import.md:14:31:14:36
PARSE ERROR - multi_qualified_import.md:14:36:14:37
PARSE ERROR - multi_qualified_import.md:14:37:14:38
MODULE NOT FOUND - multi_qualified_import.md:3:1:3:17
UNDECLARED TYPE - multi_qualified_import.md:5:16:5:23
UNDEFINED VARIABLE - multi_qualified_import.md:6:16:6:45
MODULE NOT IMPORTED - multi_qualified_import.md:9:11:9:33
UNUSED VARIABLE - multi_qualified_import.md:10:12:10:19
MODULE NOT IMPORTED - multi_qualified_import.md:13:8:13:34
UNDEFINED VARIABLE - multi_qualified_import.md:14:8:14:12
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:3:17:3:22:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:3:23:3:31:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                      ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:3:32:3:33:**
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

**multi_qualified_import.md:3:40:3:41:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                                       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:14:12:14:17:**
```roc
data = json.Core.Utf8.encode("hello")
```
           ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:14:17:14:22:**
```roc
data = json.Core.Utf8.encode("hello")
```
                ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:14:22:14:29:**
```roc
data = json.Core.Utf8.encode("hello")
```
                     ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:14:29:14:30:**
```roc
data = json.Core.Utf8.encode("hello")
```
                            ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:14:30:14:31:**
```roc
data = json.Core.Utf8.encode("hello")
```
                             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:14:31:14:36:**
```roc
data = json.Core.Utf8.encode("hello")
```
                              ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:14:36:14:37:**
```roc
data = json.Core.Utf8.encode("hello")
```
                                   ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**multi_qualified_import.md:14:37:14:38:**
```roc
data = json.Core.Utf8.encode("hello")
```
                                    ^


**MODULE NOT FOUND**
The module `json.Core` was not found in this Roc project.

You're attempting to use this module here:
**multi_qualified_import.md:3:1:3:17:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Encoder_ is not declared in this scope.

This type is referenced here:
**multi_qualified_import.md:5:16:5:23:**
```roc
json_encoder : Encoder
```
               ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `defaultEncoder` in this scope.
Is there an `import` or `exposing` missing up-top?

**multi_qualified_import.md:6:16:6:45:**
```roc
json_encoder = Json.Core.Utf8.defaultEncoder
```
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `MultiQualifiedImport := {}

import json.Core.Utf8 exposing [Encoder]

json_encoder : Encoder
json_encoder = Json.Core.Utf8.defaultEncoder

# Test with qualified type in annotation
process : json.Core.Utf8` imported into this Roc file.

You're attempting to use this module here:
**multi_qualified_import.md:9:11:9:33:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
          ^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `encoder` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_encoder` to suppress this warning.
The unused variable is declared here:
**multi_qualified_import.md:10:12:10:19:**
```roc
process = |encoder| "processing"
```
           ^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `MultiQualifiedImport := {}

import json.Core.Utf8 exposing [Encoder]

json_encoder : Encoder
json_encoder = Json.Core.Utf8.defaultEncoder

# Test with qualified type in annotation
process : json.Core.Utf8.Encoder -> Str
process = |encoder| "processing"

# Test with multiple qualifiers
data : json.Core.Utf8` imported into this Roc file.

You're attempting to use this module here:
**multi_qualified_import.md:13:8:13:34:**
```roc
data : json.Core.Utf8.EncodedData
```
       ^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `json` in this scope.
Is there an `import` or `exposing` missing up-top?

**multi_qualified_import.md:14:8:14:12:**
```roc
data = json.Core.Utf8.encode("hello")
```
       ^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:21),OpColonEqual(1:22-1:24),OpenCurly(1:25-1:26),CloseCurly(1:26-1:27),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),NoSpaceDotUpperIdent(3:17-3:22),KwExposing(3:23-3:31),OpenSquare(3:32-3:33),UpperIdent(3:33-3:40),CloseSquare(3:40-3:41),
LowerIdent(5:1-5:13),OpColon(5:14-5:15),UpperIdent(5:16-5:23),
LowerIdent(6:1-6:13),OpAssign(6:14-6:15),UpperIdent(6:16-6:20),NoSpaceDotUpperIdent(6:20-6:25),NoSpaceDotUpperIdent(6:25-6:30),NoSpaceDotLowerIdent(6:30-6:45),
LowerIdent(9:1-9:8),OpColon(9:9-9:10),LowerIdent(9:11-9:15),NoSpaceDotUpperIdent(9:15-9:20),NoSpaceDotUpperIdent(9:20-9:25),NoSpaceDotUpperIdent(9:25-9:33),OpArrow(9:34-9:36),UpperIdent(9:37-9:40),
LowerIdent(10:1-10:8),OpAssign(10:9-10:10),OpBar(10:11-10:12),LowerIdent(10:12-10:19),OpBar(10:19-10:20),StringStart(10:21-10:22),StringPart(10:22-10:32),StringEnd(10:32-10:33),
LowerIdent(13:1-13:5),OpColon(13:6-13:7),LowerIdent(13:8-13:12),NoSpaceDotUpperIdent(13:12-13:17),NoSpaceDotUpperIdent(13:17-13:22),NoSpaceDotUpperIdent(13:22-13:34),
LowerIdent(14:1-14:5),OpAssign(14:6-14:7),LowerIdent(14:8-14:12),NoSpaceDotUpperIdent(14:12-14:17),NoSpaceDotUpperIdent(14:17-14:22),NoSpaceDotLowerIdent(14:22-14:29),NoSpaceOpenRound(14:29-14:30),StringStart(14:30-14:31),StringPart(14:31-14:36),StringEnd(14:36-14:37),CloseRound(14:37-14:38),
EndOfFile(15:1-15:1),
~~~
# PARSE
~~~clojure
(file @1.1-14.38
	(type-module @1.1-1.21)
	(statements
		(s-type-decl @1.1-1.27
			(header @1.1-1.21 (name "MultiQualifiedImport")
				(args))
			(ty-record @1.25-1.27))
		(s-import @3.1-3.17 (raw "json.Core"))
		(s-malformed @3.17-3.22 (tag "statement_unexpected_token"))
		(s-malformed @3.23-3.31 (tag "statement_unexpected_token"))
		(s-malformed @3.32-3.33 (tag "statement_unexpected_token"))
		(s-malformed @3.40-3.41 (tag "expected_colon_after_type_annotation"))
		(s-type-anno @5.1-5.23 (name "json_encoder")
			(ty @5.16-5.23 (name "Encoder")))
		(s-decl @6.1-6.45
			(p-ident @6.1-6.13 (raw "json_encoder"))
			(e-ident @6.16-6.45 (raw "Json.Core.Utf8.defaultEncoder")))
		(s-type-anno @9.1-9.40 (name "process")
			(ty-fn @9.11-9.40
				(ty @9.11-9.33 (name "json.Core.Utf8.Encoder"))
				(ty @9.37-9.40 (name "Str"))))
		(s-decl @10.1-10.33
			(p-ident @10.1-10.8 (raw "process"))
			(e-lambda @10.11-10.33
				(args
					(p-ident @10.12-10.19 (raw "encoder")))
				(e-string @10.21-10.33
					(e-string-part @10.22-10.32 (raw "processing")))))
		(s-type-anno @13.1-13.34 (name "data")
			(ty @13.8-13.34 (name "json.Core.Utf8.EncodedData")))
		(s-decl @14.1-14.12
			(p-ident @14.1-14.5 (raw "data"))
			(e-ident @14.8-14.12 (raw "json")))
		(s-malformed @14.12-14.17 (tag "statement_unexpected_token"))
		(s-malformed @14.17-14.22 (tag "statement_unexpected_token"))
		(s-malformed @14.22-14.29 (tag "statement_unexpected_token"))
		(s-malformed @14.29-14.30 (tag "statement_unexpected_token"))
		(s-malformed @14.30-14.31 (tag "statement_unexpected_token"))
		(s-malformed @14.31-14.36 (tag "statement_unexpected_token"))
		(s-malformed @14.36-14.37 (tag "statement_unexpected_token"))
		(s-malformed @14.37-14.38 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
MultiQualifiedImport := {}

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
		(p-assign @6.1-6.13 (ident "json_encoder"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation @6.1-6.13
			(declared-type
				(ty-malformed @5.16-5.23))))
	(d-let
		(p-assign @10.1-10.8 (ident "process"))
		(e-lambda @10.11-10.33
			(args
				(p-assign @10.12-10.19 (ident "encoder")))
			(e-string @10.21-10.33
				(e-literal @10.22-10.32 (string "processing"))))
		(annotation @10.1-10.8
			(declared-type
				(ty-fn @9.11-9.40 (effectful false)
					(ty-malformed @9.11-9.33)
					(ty-lookup @9.37-9.40 (name "Str") (builtin))))))
	(d-let
		(p-assign @14.1-14.5 (ident "data"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation @14.1-14.5
			(declared-type
				(ty-malformed @13.8-13.34))))
	(s-nominal-decl @1.1-1.27
		(ty-header @1.1-1.21 (name "MultiQualifiedImport"))
		(ty-record @1.25-1.27))
	(s-import @3.1-3.17 (module "json.Core") (qualifier "json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.13 (type "Error"))
		(patt @10.1-10.8 (type "Error -> Str"))
		(patt @14.1-14.5 (type "Error")))
	(type_decls
		(nominal @1.1-1.27 (type "MultiQualifiedImport")
			(ty-header @1.1-1.21 (name "MultiQualifiedImport"))))
	(expressions
		(expr @6.16-6.45 (type "Error"))
		(expr @10.11-10.33 (type "Error -> Str"))
		(expr @14.8-14.12 (type "Error"))))
~~~
