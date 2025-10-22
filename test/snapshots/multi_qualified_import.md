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
		(s-import (raw "json.Core"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
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
		(p-assign (ident "json_encoder"))
		(e-runtime-error (tag "ident_not_in_scope"))
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
		(exposes)))
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
