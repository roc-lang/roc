# META
~~~ini
description=Test multi-level qualified imports and type annotations
type=file
~~~
# SOURCE
~~~roc
module [json_encoder]

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
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:3:17:3:22
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:3:23:3:31
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:14:12:14:17
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:14:17:14:22
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:14:22:14:29
INVALID STATEMENT - multi_qualified_import.md:3:17:3:22
INVALID STATEMENT - multi_qualified_import.md:3:23:3:31
INVALID STATEMENT - multi_qualified_import.md:3:32:3:41
UNDECLARED TYPE - multi_qualified_import.md:5:16:5:23
UNDEFINED VARIABLE - multi_qualified_import.md:6:16:6:45
UNUSED VARIABLE - multi_qualified_import.md:10:12:10:19
UNDEFINED VARIABLE - multi_qualified_import.md:14:8:14:12
INVALID STATEMENT - multi_qualified_import.md:14:12:14:17
INVALID STATEMENT - multi_qualified_import.md:14:17:14:22
INVALID STATEMENT - multi_qualified_import.md:14:22:14:29
INVALID STATEMENT - multi_qualified_import.md:14:29:14:38
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Utf8** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:3:17:3:22:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **exposing** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:3:23:3:31:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                      ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Core** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:14:12:14:17:**
```roc
data = json.Core.Utf8.encode("hello")
```
           ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Utf8** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:14:17:14:22:**
```roc
data = json.Core.Utf8.encode("hello")
```
                ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.encode** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:14:22:14:29:**
```roc
data = json.Core.Utf8.encode("hello")
```
                     ^^^^^^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:3:17:3:22:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                ^^^^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:3:23:3:31:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                      ^^^^^^^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:3:32:3:41:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                               ^^^^^^^^^


**UNDECLARED TYPE**
The type _Encoder_ is not declared in this scope.

This type is referenced here:
**multi_qualified_import.md:5:16:5:23:**
```roc
json_encoder : Encoder
```
               ^^^^^^^


**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'defaultEncoder' is not defined:
**multi_qualified_import.md:6:16:6:45:**
```roc
json_encoder = Json.Core.Utf8.defaultEncoder
```
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `encoder` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_encoder` to suppress this warning.
The unused variable is declared here:
**multi_qualified_import.md:10:12:10:19:**
```roc
process = |encoder| "processing"
```
           ^^^^^^^


**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'json' is not defined:
**multi_qualified_import.md:14:8:14:12:**
```roc
data = json.Core.Utf8.encode("hello")
```
       ^^^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:14:12:14:17:**
```roc
data = json.Core.Utf8.encode("hello")
```
           ^^^^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:14:17:14:22:**
```roc
data = json.Core.Utf8.encode("hello")
```
                ^^^^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:14:22:14:29:**
```roc
data = json.Core.Utf8.encode("hello")
```
                     ^^^^^^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:14:29:14:38:**
```roc
data = json.Core.Utf8.encode("hello")
```
                            ^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:21),CloseSquare(1:21-1:22),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),NoSpaceDotUpperIdent(3:17-3:22),KwExposing(3:23-3:31),OpenSquare(3:32-3:33),UpperIdent(3:33-3:40),CloseSquare(3:40-3:41),
LowerIdent(5:1-5:13),OpColon(5:14-5:15),UpperIdent(5:16-5:23),
LowerIdent(6:1-6:13),OpAssign(6:14-6:15),UpperIdent(6:16-6:20),NoSpaceDotUpperIdent(6:20-6:25),NoSpaceDotUpperIdent(6:25-6:30),NoSpaceDotLowerIdent(6:30-6:45),
LowerIdent(9:1-9:8),OpColon(9:9-9:10),LowerIdent(9:11-9:15),NoSpaceDotUpperIdent(9:15-9:20),NoSpaceDotUpperIdent(9:20-9:25),NoSpaceDotUpperIdent(9:25-9:33),OpArrow(9:34-9:36),UpperIdent(9:37-9:40),
LowerIdent(10:1-10:8),OpAssign(10:9-10:10),OpBar(10:11-10:12),LowerIdent(10:12-10:19),OpBar(10:19-10:20),StringStart(10:21-10:22),StringPart(10:22-10:32),StringEnd(10:32-10:33),
LowerIdent(13:1-13:5),OpColon(13:6-13:7),LowerIdent(13:8-13:12),NoSpaceDotUpperIdent(13:12-13:17),NoSpaceDotUpperIdent(13:17-13:22),NoSpaceDotUpperIdent(13:22-13:34),
LowerIdent(14:1-14:5),OpAssign(14:6-14:7),LowerIdent(14:8-14:12),NoSpaceDotUpperIdent(14:12-14:17),NoSpaceDotUpperIdent(14:17-14:22),NoSpaceDotLowerIdent(14:22-14:29),NoSpaceOpenRound(14:29-14:30),StringStart(14:30-14:31),StringPart(14:31-14:36),StringEnd(14:36-14:37),CloseRound(14:37-14:38),EndOfFile(14:38-14:38),
~~~
# PARSE
~~~clojure
(file @1.1-14.38
	(module @1.1-1.22
		(exposes @1.8-1.22
			(exposed-lower-ident @1.9-1.21
				(text "json_encoder"))))
	(statements
		(s-import @3.1-3.17 (raw "json.Core"))
		(e-malformed @3.17-3.22 (reason "expr_unexpected_token"))
		(e-malformed @3.23-3.31 (reason "expr_unexpected_token"))
		(e-list @3.32-3.41
			(e-tag @3.33-3.40 (raw "Encoder")))
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
		(e-malformed @14.12-14.17 (reason "expr_unexpected_token"))
		(e-malformed @14.17-14.22 (reason "expr_unexpected_token"))
		(e-malformed @14.22-14.29 (reason "expr_unexpected_token"))
		(e-tuple @14.29-14.38
			(e-string @14.30-14.37
				(e-string-part @14.31-14.36 (raw "hello"))))))
~~~
# FORMATTED
~~~roc
module [json_encoder]

import json.Core
[Encoder]

json_encoder : Encoder
json_encoder = Json.defaultEncoder

# Test with qualified type in annotation
process : json.Core.Utf8.Encoder -> Str
process = |encoder| "processing"

# Test with multiple qualifiers
data : json.Core.Utf8.EncodedData
data = json
("hello")
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(def
		(pattern
			(p-assign @6.1-6.13 (ident "json_encoder")))
		(expr
			(e-runtime-error (tag "ident_not_in_scope")))
		(annotation
			(annotation
				(type-anno
					(ty @5.16-5.23 (name "Encoder"))))))
	(def
		(pattern
			(p-assign @10.1-10.8 (ident "process")))
		(expr
			(e-lambda @10.11-10.33
				(args
					(p-assign @10.12-10.19 (ident "encoder")))
				(e-string @10.21-10.33
					(e-literal @10.22-10.32 (string "processing")))))
		(annotation
			(annotation
				(type-anno
					(ty-fn @9.11-9.40 (effectful false)
						(ty-lookup-external @9.11-9.33
							(external-decl @9.11-9.33 (qualified-name "json.Core.Utf8.Encoder") (module-name "json.Core.Utf8") (local-name "Encoder") (kind "type")))
						(ty @9.37-9.40 (name "Str")))))))
	(def
		(pattern
			(p-assign @14.1-14.5 (ident "data")))
		(expr
			(e-runtime-error (tag "ident_not_in_scope")))
		(annotation
			(annotation
				(type-anno
					(ty-lookup-external @13.8-13.34
						(external-decl @13.8-13.34 (qualified-name "json.Core.Utf8.EncodedData") (module-name "json.Core.Utf8") (local-name "EncodedData") (kind "type")))))))
	(s-import @3.1-3.17 (module "json.Core") (qualifier "json")
		(exposes))
	(external-decl (qualified-name "json.Core.Utf8.Encoder") (module-name "json.Core.Utf8") (local-name "Encoder") (kind "type"))
	(external-decl (qualified-name "json.Core.Utf8.EncodedData") (module-name "json.Core.Utf8") (local-name "EncodedData") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.13 (type "Error"))
		(patt @10.1-10.8 (type "json.Core.Utf8.Encoder -> Str"))
		(patt @14.1-14.5 (type "Error")))
	(expressions
		(expr @1.1-1.1 (type "Error"))
		(expr @10.11-10.33 (type "json.Core.Utf8.Encoder -> Str"))
		(expr @1.1-1.1 (type "Error"))))
~~~
