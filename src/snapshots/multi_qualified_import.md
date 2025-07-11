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
process = \encoder -> "processing"

# Test with multiple qualifiers
data : json.Core.Utf8.EncodedData
data = json.Core.Utf8.encode "hello"
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:3:17:3:22
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:3:23:3:31
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:9:15:9:20
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:9:20:9:25
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:9:25:9:33
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:9:34:9:36
PARSE ERROR - multi_qualified_import.md:10:1:10:8
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:10:9:10:10
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:10:11:10:12
PARSE ERROR - multi_qualified_import.md:10:23:10:24
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:10:24:10:34
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:10:34:10:35
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:13:12:13:17
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:13:17:13:22
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:13:22:13:34
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:14:12:14:17
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:14:17:14:22
UNEXPECTED TOKEN IN EXPRESSION - multi_qualified_import.md:14:22:14:29
INVALID STATEMENT - multi_qualified_import.md:3:17:3:22
INVALID STATEMENT - multi_qualified_import.md:3:23:3:31
INVALID STATEMENT - multi_qualified_import.md:3:32:3:41
UNDECLARED TYPE - multi_qualified_import.md:5:16:5:23
UNDEFINED VARIABLE - multi_qualified_import.md:6:16:6:45
INVALID STATEMENT - multi_qualified_import.md:9:15:9:20
INVALID STATEMENT - multi_qualified_import.md:9:20:9:25
INVALID STATEMENT - multi_qualified_import.md:9:25:9:33
INVALID STATEMENT - multi_qualified_import.md:9:34:9:36
INVALID STATEMENT - multi_qualified_import.md:10:9:10:10
INVALID STATEMENT - multi_qualified_import.md:10:11:10:12
INVALID STATEMENT - multi_qualified_import.md:10:12:10:24
INVALID STATEMENT - multi_qualified_import.md:10:24:10:34
INVALID STATEMENT - multi_qualified_import.md:10:34:10:35
INVALID STATEMENT - multi_qualified_import.md:13:12:13:17
INVALID STATEMENT - multi_qualified_import.md:13:17:13:22
INVALID STATEMENT - multi_qualified_import.md:13:22:13:34
UNDEFINED VARIABLE - multi_qualified_import.md:14:8:14:12
INVALID STATEMENT - multi_qualified_import.md:14:12:14:17
INVALID STATEMENT - multi_qualified_import.md:14:17:14:22
INVALID STATEMENT - multi_qualified_import.md:14:22:14:29
INVALID STATEMENT - multi_qualified_import.md:14:30:14:37
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
**multi_qualified_import.md:9:15:9:20:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
              ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Utf8** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:9:20:9:25:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
                   ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Encoder** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:9:25:9:33:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
                        ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **->** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:9:34:9:36:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
                                 ^^


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

Here is the problematic code:
**multi_qualified_import.md:10:1:10:8:**
```roc
process = \encoder -> "processing"
```
^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:10:9:10:10:**
```roc
process = \encoder -> "processing"
```
        ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **\** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:10:11:10:12:**
```roc
process = \encoder -> "processing"
```
          ^


**PARSE ERROR**
A parsing error occurred: `expr_arrow_expects_ident`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**multi_qualified_import.md:10:23:10:24:**
```roc
process = \encoder -> "processing"
```
                      ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **processing** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:10:24:10:34:**
```roc
process = \encoder -> "processing"
```
                       ^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:10:34:10:35:**
```roc
process = \encoder -> "processing"
```
                                 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Core** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:13:12:13:17:**
```roc
data : json.Core.Utf8.EncodedData
```
           ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Utf8** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:13:17:13:22:**
```roc
data : json.Core.Utf8.EncodedData
```
                ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.EncodedData** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:13:22:13:34:**
```roc
data : json.Core.Utf8.EncodedData
```
                     ^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Core** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:14:12:14:17:**
```roc
data = json.Core.Utf8.encode "hello"
```
           ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Utf8** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:14:17:14:22:**
```roc
data = json.Core.Utf8.encode "hello"
```
                ^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.encode** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**multi_qualified_import.md:14:22:14:29:**
```roc
data = json.Core.Utf8.encode "hello"
```
                     ^^^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:3:17:3:22:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                ^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:3:23:3:31:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                      ^^^^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:3:32:3:41:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
                               ^^^^^^^^^


**UNDECLARED TYPE**
The type ``Encoder`` is not declared in this scope.

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


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:9:15:9:20:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
              ^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:9:20:9:25:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
                   ^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:9:25:9:33:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
                        ^^^^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:9:34:9:36:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
                                 ^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:10:9:10:10:**
```roc
process = \encoder -> "processing"
```
        ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:10:11:10:12:**
```roc
process = \encoder -> "processing"
```
          ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:10:12:10:24:**
```roc
process = \encoder -> "processing"
```
           ^^^^^^^^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:10:24:10:34:**
```roc
process = \encoder -> "processing"
```
                       ^^^^^^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:10:34:10:35:**
```roc
process = \encoder -> "processing"
```
                                 ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:13:12:13:17:**
```roc
data : json.Core.Utf8.EncodedData
```
           ^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:13:17:13:22:**
```roc
data : json.Core.Utf8.EncodedData
```
                ^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:13:22:13:34:**
```roc
data : json.Core.Utf8.EncodedData
```
                     ^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `json` in this scope.
Is there an `import` or `exposing` missing up-top?

**multi_qualified_import.md:14:8:14:12:**
```roc
data = json.Core.Utf8.encode "hello"
```
       ^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:14:12:14:17:**
```roc
data = json.Core.Utf8.encode "hello"
```
           ^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:14:17:14:22:**
```roc
data = json.Core.Utf8.encode "hello"
```
                ^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:14:22:14:29:**
```roc
data = json.Core.Utf8.encode "hello"
```
                     ^^^^^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**multi_qualified_import.md:14:30:14:37:**
```roc
data = json.Core.Utf8.encode "hello"
```
                             ^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:21),CloseSquare(1:21-1:22),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),NoSpaceDotUpperIdent(3:17-3:22),KwExposing(3:23-3:31),OpenSquare(3:32-3:33),UpperIdent(3:33-3:40),CloseSquare(3:40-3:41),
LowerIdent(5:1-5:13),OpColon(5:14-5:15),UpperIdent(5:16-5:23),
LowerIdent(6:1-6:13),OpAssign(6:14-6:15),UpperIdent(6:16-6:20),NoSpaceDotUpperIdent(6:20-6:25),NoSpaceDotUpperIdent(6:25-6:30),NoSpaceDotLowerIdent(6:30-6:45),
LowerIdent(9:1-9:8),OpColon(9:9-9:10),LowerIdent(9:11-9:15),NoSpaceDotUpperIdent(9:15-9:20),NoSpaceDotUpperIdent(9:20-9:25),NoSpaceDotUpperIdent(9:25-9:33),OpArrow(9:34-9:36),UpperIdent(9:37-9:40),
LowerIdent(10:1-10:8),OpAssign(10:9-10:10),OpBackslash(10:11-10:12),LowerIdent(10:12-10:19),OpArrow(10:20-10:22),StringStart(10:23-10:24),StringPart(10:24-10:34),StringEnd(10:34-10:35),
LowerIdent(13:1-13:5),OpColon(13:6-13:7),LowerIdent(13:8-13:12),NoSpaceDotUpperIdent(13:12-13:17),NoSpaceDotUpperIdent(13:17-13:22),NoSpaceDotUpperIdent(13:22-13:34),
LowerIdent(14:1-14:5),OpAssign(14:6-14:7),LowerIdent(14:8-14:12),NoSpaceDotUpperIdent(14:12-14:17),NoSpaceDotUpperIdent(14:17-14:22),NoSpaceDotLowerIdent(14:22-14:29),StringStart(14:30-14:31),StringPart(14:31-14:36),StringEnd(14:36-14:37),EndOfFile(14:37-14:37),
~~~
# PARSE
~~~clojure
(file @1.1-14.37
	(module @1.1-1.22
		(exposes @1.8-1.22
			(exposed-lower-ident @1.9-1.21 (text "json_encoder"))))
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
		(s-type-anno @9.1-9.15 (name "process")
			(ty-var @1.1-1.1 (raw "json")))
		(e-malformed @9.15-9.20 (reason "expr_unexpected_token"))
		(e-malformed @9.20-9.25 (reason "expr_unexpected_token"))
		(e-malformed @9.25-9.33 (reason "expr_unexpected_token"))
		(e-malformed @9.34-9.36 (reason "expr_unexpected_token"))
		(s-malformed @9.37-10.8 (tag "expected_colon_after_type_annotation"))
		(e-malformed @10.9-10.10 (reason "expr_unexpected_token"))
		(e-malformed @10.11-10.12 (reason "expr_unexpected_token"))
		(e-malformed @10.23-10.24 (reason "expr_arrow_expects_ident"))
		(e-malformed @10.24-10.34 (reason "expr_unexpected_token"))
		(e-malformed @10.34-10.35 (reason "expr_unexpected_token"))
		(s-type-anno @13.1-13.12 (name "data")
			(ty-var @1.1-1.1 (raw "json")))
		(e-malformed @13.12-13.17 (reason "expr_unexpected_token"))
		(e-malformed @13.17-13.22 (reason "expr_unexpected_token"))
		(e-malformed @13.22-13.34 (reason "expr_unexpected_token"))
		(s-decl @14.1-14.12
			(p-ident @14.1-14.5 (raw "data"))
			(e-ident @14.8-14.12 (raw "json")))
		(e-malformed @14.12-14.17 (reason "expr_unexpected_token"))
		(e-malformed @14.17-14.22 (reason "expr_unexpected_token"))
		(e-malformed @14.22-14.29 (reason "expr_unexpected_token"))
		(e-string @14.30-14.37
			(e-string-part @14.31-14.36 (raw "hello")))))
~~~
# FORMATTED
~~~roc
module [json_encoder]

import json.Core[Encoder]

json_encoder : Encoder
json_encoder = Json.defaultEncoder

# Test with qualified type in annotation
process : json

# Test with multiple qualifiers
data : json
data = json"hello"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.13 (ident "json_encoder"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation @6.1-6.13
			(declared-type
				(ty @5.16-5.23 (name "Encoder")))))
	(d-let
		(p-assign @14.1-14.5 (ident "data"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-import @3.1-3.17 (module "json.Core") (qualifier "json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.13 (type "Error"))
		(patt @14.1-14.5 (type "Error")))
	(expressions
		(expr @6.16-6.45 (type "Error"))
		(expr @14.8-14.12 (type "Error"))))
~~~
