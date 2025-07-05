# META
~~~ini
description=Nested module qualification
type=file
~~~
# SOURCE
~~~roc
module []

import json.Parser.Config
import http.Client.Auth as HttpAuth
import utils.String.Format exposing [padLeft]

# Test multi-level type qualification
parseConfig : Config.Settings -> Str
parseConfig = |settings| Config.toString(settings)

# Test multi-level value qualification
authenticate : Str, Str -> HttpAuth.Token
authenticate = |user, pass| HttpAuth.login(user, pass)

# Test deeply nested qualification
processData : Config.Parser.Advanced, Str -> Result(Str, Config.Parser.Error)
processData = |advancedConfig, input|
    Config.Parser.Advanced.parseWith(advancedConfig, input)

# Test mixed qualification (exposed item + qualified)
formatOutput : Str -> Str
formatOutput = |text| padLeft(text, Config.defaultPadding)

# Test qualified type in function signature
validateAuth : HttpAuth.Credentials -> Result(HttpAuth.Token, HttpAuth.Error)
validateAuth = |creds| HttpAuth.validate(creds)
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - can_import_nested_modules.md:4:19:4:27
UNEXPECTED TOKEN IN EXPRESSION - can_import_nested_modules.md:4:25:4:36
UNEXPECTED TOKEN IN EXPRESSION - can_import_nested_modules.md:5:13:5:27
UNEXPECTED TOKEN IN EXPRESSION - can_import_nested_modules.md:5:20:5:36
UNEXPECTED TOKEN IN EXPRESSION - can_import_nested_modules.md:5:28:5:38
UNDEFINED VARIABLE - can_import_nested_modules.md:9:26:9:41
UNDEFINED VARIABLE - can_import_nested_modules.md:13:29:13:43
UNDEFINED VARIABLE - can_import_nested_modules.md:18:5:18:37
UNDEFINED VARIABLE - can_import_nested_modules.md:22:23:22:30
UNDEFINED VARIABLE - can_import_nested_modules.md:22:37:22:58
UNDEFINED VARIABLE - can_import_nested_modules.md:26:24:26:41
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_import_nested_modules.md:3:19:3:19:**
```roc
import json.Parser.Config
```
                  


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Auth as** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_import_nested_modules.md:4:19:4:27:**
```roc
import http.Client.Auth as HttpAuth
```
                  ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as HttpAuth** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_import_nested_modules.md:4:25:4:36:**
```roc
import http.Client.Auth as HttpAuth
```
                        ^^^^^^^^^^^


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
**can_import_nested_modules.md:4:28:4:28:**
```roc
import http.Client.Auth as HttpAuth
```
                           


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.String.Format** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_import_nested_modules.md:5:13:5:27:**
```roc
import utils.String.Format exposing [padLeft]
```
            ^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Format exposing** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_import_nested_modules.md:5:20:5:36:**
```roc
import utils.String.Format exposing [padLeft]
```
                   ^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **exposing [** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**can_import_nested_modules.md:5:28:5:38:**
```roc
import utils.String.Format exposing [padLeft]
```
                           ^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:19),NoSpaceDotUpperIdent(3:19-3:26),Newline(1:1-1:1),
KwImport(4:1-4:7),LowerIdent(4:8-4:12),NoSpaceDotUpperIdent(4:12-4:19),NoSpaceDotUpperIdent(4:19-4:24),KwAs(4:25-4:27),UpperIdent(4:28-4:36),Newline(1:1-1:1),
KwImport(5:1-5:7),LowerIdent(5:8-5:13),NoSpaceDotUpperIdent(5:13-5:20),NoSpaceDotUpperIdent(5:20-5:27),KwExposing(5:28-5:36),OpenSquare(5:37-5:38),LowerIdent(5:38-5:45),CloseSquare(5:45-5:46),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(7:2-7:38),
LowerIdent(8:1-8:12),OpColon(8:13-8:14),UpperIdent(8:15-8:21),NoSpaceDotUpperIdent(8:21-8:30),OpArrow(8:31-8:33),UpperIdent(8:34-8:37),Newline(1:1-1:1),
LowerIdent(9:1-9:12),OpAssign(9:13-9:14),OpBar(9:15-9:16),LowerIdent(9:16-9:24),OpBar(9:24-9:25),UpperIdent(9:26-9:32),NoSpaceDotLowerIdent(9:32-9:41),NoSpaceOpenRound(9:41-9:42),LowerIdent(9:42-9:50),CloseRound(9:50-9:51),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(11:2-11:39),
LowerIdent(12:1-12:13),OpColon(12:14-12:15),UpperIdent(12:16-12:19),Comma(12:19-12:20),UpperIdent(12:21-12:24),OpArrow(12:25-12:27),UpperIdent(12:28-12:36),NoSpaceDotUpperIdent(12:36-12:42),Newline(1:1-1:1),
LowerIdent(13:1-13:13),OpAssign(13:14-13:15),OpBar(13:16-13:17),LowerIdent(13:17-13:21),Comma(13:21-13:22),LowerIdent(13:23-13:27),OpBar(13:27-13:28),UpperIdent(13:29-13:37),NoSpaceDotLowerIdent(13:37-13:43),NoSpaceOpenRound(13:43-13:44),LowerIdent(13:44-13:48),Comma(13:48-13:49),LowerIdent(13:50-13:54),CloseRound(13:54-13:55),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(15:2-15:35),
LowerIdent(16:1-16:12),OpColon(16:13-16:14),UpperIdent(16:15-16:21),NoSpaceDotUpperIdent(16:21-16:28),NoSpaceDotUpperIdent(16:28-16:37),Comma(16:37-16:38),UpperIdent(16:39-16:42),OpArrow(16:43-16:45),UpperIdent(16:46-16:52),NoSpaceOpenRound(16:52-16:53),UpperIdent(16:53-16:56),Comma(16:56-16:57),UpperIdent(16:58-16:64),NoSpaceDotUpperIdent(16:64-16:71),NoSpaceDotUpperIdent(16:71-16:77),CloseRound(16:77-16:78),Newline(1:1-1:1),
LowerIdent(17:1-17:12),OpAssign(17:13-17:14),OpBar(17:15-17:16),LowerIdent(17:16-17:30),Comma(17:30-17:31),LowerIdent(17:32-17:37),OpBar(17:37-17:38),Newline(1:1-1:1),
UpperIdent(18:5-18:11),NoSpaceDotUpperIdent(18:11-18:18),NoSpaceDotUpperIdent(18:18-18:27),NoSpaceDotLowerIdent(18:27-18:37),NoSpaceOpenRound(18:37-18:38),LowerIdent(18:38-18:52),Comma(18:52-18:53),LowerIdent(18:54-18:59),CloseRound(18:59-18:60),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(20:2-20:54),
LowerIdent(21:1-21:13),OpColon(21:14-21:15),UpperIdent(21:16-21:19),OpArrow(21:20-21:22),UpperIdent(21:23-21:26),Newline(1:1-1:1),
LowerIdent(22:1-22:13),OpAssign(22:14-22:15),OpBar(22:16-22:17),LowerIdent(22:17-22:21),OpBar(22:21-22:22),LowerIdent(22:23-22:30),NoSpaceOpenRound(22:30-22:31),LowerIdent(22:31-22:35),Comma(22:35-22:36),UpperIdent(22:37-22:43),NoSpaceDotLowerIdent(22:43-22:58),CloseRound(22:58-22:59),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(24:2-24:44),
LowerIdent(25:1-25:13),OpColon(25:14-25:15),UpperIdent(25:16-25:24),NoSpaceDotUpperIdent(25:24-25:36),OpArrow(25:37-25:39),UpperIdent(25:40-25:46),NoSpaceOpenRound(25:46-25:47),UpperIdent(25:47-25:55),NoSpaceDotUpperIdent(25:55-25:61),Comma(25:61-25:62),UpperIdent(25:63-25:71),NoSpaceDotUpperIdent(25:71-25:77),CloseRound(25:77-25:78),Newline(1:1-1:1),
LowerIdent(26:1-26:13),OpAssign(26:14-26:15),OpBar(26:16-26:17),LowerIdent(26:17-26:22),OpBar(26:22-26:23),UpperIdent(26:24-26:32),NoSpaceDotLowerIdent(26:32-26:41),NoSpaceOpenRound(26:41-26:42),LowerIdent(26:42-26:47),CloseRound(26:47-26:48),EndOfFile(26:48-26:48),
~~~
# PARSE
~~~clojure
(file @1.1-26.48
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @3.1-3.19 (raw "json.Parser"))
		(e-malformed @1.1-1.1 (reason "expr_unexpected_token"))
		(s-import @4.1-4.19 (raw "http.Client"))
		(e-malformed @4.19-4.27 (reason "expr_unexpected_token"))
		(e-malformed @4.25-4.36 (reason "expr_unexpected_token"))
		(s-malformed @4.28-5.13 (tag "expected_colon_after_type_annotation"))
		(e-ident @5.8-5.13 (raw "utils"))
		(e-malformed @5.13-5.27 (reason "expr_unexpected_token"))
		(e-malformed @5.20-5.36 (reason "expr_unexpected_token"))
		(e-malformed @5.28-5.38 (reason "expr_unexpected_token"))
		(e-list @5.37-5.46
			(e-ident @5.38-5.45 (raw "padLeft")))
		(s-type-anno @1.1-1.1 (name "parseConfig")
			(ty-fn @8.15-8.37
				(ty @8.15-8.30 (name "Config.Settings"))
				(ty @8.34-8.37 (name "Str"))))
		(s-decl @9.1-9.51
			(p-ident @9.1-9.12 (raw "parseConfig"))
			(e-lambda @9.15-9.51
				(args
					(p-ident @9.16-9.24 (raw "settings")))
				(e-apply @9.26-9.51
					(e-ident @9.26-9.41 (raw "Config.toString"))
					(e-ident @9.42-9.50 (raw "settings")))))
		(s-type-anno @1.1-1.1 (name "authenticate")
			(ty-fn @12.16-12.42
				(ty @12.16-12.19 (name "Str"))
				(ty @12.21-12.24 (name "Str"))
				(ty @12.28-12.42 (name "HttpAuth.Token"))))
		(s-decl @13.1-13.55
			(p-ident @13.1-13.13 (raw "authenticate"))
			(e-lambda @13.16-13.55
				(args
					(p-ident @13.17-13.21 (raw "user"))
					(p-ident @13.23-13.27 (raw "pass")))
				(e-apply @13.29-13.55
					(e-ident @13.29-13.43 (raw "HttpAuth.login"))
					(e-ident @13.44-13.48 (raw "user"))
					(e-ident @13.50-13.54 (raw "pass")))))
		(s-type-anno @16.1-17.12 (name "processData")
			(ty-fn @16.15-16.78
				(ty @16.15-16.37 (name "Config.Parser.Advanced"))
				(ty @16.39-16.42 (name "Str"))
				(ty-apply @16.46-16.78
					(ty @16.46-16.52 (name "Result"))
					(ty @16.53-16.56 (name "Str"))
					(ty @16.58-16.77 (name "Config.Parser.Error")))))
		(s-decl @17.1-18.60
			(p-ident @17.1-17.12 (raw "processData"))
			(e-lambda @17.15-18.60
				(args
					(p-ident @17.16-17.30 (raw "advancedConfig"))
					(p-ident @17.32-17.37 (raw "input")))
				(e-apply @18.5-18.60
					(e-ident @18.5-18.37 (raw "Config.Parser.Advanced.parseWith"))
					(e-ident @18.38-18.52 (raw "advancedConfig"))
					(e-ident @18.54-18.59 (raw "input")))))
		(s-type-anno @1.1-1.1 (name "formatOutput")
			(ty-fn @21.16-21.26
				(ty @21.16-21.19 (name "Str"))
				(ty @21.23-21.26 (name "Str"))))
		(s-decl @22.1-22.59
			(p-ident @22.1-22.13 (raw "formatOutput"))
			(e-lambda @22.16-22.59
				(args
					(p-ident @22.17-22.21 (raw "text")))
				(e-apply @22.23-22.59
					(e-ident @22.23-22.30 (raw "padLeft"))
					(e-ident @22.31-22.35 (raw "text"))
					(e-ident @22.37-22.58 (raw "Config.defaultPadding")))))
		(s-type-anno @25.1-26.13 (name "validateAuth")
			(ty-fn @25.16-25.78
				(ty @25.16-25.36 (name "HttpAuth.Credentials"))
				(ty-apply @25.40-25.78
					(ty @25.40-25.46 (name "Result"))
					(ty @25.47-25.61 (name "HttpAuth.Token"))
					(ty @25.63-25.77 (name "HttpAuth.Error")))))
		(s-decl @26.1-26.48
			(p-ident @26.1-26.13 (raw "validateAuth"))
			(e-lambda @26.16-26.48
				(args
					(p-ident @26.17-26.22 (raw "creds")))
				(e-apply @26.24-26.48
					(e-ident @26.24-26.41 (raw "HttpAuth.validate"))
					(e-ident @26.42-26.47 (raw "creds")))))))
~~~
# FORMATTED
~~~roc
module []

import json.Parser
import http.Clientutils[padLeft]

# Test multi-level type qualification
parseConfig : Config.Settings -> Str
parseConfig = |settings| Config.toString(settings)

# Test multi-level value qualification
authenticate : Str, Str -> HttpAuth.Token
authenticate = |user, pass| HttpAuth.login(user, pass)

# Test deeply nested qualification
processData : Config..Parser.Advanced, Str -> Result(Str, Config..Parser.Error)
processData = |advancedConfig, input|
	Config.parseWith(advancedConfig, input)

# Test mixed qualification (exposed item + qualified)
formatOutput : Str -> Str
formatOutput = |text| padLeft(text, Config.defaultPadding)

# Test qualified type in function signature
validateAuth : HttpAuth.Credentials -> Result(HttpAuth.Token, HttpAuth.Error)
validateAuth = |creds| HttpAuth.validate(creds)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @9.1-9.12 (ident "parseConfig"))
		(e-lambda @9.15-9.51
			(args
				(p-assign @9.16-9.24 (ident "settings")))
			(e-call @9.26-9.51
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local @9.42-9.50
					(pattern @9.16-9.24))))
		(annotation @9.1-9.12
			(declared-type
				(ty-fn @8.15-8.37 (effectful false)
					(ty-lookup-external @8.15-8.30
						(ext-decl @8.15-8.30 (ident "Config.Settings") (kind "type")))
					(ty @8.34-8.37 (name "Str"))))))
	(d-let
		(p-assign @13.1-13.13 (ident "authenticate"))
		(e-lambda @13.16-13.55
			(args
				(p-assign @13.17-13.21 (ident "user"))
				(p-assign @13.23-13.27 (ident "pass")))
			(e-call @13.29-13.55
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local @13.44-13.48
					(pattern @13.17-13.21))
				(e-lookup-local @13.50-13.54
					(pattern @13.23-13.27))))
		(annotation @13.1-13.13
			(declared-type
				(ty-fn @12.16-12.42 (effectful false)
					(ty @12.16-12.19 (name "Str"))
					(ty @12.21-12.24 (name "Str"))
					(ty-lookup-external @12.28-12.42
						(ext-decl @12.28-12.42 (ident "HttpAuth.Token") (kind "type")))))))
	(d-let
		(p-assign @17.1-17.12 (ident "processData"))
		(e-lambda @17.15-18.60
			(args
				(p-assign @17.16-17.30 (ident "advancedConfig"))
				(p-assign @17.32-17.37 (ident "input")))
			(e-call @18.5-18.60
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local @18.38-18.52
					(pattern @17.16-17.30))
				(e-lookup-local @18.54-18.59
					(pattern @17.32-17.37))))
		(annotation @17.1-17.12
			(declared-type
				(ty-fn @16.15-16.78 (effectful false)
					(ty-lookup-external @16.15-16.37
						(ext-decl @16.15-16.37 (ident "Config.Parser.Advanced") (kind "type")))
					(ty @16.39-16.42 (name "Str"))
					(ty-apply @16.46-16.78 (symbol "Result")
						(ty @16.53-16.56 (name "Str"))
						(ty-lookup-external @16.58-16.77
							(ext-decl @16.58-16.77 (ident "Config.Parser.Error") (kind "type"))))))))
	(d-let
		(p-assign @22.1-22.13 (ident "formatOutput"))
		(e-lambda @22.16-22.59
			(args
				(p-assign @22.17-22.21 (ident "text")))
			(e-call @22.23-22.59
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local @22.31-22.35
					(pattern @22.17-22.21))
				(e-runtime-error (tag "ident_not_in_scope"))))
		(annotation @22.1-22.13
			(declared-type
				(ty-fn @21.16-21.26 (effectful false)
					(ty @21.16-21.19 (name "Str"))
					(ty @21.23-21.26 (name "Str"))))))
	(d-let
		(p-assign @26.1-26.13 (ident "validateAuth"))
		(e-lambda @26.16-26.48
			(args
				(p-assign @26.17-26.22 (ident "creds")))
			(e-call @26.24-26.48
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local @26.42-26.47
					(pattern @26.17-26.22))))
		(annotation @26.1-26.13
			(declared-type
				(ty-fn @25.16-25.78 (effectful false)
					(ty-lookup-external @25.16-25.36
						(ext-decl @25.16-25.36 (ident "HttpAuth.Credentials") (kind "type")))
					(ty-apply @25.40-25.78 (symbol "Result")
						(ty-lookup-external @25.47-25.61
							(ext-decl @25.47-25.61 (ident "HttpAuth.Token") (kind "type")))
						(ty-lookup-external @25.63-25.77
							(ext-decl @25.63-25.77 (ident "HttpAuth.Error") (kind "type"))))))))
	(s-import @3.1-3.19 (module "json.Parser") (qualifier "json")
		(exposes))
	(s-import @4.1-4.19 (module "http.Client") (qualifier "http")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @9.1-9.12 (type "Config.Settings -> Str"))
		(patt @13.1-13.13 (type "Str, Str -> HttpAuth.Token"))
		(patt @17.1-17.12 (type "Config.Parser.Advanced, Str -> Error"))
		(patt @22.1-22.13 (type "Str -> Str"))
		(patt @26.1-26.13 (type "HttpAuth.Credentials -> Error")))
	(expressions
		(expr @9.15-9.51 (type "Config.Settings -> Str"))
		(expr @13.16-13.55 (type "Str, Str -> HttpAuth.Token"))
		(expr @17.15-18.60 (type "Config.Parser.Advanced, Str -> Error"))
		(expr @22.16-22.59 (type "Str -> Str"))
		(expr @26.16-26.48 (type "HttpAuth.Credentials -> Error"))))
~~~
