# META
~~~ini
description=Nested module qualification
type=file
~~~
# SOURCE
~~~roc
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
PARSE ERROR - can_import_nested_modules.md:1:19:1:26
PARSE ERROR - can_import_nested_modules.md:2:19:2:24
PARSE ERROR - can_import_nested_modules.md:2:25:2:27
PARSE ERROR - can_import_nested_modules.md:3:1:3:7
PARSE ERROR - can_import_nested_modules.md:3:8:3:13
PARSE ERROR - can_import_nested_modules.md:3:13:3:20
PARSE ERROR - can_import_nested_modules.md:3:20:3:27
PARSE ERROR - can_import_nested_modules.md:3:28:3:36
PARSE ERROR - can_import_nested_modules.md:3:37:3:38
PARSE ERROR - can_import_nested_modules.md:3:38:3:45
PARSE ERROR - can_import_nested_modules.md:3:45:3:46
MISSING MAIN! FUNCTION - can_import_nested_modules.md:1:1:24:48
MODULE NOT FOUND - can_import_nested_modules.md:1:1:1:19
MODULE NOT FOUND - can_import_nested_modules.md:2:1:2:19
MODULE NOT IMPORTED - can_import_nested_modules.md:6:15:6:30
UNDEFINED VARIABLE - can_import_nested_modules.md:7:26:7:41
MODULE NOT IMPORTED - can_import_nested_modules.md:10:28:10:42
UNDEFINED VARIABLE - can_import_nested_modules.md:11:29:11:43
MODULE NOT IMPORTED - can_import_nested_modules.md:14:15:14:37
MODULE NOT IMPORTED - can_import_nested_modules.md:14:58:14:77
UNDEFINED VARIABLE - can_import_nested_modules.md:16:5:16:37
UNDEFINED VARIABLE - can_import_nested_modules.md:20:23:20:30
UNDEFINED VARIABLE - can_import_nested_modules.md:20:37:20:58
MODULE NOT IMPORTED - can_import_nested_modules.md:23:16:23:36
MODULE NOT IMPORTED - can_import_nested_modules.md:23:47:23:61
MODULE NOT IMPORTED - can_import_nested_modules.md:23:63:23:77
UNDEFINED VARIABLE - can_import_nested_modules.md:24:24:24:41
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_import_nested_modules.md:1:19:1:26:**
```roc
import json.Parser.Config
```
                  ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_import_nested_modules.md:2:19:2:24:**
```roc
import http.Client.Auth as HttpAuth
```
                  ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_import_nested_modules.md:2:25:2:27:**
```roc
import http.Client.Auth as HttpAuth
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

**can_import_nested_modules.md:3:1:3:7:**
```roc
import utils.String.Format exposing [padLeft]
```
^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_import_nested_modules.md:3:8:3:13:**
```roc
import utils.String.Format exposing [padLeft]
```
       ^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_import_nested_modules.md:3:13:3:20:**
```roc
import utils.String.Format exposing [padLeft]
```
            ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_import_nested_modules.md:3:20:3:27:**
```roc
import utils.String.Format exposing [padLeft]
```
                   ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_import_nested_modules.md:3:28:3:36:**
```roc
import utils.String.Format exposing [padLeft]
```
                           ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_import_nested_modules.md:3:37:3:38:**
```roc
import utils.String.Format exposing [padLeft]
```
                                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_import_nested_modules.md:3:38:3:45:**
```roc
import utils.String.Format exposing [padLeft]
```
                                     ^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_import_nested_modules.md:3:45:3:46:**
```roc
import utils.String.Format exposing [padLeft]
```
                                            ^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**can_import_nested_modules.md:1:1:24:48:**
```roc
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
```


**MODULE NOT FOUND**
The module `json.Parser` was not found in this Roc project.

You're attempting to use this module here:
**can_import_nested_modules.md:1:1:1:19:**
```roc
import json.Parser.Config
```
^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `http.Client` was not found in this Roc project.

You're attempting to use this module here:
**can_import_nested_modules.md:2:1:2:19:**
```roc
import http.Client.Auth as HttpAuth
```
^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `Config` imported into this Roc file.

You're attempting to use this module here:
**can_import_nested_modules.md:6:15:6:30:**
```roc
parseConfig : Config.Settings -> Str
```
              ^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toString` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_nested_modules.md:7:26:7:41:**
```roc
parseConfig = |settings| Config.toString(settings)
```
                         ^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `HttpAuth` imported into this Roc file.

You're attempting to use this module here:
**can_import_nested_modules.md:10:28:10:42:**
```roc
authenticate : Str, Str -> HttpAuth.Token
```
                           ^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `login` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_nested_modules.md:11:29:11:43:**
```roc
authenticate = |user, pass| HttpAuth.login(user, pass)
```
                            ^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `import json.Parser.Config
import http.Client.Auth as HttpAuth
import utils.String.Format exposing [padLeft]

# Test multi-level type qualification
parseConfig : Config.Settings -> Str
parseConfig = |settings| Config.toString(settings)

# Test multi-level value qualification
authenticate : Str, Str -> HttpAuth.Token
authenticate = |user, pass| HttpAuth.login(user, pass)

# Test deeply nested qualification
processData : Config.Parser` imported into this Roc file.

You're attempting to use this module here:
**can_import_nested_modules.md:14:15:14:37:**
```roc
processData : Config.Parser.Advanced, Str -> Result(Str, Config.Parser.Error)
```
              ^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `import json.Parser.Config
import http.Client.Auth as HttpAuth
import utils.String.Format exposing [padLeft]

# Test multi-level type qualification
parseConfig : Config.Settings -> Str
parseConfig = |settings| Config.toString(settings)

# Test multi-level value qualification
authenticate : Str, Str -> HttpAuth.Token
authenticate = |user, pass| HttpAuth.login(user, pass)

# Test deeply nested qualification
processData : Config.Parser.Advanced, Str -> Result(Str, Config.Parser` imported into this Roc file.

You're attempting to use this module here:
**can_import_nested_modules.md:14:58:14:77:**
```roc
processData : Config.Parser.Advanced, Str -> Result(Str, Config.Parser.Error)
```
                                                         ^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `parseWith` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_nested_modules.md:16:5:16:37:**
```roc
    Config.Parser.Advanced.parseWith(advancedConfig, input)
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `padLeft` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_nested_modules.md:20:23:20:30:**
```roc
formatOutput = |text| padLeft(text, Config.defaultPadding)
```
                      ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `defaultPadding` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_nested_modules.md:20:37:20:58:**
```roc
formatOutput = |text| padLeft(text, Config.defaultPadding)
```
                                    ^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `HttpAuth` imported into this Roc file.

You're attempting to use this module here:
**can_import_nested_modules.md:23:16:23:36:**
```roc
validateAuth : HttpAuth.Credentials -> Result(HttpAuth.Token, HttpAuth.Error)
```
               ^^^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `HttpAuth` imported into this Roc file.

You're attempting to use this module here:
**can_import_nested_modules.md:23:47:23:61:**
```roc
validateAuth : HttpAuth.Credentials -> Result(HttpAuth.Token, HttpAuth.Error)
```
                                              ^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `HttpAuth` imported into this Roc file.

You're attempting to use this module here:
**can_import_nested_modules.md:23:63:23:77:**
```roc
validateAuth : HttpAuth.Credentials -> Result(HttpAuth.Token, HttpAuth.Error)
```
                                                              ^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `validate` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_nested_modules.md:24:24:24:41:**
```roc
validateAuth = |creds| HttpAuth.validate(creds)
```
                       ^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:19),NoSpaceDotUpperIdent(1:19-1:26),
KwImport(2:1-2:7),LowerIdent(2:8-2:12),NoSpaceDotUpperIdent(2:12-2:19),NoSpaceDotUpperIdent(2:19-2:24),KwAs(2:25-2:27),UpperIdent(2:28-2:36),
KwImport(3:1-3:7),LowerIdent(3:8-3:13),NoSpaceDotUpperIdent(3:13-3:20),NoSpaceDotUpperIdent(3:20-3:27),KwExposing(3:28-3:36),OpenSquare(3:37-3:38),LowerIdent(3:38-3:45),CloseSquare(3:45-3:46),
LowerIdent(6:1-6:12),OpColon(6:13-6:14),UpperIdent(6:15-6:21),NoSpaceDotUpperIdent(6:21-6:30),OpArrow(6:31-6:33),UpperIdent(6:34-6:37),
LowerIdent(7:1-7:12),OpAssign(7:13-7:14),OpBar(7:15-7:16),LowerIdent(7:16-7:24),OpBar(7:24-7:25),UpperIdent(7:26-7:32),NoSpaceDotLowerIdent(7:32-7:41),NoSpaceOpenRound(7:41-7:42),LowerIdent(7:42-7:50),CloseRound(7:50-7:51),
LowerIdent(10:1-10:13),OpColon(10:14-10:15),UpperIdent(10:16-10:19),Comma(10:19-10:20),UpperIdent(10:21-10:24),OpArrow(10:25-10:27),UpperIdent(10:28-10:36),NoSpaceDotUpperIdent(10:36-10:42),
LowerIdent(11:1-11:13),OpAssign(11:14-11:15),OpBar(11:16-11:17),LowerIdent(11:17-11:21),Comma(11:21-11:22),LowerIdent(11:23-11:27),OpBar(11:27-11:28),UpperIdent(11:29-11:37),NoSpaceDotLowerIdent(11:37-11:43),NoSpaceOpenRound(11:43-11:44),LowerIdent(11:44-11:48),Comma(11:48-11:49),LowerIdent(11:50-11:54),CloseRound(11:54-11:55),
LowerIdent(14:1-14:12),OpColon(14:13-14:14),UpperIdent(14:15-14:21),NoSpaceDotUpperIdent(14:21-14:28),NoSpaceDotUpperIdent(14:28-14:37),Comma(14:37-14:38),UpperIdent(14:39-14:42),OpArrow(14:43-14:45),UpperIdent(14:46-14:52),NoSpaceOpenRound(14:52-14:53),UpperIdent(14:53-14:56),Comma(14:56-14:57),UpperIdent(14:58-14:64),NoSpaceDotUpperIdent(14:64-14:71),NoSpaceDotUpperIdent(14:71-14:77),CloseRound(14:77-14:78),
LowerIdent(15:1-15:12),OpAssign(15:13-15:14),OpBar(15:15-15:16),LowerIdent(15:16-15:30),Comma(15:30-15:31),LowerIdent(15:32-15:37),OpBar(15:37-15:38),
UpperIdent(16:5-16:11),NoSpaceDotUpperIdent(16:11-16:18),NoSpaceDotUpperIdent(16:18-16:27),NoSpaceDotLowerIdent(16:27-16:37),NoSpaceOpenRound(16:37-16:38),LowerIdent(16:38-16:52),Comma(16:52-16:53),LowerIdent(16:54-16:59),CloseRound(16:59-16:60),
LowerIdent(19:1-19:13),OpColon(19:14-19:15),UpperIdent(19:16-19:19),OpArrow(19:20-19:22),UpperIdent(19:23-19:26),
LowerIdent(20:1-20:13),OpAssign(20:14-20:15),OpBar(20:16-20:17),LowerIdent(20:17-20:21),OpBar(20:21-20:22),LowerIdent(20:23-20:30),NoSpaceOpenRound(20:30-20:31),LowerIdent(20:31-20:35),Comma(20:35-20:36),UpperIdent(20:37-20:43),NoSpaceDotLowerIdent(20:43-20:58),CloseRound(20:58-20:59),
LowerIdent(23:1-23:13),OpColon(23:14-23:15),UpperIdent(23:16-23:24),NoSpaceDotUpperIdent(23:24-23:36),OpArrow(23:37-23:39),UpperIdent(23:40-23:46),NoSpaceOpenRound(23:46-23:47),UpperIdent(23:47-23:55),NoSpaceDotUpperIdent(23:55-23:61),Comma(23:61-23:62),UpperIdent(23:63-23:71),NoSpaceDotUpperIdent(23:71-23:77),CloseRound(23:77-23:78),
LowerIdent(24:1-24:13),OpAssign(24:14-24:15),OpBar(24:16-24:17),LowerIdent(24:17-24:22),OpBar(24:22-24:23),UpperIdent(24:24-24:32),NoSpaceDotLowerIdent(24:32-24:41),NoSpaceOpenRound(24:41-24:42),LowerIdent(24:42-24:47),CloseRound(24:47-24:48),
EndOfFile(25:1-25:1),
~~~
# PARSE
~~~clojure
(file @1.1-24.48
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.19 (raw "json.Parser"))
		(s-malformed @1.19-1.26 (tag "statement_unexpected_token"))
		(s-import @2.1-2.19 (raw "http.Client"))
		(s-malformed @2.19-2.24 (tag "statement_unexpected_token"))
		(s-malformed @2.25-2.27 (tag "statement_unexpected_token"))
		(s-malformed @3.1-3.7 (tag "expected_colon_after_type_annotation"))
		(s-malformed @3.8-3.13 (tag "statement_unexpected_token"))
		(s-malformed @3.13-3.20 (tag "statement_unexpected_token"))
		(s-malformed @3.20-3.27 (tag "statement_unexpected_token"))
		(s-malformed @3.28-3.36 (tag "statement_unexpected_token"))
		(s-malformed @3.37-3.38 (tag "statement_unexpected_token"))
		(s-malformed @3.38-3.45 (tag "statement_unexpected_token"))
		(s-malformed @3.45-3.46 (tag "statement_unexpected_token"))
		(s-type-anno @6.1-6.37 (name "parseConfig")
			(ty-fn @6.15-6.37
				(ty @6.15-6.30 (name "Config.Settings"))
				(ty @6.34-6.37 (name "Str"))))
		(s-decl @7.1-7.51
			(p-ident @7.1-7.12 (raw "parseConfig"))
			(e-lambda @7.15-7.51
				(args
					(p-ident @7.16-7.24 (raw "settings")))
				(e-apply @7.26-7.51
					(e-ident @7.26-7.41 (raw "Config.toString"))
					(e-ident @7.42-7.50 (raw "settings")))))
		(s-type-anno @10.1-10.42 (name "authenticate")
			(ty-fn @10.16-10.42
				(ty @10.16-10.19 (name "Str"))
				(ty @10.21-10.24 (name "Str"))
				(ty @10.28-10.42 (name "HttpAuth.Token"))))
		(s-decl @11.1-11.55
			(p-ident @11.1-11.13 (raw "authenticate"))
			(e-lambda @11.16-11.55
				(args
					(p-ident @11.17-11.21 (raw "user"))
					(p-ident @11.23-11.27 (raw "pass")))
				(e-apply @11.29-11.55
					(e-ident @11.29-11.43 (raw "HttpAuth.login"))
					(e-ident @11.44-11.48 (raw "user"))
					(e-ident @11.50-11.54 (raw "pass")))))
		(s-type-anno @14.1-14.78 (name "processData")
			(ty-fn @14.15-14.78
				(ty @14.15-14.37 (name "Config.Parser.Advanced"))
				(ty @14.39-14.42 (name "Str"))
				(ty-apply @14.46-14.78
					(ty @14.46-14.52 (name "Result"))
					(ty @14.53-14.56 (name "Str"))
					(ty @14.58-14.77 (name "Config.Parser.Error")))))
		(s-decl @15.1-16.60
			(p-ident @15.1-15.12 (raw "processData"))
			(e-lambda @15.15-16.60
				(args
					(p-ident @15.16-15.30 (raw "advancedConfig"))
					(p-ident @15.32-15.37 (raw "input")))
				(e-apply @16.5-16.60
					(e-ident @16.5-16.37 (raw "Config.Parser.Advanced.parseWith"))
					(e-ident @16.38-16.52 (raw "advancedConfig"))
					(e-ident @16.54-16.59 (raw "input")))))
		(s-type-anno @19.1-19.26 (name "formatOutput")
			(ty-fn @19.16-19.26
				(ty @19.16-19.19 (name "Str"))
				(ty @19.23-19.26 (name "Str"))))
		(s-decl @20.1-20.59
			(p-ident @20.1-20.13 (raw "formatOutput"))
			(e-lambda @20.16-20.59
				(args
					(p-ident @20.17-20.21 (raw "text")))
				(e-apply @20.23-20.59
					(e-ident @20.23-20.30 (raw "padLeft"))
					(e-ident @20.31-20.35 (raw "text"))
					(e-ident @20.37-20.58 (raw "Config.defaultPadding")))))
		(s-type-anno @23.1-23.78 (name "validateAuth")
			(ty-fn @23.16-23.78
				(ty @23.16-23.36 (name "HttpAuth.Credentials"))
				(ty-apply @23.40-23.78
					(ty @23.40-23.46 (name "Result"))
					(ty @23.47-23.61 (name "HttpAuth.Token"))
					(ty @23.63-23.77 (name "HttpAuth.Error")))))
		(s-decl @24.1-24.48
			(p-ident @24.1-24.13 (raw "validateAuth"))
			(e-lambda @24.16-24.48
				(args
					(p-ident @24.17-24.22 (raw "creds")))
				(e-apply @24.24-24.48
					(e-ident @24.24-24.41 (raw "HttpAuth.validate"))
					(e-ident @24.42-24.47 (raw "creds")))))))
~~~
# FORMATTED
~~~roc
import json.Parser

import http.Client



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
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @7.1-7.12 (ident "parseConfig"))
		(e-lambda @7.15-7.51
			(args
				(p-assign @7.16-7.24 (ident "settings")))
			(e-call @7.26-7.51
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local @7.42-7.50
					(p-assign @7.16-7.24 (ident "settings")))))
		(annotation @7.1-7.12
			(declared-type
				(ty-fn @6.15-6.37 (effectful false)
					(ty-malformed @6.15-6.30)
					(ty @6.34-6.37 (name "Str"))))))
	(d-let
		(p-assign @11.1-11.13 (ident "authenticate"))
		(e-lambda @11.16-11.55
			(args
				(p-assign @11.17-11.21 (ident "user"))
				(p-assign @11.23-11.27 (ident "pass")))
			(e-call @11.29-11.55
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local @11.44-11.48
					(p-assign @11.17-11.21 (ident "user")))
				(e-lookup-local @11.50-11.54
					(p-assign @11.23-11.27 (ident "pass")))))
		(annotation @11.1-11.13
			(declared-type
				(ty-fn @10.16-10.42 (effectful false)
					(ty @10.16-10.19 (name "Str"))
					(ty @10.21-10.24 (name "Str"))
					(ty-malformed @10.28-10.42)))))
	(d-let
		(p-assign @15.1-15.12 (ident "processData"))
		(e-lambda @15.15-16.60
			(args
				(p-assign @15.16-15.30 (ident "advancedConfig"))
				(p-assign @15.32-15.37 (ident "input")))
			(e-call @16.5-16.60
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local @16.38-16.52
					(p-assign @15.16-15.30 (ident "advancedConfig")))
				(e-lookup-local @16.54-16.59
					(p-assign @15.32-15.37 (ident "input")))))
		(annotation @15.1-15.12
			(declared-type
				(ty-fn @14.15-14.78 (effectful false)
					(ty-malformed @14.15-14.37)
					(ty @14.39-14.42 (name "Str"))
					(ty-apply @14.46-14.78 (symbol "Result")
						(ty @14.53-14.56 (name "Str"))
						(ty-malformed @14.58-14.77))))))
	(d-let
		(p-assign @20.1-20.13 (ident "formatOutput"))
		(e-lambda @20.16-20.59
			(args
				(p-assign @20.17-20.21 (ident "text")))
			(e-call @20.23-20.59
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local @20.31-20.35
					(p-assign @20.17-20.21 (ident "text")))
				(e-runtime-error (tag "ident_not_in_scope"))))
		(annotation @20.1-20.13
			(declared-type
				(ty-fn @19.16-19.26 (effectful false)
					(ty @19.16-19.19 (name "Str"))
					(ty @19.23-19.26 (name "Str"))))))
	(d-let
		(p-assign @24.1-24.13 (ident "validateAuth"))
		(e-lambda @24.16-24.48
			(args
				(p-assign @24.17-24.22 (ident "creds")))
			(e-call @24.24-24.48
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local @24.42-24.47
					(p-assign @24.17-24.22 (ident "creds")))))
		(annotation @24.1-24.13
			(declared-type
				(ty-fn @23.16-23.78 (effectful false)
					(ty-malformed @23.16-23.36)
					(ty-apply @23.40-23.78 (symbol "Result")
						(ty-malformed @23.47-23.61)
						(ty-malformed @23.63-23.77))))))
	(s-import @1.1-1.19 (module "json.Parser") (qualifier "json")
		(exposes))
	(s-import @2.1-2.19 (module "http.Client") (qualifier "http")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.12 (type "Error -> Str"))
		(patt @11.1-11.13 (type "Str, Str -> Error"))
		(patt @15.1-15.12 (type "Error, Str -> Result(Str, Error)"))
		(patt @20.1-20.13 (type "Str -> Str"))
		(patt @24.1-24.13 (type "Error -> Result(Error, Error)")))
	(expressions
		(expr @7.15-7.51 (type "Error -> Str"))
		(expr @11.16-11.55 (type "Str, Str -> Error"))
		(expr @15.15-16.60 (type "Error, Str -> Result(Str, Error)"))
		(expr @20.16-20.59 (type "Str -> Str"))
		(expr @24.16-24.48 (type "Error -> Result(Error, Error)"))))
~~~
