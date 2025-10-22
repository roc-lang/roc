# META
~~~ini
description=Nested module qualification
type=snippet
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
There is no module with the name `Config.Parser` imported into this Roc file.

You're attempting to use this module here:
**can_import_nested_modules.md:14:15:14:37:**
```roc
processData : Config.Parser.Advanced, Str -> Result(Str, Config.Parser.Error)
```
              ^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `Config.Parser` imported into this Roc file.

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
KwImport,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,NoSpaceDotLowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "json.Parser"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-import (raw "http.Client"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "parseConfig")
			(ty-fn
				(ty (name "Config.Settings"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "parseConfig"))
			(e-lambda
				(args
					(p-ident (raw "settings")))
				(e-apply
					(e-ident (raw "Config.toString"))
					(e-ident (raw "settings")))))
		(s-type-anno (name "authenticate")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))
				(ty (name "HttpAuth.Token"))))
		(s-decl
			(p-ident (raw "authenticate"))
			(e-lambda
				(args
					(p-ident (raw "user"))
					(p-ident (raw "pass")))
				(e-apply
					(e-ident (raw "HttpAuth.login"))
					(e-ident (raw "user"))
					(e-ident (raw "pass")))))
		(s-type-anno (name "processData")
			(ty-fn
				(ty (name "Config.Parser.Advanced"))
				(ty (name "Str"))
				(ty-apply
					(ty (name "Result"))
					(ty (name "Str"))
					(ty (name "Config.Parser.Error")))))
		(s-decl
			(p-ident (raw "processData"))
			(e-lambda
				(args
					(p-ident (raw "advancedConfig"))
					(p-ident (raw "input")))
				(e-apply
					(e-ident (raw "Config.Parser.Advanced.parseWith"))
					(e-ident (raw "advancedConfig"))
					(e-ident (raw "input")))))
		(s-type-anno (name "formatOutput")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "formatOutput"))
			(e-lambda
				(args
					(p-ident (raw "text")))
				(e-apply
					(e-ident (raw "padLeft"))
					(e-ident (raw "text"))
					(e-ident (raw "Config.defaultPadding")))))
		(s-type-anno (name "validateAuth")
			(ty-fn
				(ty (name "HttpAuth.Credentials"))
				(ty-apply
					(ty (name "Result"))
					(ty (name "HttpAuth.Token"))
					(ty (name "HttpAuth.Error")))))
		(s-decl
			(p-ident (raw "validateAuth"))
			(e-lambda
				(args
					(p-ident (raw "creds")))
				(e-apply
					(e-ident (raw "HttpAuth.validate"))
					(e-ident (raw "creds")))))))
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
		(p-assign (ident "parseConfig"))
		(e-lambda
			(args
				(p-assign (ident "settings")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "settings")))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "authenticate"))
		(e-lambda
			(args
				(p-assign (ident "user"))
				(p-assign (ident "pass")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "user")))
				(e-lookup-local
					(p-assign (ident "pass")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin))
				(ty-malformed))))
	(d-let
		(p-assign (ident "processData"))
		(e-lambda
			(args
				(p-assign (ident "advancedConfig"))
				(p-assign (ident "input")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "advancedConfig")))
				(e-lookup-local
					(p-assign (ident "input")))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Result") (external-module "Result")
					(ty-lookup (name "Str") (builtin))
					(ty-malformed)))))
	(d-let
		(p-assign (ident "formatOutput"))
		(e-lambda
			(args
				(p-assign (ident "text")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "text")))
				(e-runtime-error (tag "ident_not_in_scope"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "validateAuth"))
		(e-lambda
			(args
				(p-assign (ident "creds")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "creds")))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-apply (name "Result") (external-module "Result")
					(ty-malformed)
					(ty-malformed)))))
	(s-import (module "json.Parser")
		(exposes))
	(s-import (module "http.Client")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Error"))
		(patt (type "Str, Str -> Error"))
		(patt (type "Error, Str -> Error"))
		(patt (type "Str -> Error"))
		(patt (type "Error -> Error")))
	(expressions
		(expr (type "Error -> Error"))
		(expr (type "Str, Str -> Error"))
		(expr (type "Error, Str -> Error"))
		(expr (type "Str -> Error"))
		(expr (type "Error -> Error"))))
~~~
