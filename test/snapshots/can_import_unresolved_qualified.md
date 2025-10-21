# META
~~~ini
description=Error handling for unresolved qualified names
type=snippet
~~~
# SOURCE
~~~roc
import json.Json
import http.Client as Http

# Test unresolved qualified value
main = Json.NonExistent.method

# Test unresolved qualified type in annotation
parseData : Json.InvalidType -> Str
parseData = |data| Json.stringify(data)

# Test unresolved nested qualification
processRequest : Http.Server.Request -> Http.Server.Response
processRequest = |req| Http.Server.defaultResponse

# Test typo in qualified name
result = Json.prase("test")

# Test unknown module qualification
config = Unknown.Module.config

# Test valid module but invalid member
client = Http.invalidMethod

# Test deeply nested invalid qualification
parser = Json.Parser.Advanced.NonExistent.create
~~~
# EXPECTED
MODULE NOT FOUND - can_import_unresolved_qualified.md:1:1:1:17
MODULE NOT FOUND - can_import_unresolved_qualified.md:2:1:2:27
UNDEFINED VARIABLE - can_import_unresolved_qualified.md:5:8:5:31
UNDEFINED VARIABLE - can_import_unresolved_qualified.md:9:20:9:34
MODULE NOT IMPORTED - can_import_unresolved_qualified.md:12:18:12:37
MODULE NOT IMPORTED - can_import_unresolved_qualified.md:12:41:12:61
UNDEFINED VARIABLE - can_import_unresolved_qualified.md:13:24:13:51
UNUSED VARIABLE - can_import_unresolved_qualified.md:13:19:13:22
UNDEFINED VARIABLE - can_import_unresolved_qualified.md:16:10:16:20
UNDEFINED VARIABLE - can_import_unresolved_qualified.md:22:10:22:28
UNDEFINED VARIABLE - can_import_unresolved_qualified.md:25:10:25:49
# PROBLEMS
**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**can_import_unresolved_qualified.md:1:1:1:17:**
```roc
import json.Json
```
^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `http.Client` was not found in this Roc project.

You're attempting to use this module here:
**can_import_unresolved_qualified.md:2:1:2:27:**
```roc
import http.Client as Http
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `method` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_unresolved_qualified.md:5:8:5:31:**
```roc
main = Json.NonExistent.method
```
       ^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `stringify` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_unresolved_qualified.md:9:20:9:34:**
```roc
parseData = |data| Json.stringify(data)
```
                   ^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `Http.Server` imported into this Roc file.

You're attempting to use this module here:
**can_import_unresolved_qualified.md:12:18:12:37:**
```roc
processRequest : Http.Server.Request -> Http.Server.Response
```
                 ^^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `Http.Server` imported into this Roc file.

You're attempting to use this module here:
**can_import_unresolved_qualified.md:12:41:12:61:**
```roc
processRequest : Http.Server.Request -> Http.Server.Response
```
                                        ^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `defaultResponse` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_unresolved_qualified.md:13:24:13:51:**
```roc
processRequest = |req| Http.Server.defaultResponse
```
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `req` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_req` to suppress this warning.
The unused variable is declared here:
**can_import_unresolved_qualified.md:13:19:13:22:**
```roc
processRequest = |req| Http.Server.defaultResponse
```
                  ^^^


**UNDEFINED VARIABLE**
Nothing is named `prase` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_unresolved_qualified.md:16:10:16:20:**
```roc
result = Json.prase("test")
```
         ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `invalidMethod` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_unresolved_qualified.md:22:10:22:28:**
```roc
client = Http.invalidMethod
```
         ^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `create` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_unresolved_qualified.md:25:10:25:49:**
```roc
parser = Json.Parser.Advanced.NonExistent.create
```
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "json.Json"))
		(s-import (raw "http.Client") (alias "Http"))
		(s-decl
			(p-ident (raw "main"))
			(e-ident (raw "Json.NonExistent.method")))
		(s-type-anno (name "parseData")
			(ty-fn
				(ty (name "Json.InvalidType"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "parseData"))
			(e-lambda
				(args
					(p-ident (raw "data")))
				(e-apply
					(e-ident (raw "Json.stringify"))
					(e-ident (raw "data")))))
		(s-type-anno (name "processRequest")
			(ty-fn
				(ty (name "Http.Server.Request"))
				(ty (name "Http.Server.Response"))))
		(s-decl
			(p-ident (raw "processRequest"))
			(e-lambda
				(args
					(p-ident (raw "req")))
				(e-ident (raw "Http.Server.defaultResponse"))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "Json.prase"))
				(e-string
					(e-string-part (raw "test")))))
		(s-decl
			(p-ident (raw "config"))
			(e-ident (raw "Unknown.Module.config")))
		(s-decl
			(p-ident (raw "client"))
			(e-ident (raw "Http.invalidMethod")))
		(s-decl
			(p-ident (raw "parser"))
			(e-ident (raw "Json.Parser.Advanced.NonExistent.create")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "parseData"))
		(e-lambda
			(args
				(p-assign (ident "data")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "data")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "InvalidType") (external (module-idx "4") (target-node-idx "0")))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "processRequest"))
		(e-lambda
			(args
				(p-assign (ident "req")))
			(e-runtime-error (tag "ident_not_in_scope")))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-string
				(e-literal (string "test")))))
	(d-let
		(p-assign (ident "config"))
		(e-lookup-local
			(p-assign (ident "config"))))
	(d-let
		(p-assign (ident "client"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "parser"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-import (module "json.Json")
		(exposes))
	(s-import (module "http.Client")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error -> Error"))
		(patt (type "Error -> Error"))
		(patt (type "Error"))
		(patt (type "_a"))
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error -> Error"))
		(expr (type "Error -> Error"))
		(expr (type "Error"))
		(expr (type "_a"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
