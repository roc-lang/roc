# META
~~~ini
description=Error handling for unresolved qualified names
type=file
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
MISSING MAIN! FUNCTION - can_import_unresolved_qualified.md:1:1:25:49
MODULE NOT FOUND - can_import_unresolved_qualified.md:1:1:1:17
MODULE NOT FOUND - can_import_unresolved_qualified.md:2:1:2:27
MODULE NOT IMPORTED - can_import_unresolved_qualified.md:12:18:12:37
MODULE NOT IMPORTED - can_import_unresolved_qualified.md:12:41:12:61
UNUSED VARIABLE - can_import_unresolved_qualified.md:13:19:13:22
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**can_import_unresolved_qualified.md:1:1:25:49:**
```roc
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
```


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


**MODULE NOT IMPORTED**
There is no module with the name `import json.Json
import http.Client as Http

# Test unresolved qualified value
main = Json.NonExistent.method

# Test unresolved qualified type in annotation
parseData : Json.InvalidType -> Str
parseData = |data| Json.stringify(data)

# Test unresolved nested qualification
processRequest : Http.Server` imported into this Roc file.

You're attempting to use this module here:
**can_import_unresolved_qualified.md:12:18:12:37:**
```roc
processRequest : Http.Server.Request -> Http.Server.Response
```
                 ^^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `import json.Json
import http.Client as Http

# Test unresolved qualified value
main = Json.NonExistent.method

# Test unresolved qualified type in annotation
parseData : Json.InvalidType -> Str
parseData = |data| Json.stringify(data)

# Test unresolved nested qualification
processRequest : Http.Server.Request -> Http.Server` imported into this Roc file.

You're attempting to use this module here:
**can_import_unresolved_qualified.md:12:41:12:61:**
```roc
processRequest : Http.Server.Request -> Http.Server.Response
```
                                        ^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `req` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_req` to suppress this warning.
The unused variable is declared here:
**can_import_unresolved_qualified.md:13:19:13:22:**
```roc
processRequest = |req| Http.Server.defaultResponse
```
                  ^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:17),
KwImport(2:1-2:7),LowerIdent(2:8-2:12),NoSpaceDotUpperIdent(2:12-2:19),KwAs(2:20-2:22),UpperIdent(2:23-2:27),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),UpperIdent(5:8-5:12),NoSpaceDotUpperIdent(5:12-5:24),NoSpaceDotLowerIdent(5:24-5:31),
LowerIdent(8:1-8:10),OpColon(8:11-8:12),UpperIdent(8:13-8:17),NoSpaceDotUpperIdent(8:17-8:29),OpArrow(8:30-8:32),UpperIdent(8:33-8:36),
LowerIdent(9:1-9:10),OpAssign(9:11-9:12),OpBar(9:13-9:14),LowerIdent(9:14-9:18),OpBar(9:18-9:19),UpperIdent(9:20-9:24),NoSpaceDotLowerIdent(9:24-9:34),NoSpaceOpenRound(9:34-9:35),LowerIdent(9:35-9:39),CloseRound(9:39-9:40),
LowerIdent(12:1-12:15),OpColon(12:16-12:17),UpperIdent(12:18-12:22),NoSpaceDotUpperIdent(12:22-12:29),NoSpaceDotUpperIdent(12:29-12:37),OpArrow(12:38-12:40),UpperIdent(12:41-12:45),NoSpaceDotUpperIdent(12:45-12:52),NoSpaceDotUpperIdent(12:52-12:61),
LowerIdent(13:1-13:15),OpAssign(13:16-13:17),OpBar(13:18-13:19),LowerIdent(13:19-13:22),OpBar(13:22-13:23),UpperIdent(13:24-13:28),NoSpaceDotUpperIdent(13:28-13:35),NoSpaceDotLowerIdent(13:35-13:51),
LowerIdent(16:1-16:7),OpAssign(16:8-16:9),UpperIdent(16:10-16:14),NoSpaceDotLowerIdent(16:14-16:20),NoSpaceOpenRound(16:20-16:21),StringStart(16:21-16:22),StringPart(16:22-16:26),StringEnd(16:26-16:27),CloseRound(16:27-16:28),
LowerIdent(19:1-19:7),OpAssign(19:8-19:9),UpperIdent(19:10-19:17),NoSpaceDotUpperIdent(19:17-19:24),NoSpaceDotLowerIdent(19:24-19:31),
LowerIdent(22:1-22:7),OpAssign(22:8-22:9),UpperIdent(22:10-22:14),NoSpaceDotLowerIdent(22:14-22:28),
LowerIdent(25:1-25:7),OpAssign(25:8-25:9),UpperIdent(25:10-25:14),NoSpaceDotUpperIdent(25:14-25:21),NoSpaceDotUpperIdent(25:21-25:30),NoSpaceDotUpperIdent(25:30-25:42),NoSpaceDotLowerIdent(25:42-25:49),
EndOfFile(26:1-26:1),
~~~
# PARSE
~~~clojure
(file @1.1-25.49
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.17 (raw "json.Json"))
		(s-import @2.1-2.27 (raw "http.Client") (alias "Http"))
		(s-decl @5.1-5.31
			(p-ident @5.1-5.5 (raw "main"))
			(e-ident @5.8-5.31 (raw "Json.NonExistent.method")))
		(s-type-anno @8.1-8.36 (name "parseData")
			(ty-fn @8.13-8.36
				(ty @8.13-8.29 (name "Json.InvalidType"))
				(ty @8.33-8.36 (name "Str"))))
		(s-decl @9.1-9.40
			(p-ident @9.1-9.10 (raw "parseData"))
			(e-lambda @9.13-9.40
				(args
					(p-ident @9.14-9.18 (raw "data")))
				(e-apply @9.20-9.40
					(e-ident @9.20-9.34 (raw "Json.stringify"))
					(e-ident @9.35-9.39 (raw "data")))))
		(s-type-anno @12.1-12.61 (name "processRequest")
			(ty-fn @12.18-12.61
				(ty @12.18-12.37 (name "Http.Server.Request"))
				(ty @12.41-12.61 (name "Http.Server.Response"))))
		(s-decl @13.1-13.51
			(p-ident @13.1-13.15 (raw "processRequest"))
			(e-lambda @13.18-13.51
				(args
					(p-ident @13.19-13.22 (raw "req")))
				(e-ident @13.24-13.51 (raw "Http.Server.defaultResponse"))))
		(s-decl @16.1-16.28
			(p-ident @16.1-16.7 (raw "result"))
			(e-apply @16.10-16.28
				(e-ident @16.10-16.20 (raw "Json.prase"))
				(e-string @16.21-16.27
					(e-string-part @16.22-16.26 (raw "test")))))
		(s-decl @19.1-19.31
			(p-ident @19.1-19.7 (raw "config"))
			(e-ident @19.10-19.31 (raw "Unknown.Module.config")))
		(s-decl @22.1-22.28
			(p-ident @22.1-22.7 (raw "client"))
			(e-ident @22.10-22.28 (raw "Http.invalidMethod")))
		(s-decl @25.1-25.49
			(p-ident @25.1-25.7 (raw "parser"))
			(e-ident @25.10-25.49 (raw "Json.Parser.Advanced.NonExistent.create")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.5 (ident "main"))
		(e-lookup-external @5.8-5.31
			(module-idx "0")
			(target-node-idx "0")))
	(d-let
		(p-assign @9.1-9.10 (ident "parseData"))
		(e-lambda @9.13-9.40
			(args
				(p-assign @9.14-9.18 (ident "data")))
			(e-call @9.20-9.40
				(e-lookup-external @9.20-9.34
					(module-idx "0")
					(target-node-idx "0"))
				(e-lookup-local @9.35-9.39
					(p-assign @9.14-9.18 (ident "data")))))
		(annotation @9.1-9.10
			(declared-type
				(ty-fn @8.13-8.36 (effectful false)
					(ty-lookup-external @8.13-8.29
						(module-idx "0")
						(target-node-idx "0"))
					(ty @8.33-8.36 (name "Str"))))))
	(d-let
		(p-assign @13.1-13.15 (ident "processRequest"))
		(e-lambda @13.18-13.51
			(args
				(p-assign @13.19-13.22 (ident "req")))
			(e-lookup-external @13.24-13.51
				(module-idx "1")
				(target-node-idx "0")))
		(annotation @13.1-13.15
			(declared-type
				(ty-fn @12.18-12.61 (effectful false)
					(ty-malformed @12.18-12.37)
					(ty-malformed @12.41-12.61)))))
	(d-let
		(p-assign @16.1-16.7 (ident "result"))
		(e-call @16.10-16.28
			(e-lookup-external @16.10-16.20
				(module-idx "0")
				(target-node-idx "0"))
			(e-string @16.21-16.27
				(e-literal @16.22-16.26 (string "test")))))
	(d-let
		(p-assign @19.1-19.7 (ident "config"))
		(e-lookup-local @19.10-19.31
			(p-assign @19.1-19.7 (ident "config"))))
	(d-let
		(p-assign @22.1-22.7 (ident "client"))
		(e-lookup-external @22.10-22.28
			(module-idx "1")
			(target-node-idx "0")))
	(d-let
		(p-assign @25.1-25.7 (ident "parser"))
		(e-lookup-external @25.10-25.49
			(module-idx "0")
			(target-node-idx "0")))
	(s-import @1.1-1.17 (module "json.Json") (qualifier "json")
		(exposes))
	(s-import @2.1-2.27 (module "http.Client") (qualifier "http") (alias "Http")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "Error"))
		(patt @9.1-9.10 (type "Error -> Str"))
		(patt @13.1-13.15 (type "Error -> Error"))
		(patt @16.1-16.7 (type "_a"))
		(patt @19.1-19.7 (type "_a"))
		(patt @22.1-22.7 (type "Error"))
		(patt @25.1-25.7 (type "Error")))
	(expressions
		(expr @5.8-5.31 (type "Error"))
		(expr @9.13-9.40 (type "Error -> Str"))
		(expr @13.18-13.51 (type "Error -> Error"))
		(expr @16.10-16.28 (type "_a"))
		(expr @19.10-19.31 (type "_a"))
		(expr @22.10-22.28 (type "Error"))
		(expr @25.10-25.49 (type "Error"))))
~~~
