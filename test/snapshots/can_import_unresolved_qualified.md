# META
~~~ini
description=Error handling for unresolved qualified names
type=file
~~~
# SOURCE
~~~roc
module []

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
MODULE HEADER DEPRECATED - can_import_unresolved_qualified.md:1:1:1:10
MODULE NOT FOUND - can_import_unresolved_qualified.md:3:1:3:17
MODULE NOT FOUND - can_import_unresolved_qualified.md:4:1:4:27
MODULE NOT IMPORTED - can_import_unresolved_qualified.md:14:18:14:37
MODULE NOT IMPORTED - can_import_unresolved_qualified.md:14:41:14:61
UNUSED VARIABLE - can_import_unresolved_qualified.md:15:19:15:22
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_import_unresolved_qualified.md:1:1:1:10:**
```roc
module []
```
^^^^^^^^^


**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**can_import_unresolved_qualified.md:3:1:3:17:**
```roc
import json.Json
```
^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `http.Client` was not found in this Roc project.

You're attempting to use this module here:
**can_import_unresolved_qualified.md:4:1:4:27:**
```roc
import http.Client as Http
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `module []

import json.Json
import http.Client as Http

# Test unresolved qualified value
main = Json.NonExistent.method

# Test unresolved qualified type in annotation
parseData : Json.InvalidType -> Str
parseData = |data| Json.stringify(data)

# Test unresolved nested qualification
processRequest : Http.Server` imported into this Roc file.

You're attempting to use this module here:
**can_import_unresolved_qualified.md:14:18:14:37:**
```roc
processRequest : Http.Server.Request -> Http.Server.Response
```
                 ^^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `module []

import json.Json
import http.Client as Http

# Test unresolved qualified value
main = Json.NonExistent.method

# Test unresolved qualified type in annotation
parseData : Json.InvalidType -> Str
parseData = |data| Json.stringify(data)

# Test unresolved nested qualification
processRequest : Http.Server.Request -> Http.Server` imported into this Roc file.

You're attempting to use this module here:
**can_import_unresolved_qualified.md:14:41:14:61:**
```roc
processRequest : Http.Server.Request -> Http.Server.Response
```
                                        ^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable `req` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_req` to suppress this warning.
The unused variable is declared here:
**can_import_unresolved_qualified.md:15:19:15:22:**
```roc
processRequest = |req| Http.Server.defaultResponse
```
                  ^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),
KwImport(4:1-4:7),LowerIdent(4:8-4:12),NoSpaceDotUpperIdent(4:12-4:19),KwAs(4:20-4:22),UpperIdent(4:23-4:27),
LowerIdent(7:1-7:5),OpAssign(7:6-7:7),UpperIdent(7:8-7:12),NoSpaceDotUpperIdent(7:12-7:24),NoSpaceDotLowerIdent(7:24-7:31),
LowerIdent(10:1-10:10),OpColon(10:11-10:12),UpperIdent(10:13-10:17),NoSpaceDotUpperIdent(10:17-10:29),OpArrow(10:30-10:32),UpperIdent(10:33-10:36),
LowerIdent(11:1-11:10),OpAssign(11:11-11:12),OpBar(11:13-11:14),LowerIdent(11:14-11:18),OpBar(11:18-11:19),UpperIdent(11:20-11:24),NoSpaceDotLowerIdent(11:24-11:34),NoSpaceOpenRound(11:34-11:35),LowerIdent(11:35-11:39),CloseRound(11:39-11:40),
LowerIdent(14:1-14:15),OpColon(14:16-14:17),UpperIdent(14:18-14:22),NoSpaceDotUpperIdent(14:22-14:29),NoSpaceDotUpperIdent(14:29-14:37),OpArrow(14:38-14:40),UpperIdent(14:41-14:45),NoSpaceDotUpperIdent(14:45-14:52),NoSpaceDotUpperIdent(14:52-14:61),
LowerIdent(15:1-15:15),OpAssign(15:16-15:17),OpBar(15:18-15:19),LowerIdent(15:19-15:22),OpBar(15:22-15:23),UpperIdent(15:24-15:28),NoSpaceDotUpperIdent(15:28-15:35),NoSpaceDotLowerIdent(15:35-15:51),
LowerIdent(18:1-18:7),OpAssign(18:8-18:9),UpperIdent(18:10-18:14),NoSpaceDotLowerIdent(18:14-18:20),NoSpaceOpenRound(18:20-18:21),StringStart(18:21-18:22),StringPart(18:22-18:26),StringEnd(18:26-18:27),CloseRound(18:27-18:28),
LowerIdent(21:1-21:7),OpAssign(21:8-21:9),UpperIdent(21:10-21:17),NoSpaceDotUpperIdent(21:17-21:24),NoSpaceDotLowerIdent(21:24-21:31),
LowerIdent(24:1-24:7),OpAssign(24:8-24:9),UpperIdent(24:10-24:14),NoSpaceDotLowerIdent(24:14-24:28),
LowerIdent(27:1-27:7),OpAssign(27:8-27:9),UpperIdent(27:10-27:14),NoSpaceDotUpperIdent(27:14-27:21),NoSpaceDotUpperIdent(27:21-27:30),NoSpaceDotUpperIdent(27:30-27:42),NoSpaceDotLowerIdent(27:42-27:49),
EndOfFile(28:1-28:1),
~~~
# PARSE
~~~clojure
(file @1.1-27.49
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @3.1-3.17 (raw "json.Json"))
		(s-import @4.1-4.27 (raw "http.Client") (alias "Http"))
		(s-decl @7.1-7.31
			(p-ident @7.1-7.5 (raw "main"))
			(e-ident @7.8-7.31 (raw "Json.NonExistent.method")))
		(s-type-anno @10.1-10.36 (name "parseData")
			(ty-fn @10.13-10.36
				(ty @10.13-10.29 (name "Json.InvalidType"))
				(ty @10.33-10.36 (name "Str"))))
		(s-decl @11.1-11.40
			(p-ident @11.1-11.10 (raw "parseData"))
			(e-lambda @11.13-11.40
				(args
					(p-ident @11.14-11.18 (raw "data")))
				(e-apply @11.20-11.40
					(e-ident @11.20-11.34 (raw "Json.stringify"))
					(e-ident @11.35-11.39 (raw "data")))))
		(s-type-anno @14.1-14.61 (name "processRequest")
			(ty-fn @14.18-14.61
				(ty @14.18-14.37 (name "Http.Server.Request"))
				(ty @14.41-14.61 (name "Http.Server.Response"))))
		(s-decl @15.1-15.51
			(p-ident @15.1-15.15 (raw "processRequest"))
			(e-lambda @15.18-15.51
				(args
					(p-ident @15.19-15.22 (raw "req")))
				(e-ident @15.24-15.51 (raw "Http.Server.defaultResponse"))))
		(s-decl @18.1-18.28
			(p-ident @18.1-18.7 (raw "result"))
			(e-apply @18.10-18.28
				(e-ident @18.10-18.20 (raw "Json.prase"))
				(e-string @18.21-18.27
					(e-string-part @18.22-18.26 (raw "test")))))
		(s-decl @21.1-21.31
			(p-ident @21.1-21.7 (raw "config"))
			(e-ident @21.10-21.31 (raw "Unknown.Module.config")))
		(s-decl @24.1-24.28
			(p-ident @24.1-24.7 (raw "client"))
			(e-ident @24.10-24.28 (raw "Http.invalidMethod")))
		(s-decl @27.1-27.49
			(p-ident @27.1-27.7 (raw "parser"))
			(e-ident @27.10-27.49 (raw "Json.Parser.Advanced.NonExistent.create")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @7.1-7.5 (ident "main"))
		(e-lookup-external @7.8-7.31
			(module-idx "0")
			(target-node-idx "0")))
	(d-let
		(p-assign @11.1-11.10 (ident "parseData"))
		(e-lambda @11.13-11.40
			(args
				(p-assign @11.14-11.18 (ident "data")))
			(e-call @11.20-11.40
				(e-lookup-external @11.20-11.34
					(module-idx "0")
					(target-node-idx "0"))
				(e-lookup-local @11.35-11.39
					(p-assign @11.14-11.18 (ident "data")))))
		(annotation @11.1-11.10
			(declared-type
				(ty-fn @10.13-10.36 (effectful false)
					(ty-lookup @10.13-10.29 (name "InvalidType") (external (module-idx "0") (target-node-idx "0")))
					(ty-lookup @10.33-10.36 (name "Str") (builtin))))))
	(d-let
		(p-assign @15.1-15.15 (ident "processRequest"))
		(e-lambda @15.18-15.51
			(args
				(p-assign @15.19-15.22 (ident "req")))
			(e-lookup-external @15.24-15.51
				(module-idx "1")
				(target-node-idx "0")))
		(annotation @15.1-15.15
			(declared-type
				(ty-fn @14.18-14.61 (effectful false)
					(ty-malformed @14.18-14.37)
					(ty-malformed @14.41-14.61)))))
	(d-let
		(p-assign @18.1-18.7 (ident "result"))
		(e-call @18.10-18.28
			(e-lookup-external @18.10-18.20
				(module-idx "0")
				(target-node-idx "0"))
			(e-string @18.21-18.27
				(e-literal @18.22-18.26 (string "test")))))
	(d-let
		(p-assign @21.1-21.7 (ident "config"))
		(e-lookup-local @21.10-21.31
			(p-assign @21.1-21.7 (ident "config"))))
	(d-let
		(p-assign @24.1-24.7 (ident "client"))
		(e-lookup-external @24.10-24.28
			(module-idx "1")
			(target-node-idx "0")))
	(d-let
		(p-assign @27.1-27.7 (ident "parser"))
		(e-lookup-external @27.10-27.49
			(module-idx "0")
			(target-node-idx "0")))
	(s-import @3.1-3.17 (module "json.Json") (qualifier "json")
		(exposes))
	(s-import @4.1-4.27 (module "http.Client") (qualifier "http") (alias "Http")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.5 (type "Error"))
		(patt @11.1-11.10 (type "Error -> Str"))
		(patt @15.1-15.15 (type "Error -> Error"))
		(patt @18.1-18.7 (type "_a"))
		(patt @21.1-21.7 (type "_a"))
		(patt @24.1-24.7 (type "Error"))
		(patt @27.1-27.7 (type "Error")))
	(expressions
		(expr @7.8-7.31 (type "Error"))
		(expr @11.13-11.40 (type "Error -> Str"))
		(expr @15.18-15.51 (type "Error -> Error"))
		(expr @18.10-18.28 (type "_a"))
		(expr @21.10-21.31 (type "_a"))
		(expr @24.10-24.28 (type "Error"))
		(expr @27.10-27.49 (type "Error"))))
~~~
