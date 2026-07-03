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
DUPLICATE DEFINITION - can_import_unresolved_qualified.md:1:1:1:17
NAME NOT IN SCOPE - can_import_unresolved_qualified.md:5:8:5:31
MODULE NOT FOUND - can_import_unresolved_qualified.md:8:17:8:29
NAME NOT IN SCOPE - can_import_unresolved_qualified.md:9:20:9:34
MODULE NOT FOUND - can_import_unresolved_qualified.md:12:29:12:37
MODULE NOT FOUND - can_import_unresolved_qualified.md:12:52:12:61
NAME NOT IN SCOPE - can_import_unresolved_qualified.md:13:24:13:51
UNUSED VARIABLE - can_import_unresolved_qualified.md:13:19:13:22
NAME NOT IN SCOPE - can_import_unresolved_qualified.md:16:10:16:20
DOES NOT EXIST - can_import_unresolved_qualified.md:19:10:19:31
NAME NOT IN SCOPE - can_import_unresolved_qualified.md:22:10:22:28
NAME NOT IN SCOPE - can_import_unresolved_qualified.md:25:10:25:49
# PROBLEMS

┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `Json` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  import json.Json                                                          │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                          │
 └──────────────────────────────────── can_import_unresolved_qualified.md:1:1 ┘

    In this scope, `Json` was already defined here:
      ┌───────────────────────────────────────────────────────────────────────┐
    1 │  import json.Json                                                     │
      │  ‾                                                                    │
      └─────────────────────────────── can_import_unresolved_qualified.md:1:1 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `method` in this scope. ──────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  main = Json.NonExistent.method                                            │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                            │
 └──────────────────────────────────── can_import_unresolved_qualified.md:5:8 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────┐
│ MODULE NOT FOUND ├─ This `InvalidType` type is declared to be in ───────────┐
└┬─────────────────┘  `json.Json`, which does not exist.                      │
 │                                                                            │
 │  parseData : Json.InvalidType -> Str                                       │
 │                  ‾‾‾‾‾‾‾‾‾‾‾‾                                              │
 └─────────────────────────────────── can_import_unresolved_qualified.md:8:17 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `stringify` in this scope. ───────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  parseData = |data| Json.stringify(data)                                   │
 │                     ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                         │
 └─────────────────────────────────── can_import_unresolved_qualified.md:9:20 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────┐
│ MODULE NOT FOUND ├─ This `Server.Request` type is declared to be in ────────┐
└┬─────────────────┘  `http.Client`, which does not exist.                    │
 │                                                                            │
 │  processRequest : Http.Server.Request -> Http.Server.Response              │
 │                              ‾‾‾‾‾‾‾‾                                      │
 └────────────────────────────────── can_import_unresolved_qualified.md:12:29 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `Server.Response` type is declared to be in ───────┐
└┬─────────────────┘  `http.Client`, which does not exist.                    │
 │                                                                            │
 │  processRequest : Http.Server.Request -> Http.Server.Response              │
 │                                                     ‾‾‾‾‾‾‾‾‾              │
 └────────────────────────────────── can_import_unresolved_qualified.md:12:52 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `defaultResponse` in this scope. ─────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  processRequest = |req| Http.Server.defaultResponse                        │
 │                         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                        │
 └────────────────────────────────── can_import_unresolved_qualified.md:13:24 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `req` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  processRequest = |req| Http.Server.defaultResponse                        │
 │                    ‾‾‾                                                     │
 └────────────────────────────────── can_import_unresolved_qualified.md:13:19 ┘

    If you don't need this variable, prefix it with an underscore like `_req`
    to suppress this warning.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `prase` in this scope. ───────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  result = Json.prase("test")                                               │
 │           ‾‾‾‾‾‾‾‾‾‾                                                       │
 └────────────────────────────────── can_import_unresolved_qualified.md:16:10 ┘

    Is it misspelled, or is there an import missing?


┌────────────────┐
│ DOES NOT EXIST ├─ `Unknown.Module.config` does not exist. ──────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  config = Unknown.Module.config                                            │
 │           ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                            │
 └────────────────────────────────── can_import_unresolved_qualified.md:19:10 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `invalidMethod` in this scope. ───────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  client = Http.invalidMethod                                               │
 │           ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                               │
 └────────────────────────────────── can_import_unresolved_qualified.md:22:10 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `create` in this scope. ──────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  parser = Json.Parser.Advanced.NonExistent.create                          │
 │           ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                          │
 └────────────────────────────────── can_import_unresolved_qualified.md:25:10 ┘

    Is it misspelled, or is there an import missing?

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
				(ty-malformed)
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
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
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
		(patt (type "Error -> Str"))
		(patt (type "Error -> Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error -> Str"))
		(expr (type "Error -> Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
