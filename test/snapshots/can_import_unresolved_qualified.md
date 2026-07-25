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

# Test unknown mod qualification
config = Unknown.Mod.config

# Test valid mod but invalid member
client = Http.invalidMethod

# Test deeply nested invalid qualification
parser = Json.Parser.Advanced.NonExistent.create
~~~
# EXPECTED
DUPLICATE DEFINITION - can_import_unresolved_qualified.md:1:1:1:17
DOES NOT EXIST - can_import_unresolved_qualified.md:5:25:5:31
MOD NOT FOUND - can_import_unresolved_qualified.md:8:17:8:29
DOES NOT EXIST - can_import_unresolved_qualified.md:9:25:9:34
MOD NOT FOUND - can_import_unresolved_qualified.md:12:29:12:37
MOD NOT FOUND - can_import_unresolved_qualified.md:12:52:12:61
DOES NOT EXIST - can_import_unresolved_qualified.md:13:36:13:51
UNUSED VARIABLE - can_import_unresolved_qualified.md:13:19:13:22
DOES NOT EXIST - can_import_unresolved_qualified.md:16:15:16:20
DOES NOT EXIST - can_import_unresolved_qualified.md:19:10:19:28
DOES NOT EXIST - can_import_unresolved_qualified.md:22:15:22:28
DOES NOT EXIST - can_import_unresolved_qualified.md:25:43:25:49
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


┌────────────────┐
│ DOES NOT EXIST ├─ `method` was not found in `Json`. ────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  main = Json.NonExistent.method                                            │
 │                          ‾‾‾‾‾‾                                            │
 └─────────────────────────────────── can_import_unresolved_qualified.md:5:25 ┘

    Check that `method` is spelled correctly and that `Json` exposes it.


┌──────────────────┐
│ MOD NOT FOUND ├─ This `InvalidType` type is declared to be in ───────────┐
└┬─────────────────┘  `json.Json`, which does not exist.                      │
 │                                                                            │
 │  parseData : Json.InvalidType -> Str                                       │
 │                  ‾‾‾‾‾‾‾‾‾‾‾‾                                              │
 └─────────────────────────────────── can_import_unresolved_qualified.md:8:17 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `stringify` was not found in `Json`. ─────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  parseData = |data| Json.stringify(data)                                   │
 │                          ‾‾‾‾‾‾‾‾‾                                         │
 └─────────────────────────────────── can_import_unresolved_qualified.md:9:25 ┘

    Check that `stringify` is spelled correctly and that `Json` exposes it.


┌──────────────────┐
│ MOD NOT FOUND ├─ This `Server.Request` type is declared to be in ────────┐
└┬─────────────────┘  `http.Client`, which does not exist.                    │
 │                                                                            │
 │  processRequest : Http.Server.Request -> Http.Server.Response              │
 │                              ‾‾‾‾‾‾‾‾                                      │
 └────────────────────────────────── can_import_unresolved_qualified.md:12:29 ┘



┌──────────────────┐
│ MOD NOT FOUND ├─ This `Server.Response` type is declared to be in ───────┐
└┬─────────────────┘  `http.Client`, which does not exist.                    │
 │                                                                            │
 │  processRequest : Http.Server.Request -> Http.Server.Response              │
 │                                                     ‾‾‾‾‾‾‾‾‾              │
 └────────────────────────────────── can_import_unresolved_qualified.md:12:52 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `defaultResponse` was not found in `Http`. ───────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  processRequest = |req| Http.Server.defaultResponse                        │
 │                                     ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                        │
 └────────────────────────────────── can_import_unresolved_qualified.md:13:36 ┘

    Check that `defaultResponse` is spelled correctly and that `Http` exposes
    it.


┌─────────────────┐
│ UNUSED VARIABLE ├─ Variable `req` is defined here and then never used. ─────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  processRequest = |req| Http.Server.defaultResponse                        │
 │                    ‾‾‾                                                     │
 └────────────────────────────────── can_import_unresolved_qualified.md:13:19 ┘

    If you don't need this variable, prefix it with an underscore like `_req`
    to suppress this warning.


┌────────────────┐
│ DOES NOT EXIST ├─ `prase` was not found in `Json`. ─────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  result = Json.prase("test")                                               │
 │                ‾‾‾‾‾                                                       │
 └────────────────────────────────── can_import_unresolved_qualified.md:16:15 ┘

    Check that `prase` is spelled correctly and that `Json` exposes it.


┌────────────────┐
│ DOES NOT EXIST ├─ `Unknown.Mod.config` does not exist. ─────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  config = Unknown.Mod.config                                               │
 │           ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                               │
 └────────────────────────────────── can_import_unresolved_qualified.md:19:10 ┘

    The name `Unknown` is not an imported mod or a type in scope.


    If `Unknown` is a mod, you may need to import it at the top of the file.
    For example:

        import Unknown


┌────────────────┐
│ DOES NOT EXIST ├─ `invalidMethod` was not found in `Http`. ─────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  client = Http.invalidMethod                                               │
 │                ‾‾‾‾‾‾‾‾‾‾‾‾‾                                               │
 └────────────────────────────────── can_import_unresolved_qualified.md:22:15 ┘

    Check that `invalidMethod` is spelled correctly and that `Http` exposes it.


┌────────────────┐
│ DOES NOT EXIST ├─ `create` was not found in `Json`. ────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  parser = Json.Parser.Advanced.NonExistent.create                          │
 │                                            ‾‾‾‾‾‾                          │
 └────────────────────────────────── can_import_unresolved_qualified.md:25:43 ┘

    Check that `create` is spelled correctly and that `Json` exposes it.

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
	(type-mod)
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
			(e-ident (raw "Unknown.Mod.config")))
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
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(d-let
		(p-assign (ident "parseData"))
		(e-lambda
			(args
				(p-assign (ident "data")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "processRequest"))
		(e-lambda
			(args
				(p-assign (ident "req")))
			(e-runtime-error (tag "qualified_ident_does_not_exist")))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed))))
	(d-let
		(p-assign (ident "result"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "config"))
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(d-let
		(p-assign (ident "client"))
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(d-let
		(p-assign (ident "parser"))
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(s-import (mod "json.Json")
		(exposes))
	(s-import (mod "http.Client")
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
