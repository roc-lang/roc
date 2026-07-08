# META
~~~ini
description=Comprehensive import test with various module access patterns
type=snippet
~~~
# SOURCE
~~~roc
import json.Json
import http.Client as Http exposing [get, post]
import utils.String as Str

main = {
    client = Http.get
    parser = Json.utf8
    helper = Str.trim

    # Test direct module access
    result1 = Json.parse

    # Test aliased module access
    result2 = Http.post

    # Test exposed items (should work without module prefix)
    result3 = get
    result4 = post

    # Test multiple qualified access
    combined = Str.concat

    (
        client,
        parser,
        helper,
        result1,
        result2,
        result3,
        result4,
        combined,
    )
}
~~~
# EXPECTED
DUPLICATE DEFINITION - can_import_comprehensive.md:1:1:1:17
DUPLICATE DEFINITION - can_import_comprehensive.md:3:1:3:27
NAME NOT IN SCOPE - can_import_comprehensive.md:6:14:6:22
NAME NOT IN SCOPE - can_import_comprehensive.md:7:14:7:23
NAME NOT IN SCOPE - can_import_comprehensive.md:8:14:8:22
NAME NOT IN SCOPE - can_import_comprehensive.md:11:15:11:25
NAME NOT IN SCOPE - can_import_comprehensive.md:14:15:14:24
NAME NOT IN SCOPE - can_import_comprehensive.md:17:15:17:18
NAME NOT IN SCOPE - can_import_comprehensive.md:18:15:18:19
NAME NOT IN SCOPE - can_import_comprehensive.md:21:16:21:26
# PROBLEMS

┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `Json` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  import json.Json                                                          │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                          │
 └─────────────────────────────────────────── can_import_comprehensive.md:1:1 ┘

    In this scope, `Json` was already defined here:
      ┌───────────────────────────────────────────────────────────────────────┐
    1 │  import json.Json                                                     │
      │  ‾                                                                    │
      └────────────────────────────────────── can_import_comprehensive.md:1:1 ┘


┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `Str` is being redeclared here. ───────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  import utils.String as Str                                                │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                │
 └─────────────────────────────────────────── can_import_comprehensive.md:3:1 ┘

    In this scope, `Str` was already defined here:
      ┌───────────────────────────────────────────────────────────────────────┐
    1 │  import json.Json                                                     │
      │  ‾                                                                    │
      └────────────────────────────────────── can_import_comprehensive.md:1:1 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `get` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  client = Http.get                                                         │
 │           ‾‾‾‾‾‾‾‾                                                         │
 └────────────────────────────────────────── can_import_comprehensive.md:6:14 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `utf8` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  parser = Json.utf8                                                        │
 │           ‾‾‾‾‾‾‾‾‾                                                        │
 └────────────────────────────────────────── can_import_comprehensive.md:7:14 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `trim` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  helper = Str.trim                                                         │
 │           ‾‾‾‾‾‾‾‾                                                         │
 └────────────────────────────────────────── can_import_comprehensive.md:8:14 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `parse` in this scope. ───────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  result1 = Json.parse                                                      │
 │            ‾‾‾‾‾‾‾‾‾‾                                                      │
 └───────────────────────────────────────── can_import_comprehensive.md:11:15 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `post` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  result2 = Http.post                                                       │
 │            ‾‾‾‾‾‾‾‾‾                                                       │
 └───────────────────────────────────────── can_import_comprehensive.md:14:15 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `get` in this scope. ─────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  result3 = get                                                             │
 │            ‾‾‾                                                             │
 └───────────────────────────────────────── can_import_comprehensive.md:17:15 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `post` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  result4 = post                                                            │
 │            ‾‾‾‾                                                            │
 └───────────────────────────────────────── can_import_comprehensive.md:18:15 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `concat` in this scope. ──────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  combined = Str.concat                                                     │
 │             ‾‾‾‾‾‾‾‾‾‾                                                     │
 └───────────────────────────────────────── can_import_comprehensive.md:21:16 ┘

    Is it misspelled, or is there an import missing?

# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,KwExposing,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
OpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "json.Json"))
		(s-import (raw "http.Client") (alias "Http")
			(exposing
				(exposed-lower-ident
					(text "get"))
				(exposed-lower-ident
					(text "post"))))
		(s-import (raw "utils.String") (alias "Str"))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "client"))
						(e-ident (raw "Http.get")))
					(s-decl
						(p-ident (raw "parser"))
						(e-ident (raw "Json.utf8")))
					(s-decl
						(p-ident (raw "helper"))
						(e-ident (raw "Str.trim")))
					(s-decl
						(p-ident (raw "result1"))
						(e-ident (raw "Json.parse")))
					(s-decl
						(p-ident (raw "result2"))
						(e-ident (raw "Http.post")))
					(s-decl
						(p-ident (raw "result3"))
						(e-ident (raw "get")))
					(s-decl
						(p-ident (raw "result4"))
						(e-ident (raw "post")))
					(s-decl
						(p-ident (raw "combined"))
						(e-ident (raw "Str.concat")))
					(e-tuple
						(e-ident (raw "client"))
						(e-ident (raw "parser"))
						(e-ident (raw "helper"))
						(e-ident (raw "result1"))
						(e-ident (raw "result2"))
						(e-ident (raw "result3"))
						(e-ident (raw "result4"))
						(e-ident (raw "combined"))))))))
~~~
# FORMATTED
~~~roc
import json.Json
import http.Client as Http exposing [get, post]
import utils.String as Str

main = {
	client = Http.get
	parser = Json.utf8
	helper = Str.trim

	# Test direct module access
	result1 = Json.parse

	# Test aliased module access
	result2 = Http.post

	# Test exposed items (should work without module prefix)
	result3 = get
	result4 = post

	# Test multiple qualified access
	combined = Str.concat

	(
		client,
		parser,
		helper,
		result1,
		result2,
		result3,
		result4,
		combined,
	)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "client"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(s-let
				(p-assign (ident "parser"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(s-let
				(p-assign (ident "helper"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(s-let
				(p-assign (ident "result1"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(s-let
				(p-assign (ident "result2"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(s-let
				(p-assign (ident "result3"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(s-let
				(p-assign (ident "result4"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(s-let
				(p-assign (ident "combined"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(e-tuple
				(elems
					(e-lookup-local
						(p-assign (ident "client")))
					(e-lookup-local
						(p-assign (ident "parser")))
					(e-lookup-local
						(p-assign (ident "helper")))
					(e-lookup-local
						(p-assign (ident "result1")))
					(e-lookup-local
						(p-assign (ident "result2")))
					(e-lookup-local
						(p-assign (ident "result3")))
					(e-lookup-local
						(p-assign (ident "result4")))
					(e-lookup-local
						(p-assign (ident "combined")))))))
	(s-import (module "json.Json")
		(exposes))
	(s-import (module "http.Client")
		(exposes
			(exposed (name "get") (wildcard false))
			(exposed (name "post") (wildcard false))))
	(s-import (module "utils.String")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Error, Error, Error, Error, Error, Error, Error, Error)")))
	(expressions
		(expr (type "(Error, Error, Error, Error, Error, Error, Error, Error)"))))
~~~
