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
MODULE NOT FOUND - can_import_comprehensive.md:1:1:1:17
MODULE NOT FOUND - can_import_comprehensive.md:2:1:2:48
MODULE NOT FOUND - can_import_comprehensive.md:3:1:3:27
UNDEFINED VARIABLE - can_import_comprehensive.md:17:15:17:18
UNDEFINED VARIABLE - can_import_comprehensive.md:18:15:18:19
# PROBLEMS
**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**can_import_comprehensive.md:1:1:1:17:**
```roc
import json.Json
```
^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `http.Client` was not found in this Roc project.

You're attempting to use this module here:
**can_import_comprehensive.md:2:1:2:48:**
```roc
import http.Client as Http exposing [get, post]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `utils.String` was not found in this Roc project.

You're attempting to use this module here:
**can_import_comprehensive.md:3:1:3:27:**
```roc
import utils.String as Str
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `get` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_comprehensive.md:17:15:17:18:**
```roc
    result3 = get
```
              ^^^


**UNDEFINED VARIABLE**
Nothing is named `post` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_comprehensive.md:18:15:18:19:**
```roc
    result4 = post
```
              ^^^^


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
				(e-lookup-external
					(module-idx "5")
					(target-node-idx "0")))
			(s-let
				(p-assign (ident "parser"))
				(e-lookup-external
					(module-idx "4")
					(target-node-idx "0")))
			(s-let
				(p-assign (ident "helper"))
				(e-lookup-external
					(module-idx "6")
					(target-node-idx "0")))
			(s-let
				(p-assign (ident "result1"))
				(e-lookup-external
					(module-idx "4")
					(target-node-idx "0")))
			(s-let
				(p-assign (ident "result2"))
				(e-lookup-external
					(module-idx "5")
					(target-node-idx "0")))
			(s-let
				(p-assign (ident "result3"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(s-let
				(p-assign (ident "result4"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(s-let
				(p-assign (ident "combined"))
				(e-lookup-external
					(module-idx "6")
					(target-node-idx "0")))
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
