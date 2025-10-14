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
KwImport(1:1-1:7),LowerIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:17),
KwImport(2:1-2:7),LowerIdent(2:8-2:12),NoSpaceDotUpperIdent(2:12-2:19),KwAs(2:20-2:22),UpperIdent(2:23-2:27),KwExposing(2:28-2:36),OpenSquare(2:37-2:38),LowerIdent(2:38-2:41),Comma(2:41-2:42),LowerIdent(2:43-2:47),CloseSquare(2:47-2:48),
KwImport(3:1-3:7),LowerIdent(3:8-3:13),NoSpaceDotUpperIdent(3:13-3:20),KwAs(3:21-3:23),UpperIdent(3:24-3:27),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),OpenCurly(5:8-5:9),
LowerIdent(6:5-6:11),OpAssign(6:12-6:13),UpperIdent(6:14-6:18),NoSpaceDotLowerIdent(6:18-6:22),
LowerIdent(7:5-7:11),OpAssign(7:12-7:13),UpperIdent(7:14-7:18),NoSpaceDotLowerIdent(7:18-7:23),
LowerIdent(8:5-8:11),OpAssign(8:12-8:13),UpperIdent(8:14-8:17),NoSpaceDotLowerIdent(8:17-8:22),
LowerIdent(11:5-11:12),OpAssign(11:13-11:14),UpperIdent(11:15-11:19),NoSpaceDotLowerIdent(11:19-11:25),
LowerIdent(14:5-14:12),OpAssign(14:13-14:14),UpperIdent(14:15-14:19),NoSpaceDotLowerIdent(14:19-14:24),
LowerIdent(17:5-17:12),OpAssign(17:13-17:14),LowerIdent(17:15-17:18),
LowerIdent(18:5-18:12),OpAssign(18:13-18:14),LowerIdent(18:15-18:19),
LowerIdent(21:5-21:13),OpAssign(21:14-21:15),UpperIdent(21:16-21:19),NoSpaceDotLowerIdent(21:19-21:26),
OpenRound(23:5-23:6),
LowerIdent(24:9-24:15),Comma(24:15-24:16),
LowerIdent(25:9-25:15),Comma(25:15-25:16),
LowerIdent(26:9-26:15),Comma(26:15-26:16),
LowerIdent(27:9-27:16),Comma(27:16-27:17),
LowerIdent(28:9-28:16),Comma(28:16-28:17),
LowerIdent(29:9-29:16),Comma(29:16-29:17),
LowerIdent(30:9-30:16),Comma(30:16-30:17),
LowerIdent(31:9-31:17),Comma(31:17-31:18),
CloseRound(32:5-32:6),
CloseCurly(33:1-33:2),
EndOfFile(34:1-34:1),
~~~
# PARSE
~~~clojure
(file @1.1-33.2
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.17 (raw "json.Json"))
		(s-import @2.1-2.48 (raw "http.Client") (alias "Http")
			(exposing
				(exposed-lower-ident @2.38-2.41
					(text "get"))
				(exposed-lower-ident @2.43-2.47
					(text "post"))))
		(s-import @3.1-3.27 (raw "utils.String") (alias "Str"))
		(s-decl @5.1-33.2
			(p-ident @5.1-5.5 (raw "main"))
			(e-block @5.8-33.2
				(statements
					(s-decl @6.5-6.22
						(p-ident @6.5-6.11 (raw "client"))
						(e-ident @6.14-6.22 (raw "Http.get")))
					(s-decl @7.5-7.23
						(p-ident @7.5-7.11 (raw "parser"))
						(e-ident @7.14-7.23 (raw "Json.utf8")))
					(s-decl @8.5-8.22
						(p-ident @8.5-8.11 (raw "helper"))
						(e-ident @8.14-8.22 (raw "Str.trim")))
					(s-decl @11.5-11.25
						(p-ident @11.5-11.12 (raw "result1"))
						(e-ident @11.15-11.25 (raw "Json.parse")))
					(s-decl @14.5-14.24
						(p-ident @14.5-14.12 (raw "result2"))
						(e-ident @14.15-14.24 (raw "Http.post")))
					(s-decl @17.5-17.18
						(p-ident @17.5-17.12 (raw "result3"))
						(e-ident @17.15-17.18 (raw "get")))
					(s-decl @18.5-18.19
						(p-ident @18.5-18.12 (raw "result4"))
						(e-ident @18.15-18.19 (raw "post")))
					(s-decl @21.5-21.26
						(p-ident @21.5-21.13 (raw "combined"))
						(e-ident @21.16-21.26 (raw "Str.concat")))
					(e-tuple @23.5-32.6
						(e-ident @24.9-24.15 (raw "client"))
						(e-ident @25.9-25.15 (raw "parser"))
						(e-ident @26.9-26.15 (raw "helper"))
						(e-ident @27.9-27.16 (raw "result1"))
						(e-ident @28.9-28.16 (raw "result2"))
						(e-ident @29.9-29.16 (raw "result3"))
						(e-ident @30.9-30.16 (raw "result4"))
						(e-ident @31.9-31.17 (raw "combined"))))))))
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
		(p-assign @5.1-5.5 (ident "main"))
		(e-block @5.8-33.2
			(s-let @6.5-6.22
				(p-assign @6.5-6.11 (ident "client"))
				(e-lookup-external @6.14-6.22
					(module-idx "3")
					(target-node-idx "0")))
			(s-let @7.5-7.23
				(p-assign @7.5-7.11 (ident "parser"))
				(e-lookup-external @7.14-7.23
					(module-idx "2")
					(target-node-idx "0")))
			(s-let @8.5-8.22
				(p-assign @8.5-8.11 (ident "helper"))
				(e-lookup-external @8.14-8.22
					(module-idx "4")
					(target-node-idx "0")))
			(s-let @11.5-11.25
				(p-assign @11.5-11.12 (ident "result1"))
				(e-lookup-external @11.15-11.25
					(module-idx "2")
					(target-node-idx "0")))
			(s-let @14.5-14.24
				(p-assign @14.5-14.12 (ident "result2"))
				(e-lookup-external @14.15-14.24
					(module-idx "3")
					(target-node-idx "0")))
			(s-let @17.5-17.18
				(p-assign @17.5-17.12 (ident "result3"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(s-let @18.5-18.19
				(p-assign @18.5-18.12 (ident "result4"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(s-let @21.5-21.26
				(p-assign @21.5-21.13 (ident "combined"))
				(e-lookup-external @21.16-21.26
					(module-idx "4")
					(target-node-idx "0")))
			(e-tuple @23.5-32.6
				(elems
					(e-lookup-local @24.9-24.15
						(p-assign @6.5-6.11 (ident "client")))
					(e-lookup-local @25.9-25.15
						(p-assign @7.5-7.11 (ident "parser")))
					(e-lookup-local @26.9-26.15
						(p-assign @8.5-8.11 (ident "helper")))
					(e-lookup-local @27.9-27.16
						(p-assign @11.5-11.12 (ident "result1")))
					(e-lookup-local @28.9-28.16
						(p-assign @14.5-14.12 (ident "result2")))
					(e-lookup-local @29.9-29.16
						(p-assign @17.5-17.12 (ident "result3")))
					(e-lookup-local @30.9-30.16
						(p-assign @18.5-18.12 (ident "result4")))
					(e-lookup-local @31.9-31.17
						(p-assign @21.5-21.13 (ident "combined")))))))
	(s-import @1.1-1.17 (module "json.Json") (qualifier "json")
		(exposes))
	(s-import @2.1-2.48 (module "http.Client") (qualifier "http") (alias "Http")
		(exposes
			(exposed (name "get") (wildcard false))
			(exposed (name "post") (wildcard false))))
	(s-import @3.1-3.27 (module "utils.String") (qualifier "utils") (alias "Str")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "(Error, Error, Error, Error, Error, Error, Error, Error)")))
	(expressions
		(expr @5.8-33.2 (type "(Error, Error, Error, Error, Error, Error, Error, Error)"))))
~~~
