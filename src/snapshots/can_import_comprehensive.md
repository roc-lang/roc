# META
~~~ini
description=Comprehensive import test with various module access patterns
type=file
~~~
# SOURCE
~~~roc
module []

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
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),Newline(1:1-1:1),
KwImport(4:1-4:7),LowerIdent(4:8-4:12),NoSpaceDotUpperIdent(4:12-4:19),KwAs(4:20-4:22),UpperIdent(4:23-4:27),KwExposing(4:28-4:36),OpenSquare(4:37-4:38),LowerIdent(4:38-4:41),Comma(4:41-4:42),LowerIdent(4:43-4:47),CloseSquare(4:47-4:48),Newline(1:1-1:1),
KwImport(5:1-5:7),LowerIdent(5:8-5:13),NoSpaceDotUpperIdent(5:13-5:20),KwAs(5:21-5:23),UpperIdent(5:24-5:27),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(7:1-7:5),OpAssign(7:6-7:7),OpenCurly(7:8-7:9),Newline(1:1-1:1),
LowerIdent(8:5-8:11),OpAssign(8:12-8:13),UpperIdent(8:14-8:18),NoSpaceDotLowerIdent(8:18-8:22),Newline(1:1-1:1),
LowerIdent(9:5-9:11),OpAssign(9:12-9:13),UpperIdent(9:14-9:18),NoSpaceDotLowerIdent(9:18-9:23),Newline(1:1-1:1),
LowerIdent(10:5-10:11),OpAssign(10:12-10:13),UpperIdent(10:14-10:17),NoSpaceDotLowerIdent(10:17-10:22),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(12:6-12:32),
LowerIdent(13:5-13:12),OpAssign(13:13-13:14),UpperIdent(13:15-13:19),NoSpaceDotLowerIdent(13:19-13:25),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(15:6-15:33),
LowerIdent(16:5-16:12),OpAssign(16:13-16:14),UpperIdent(16:15-16:19),NoSpaceDotLowerIdent(16:19-16:24),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(18:6-18:61),
LowerIdent(19:5-19:12),OpAssign(19:13-19:14),LowerIdent(19:15-19:18),Newline(1:1-1:1),
LowerIdent(20:5-20:12),OpAssign(20:13-20:14),LowerIdent(20:15-20:19),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(22:6-22:37),
LowerIdent(23:5-23:13),OpAssign(23:14-23:15),UpperIdent(23:16-23:19),NoSpaceDotLowerIdent(23:19-23:26),Newline(1:1-1:1),
Newline(1:1-1:1),
OpenRound(25:5-25:6),Newline(1:1-1:1),
LowerIdent(26:9-26:15),Comma(26:15-26:16),Newline(1:1-1:1),
LowerIdent(27:9-27:15),Comma(27:15-27:16),Newline(1:1-1:1),
LowerIdent(28:9-28:15),Comma(28:15-28:16),Newline(1:1-1:1),
LowerIdent(29:9-29:16),Comma(29:16-29:17),Newline(1:1-1:1),
LowerIdent(30:9-30:16),Comma(30:16-30:17),Newline(1:1-1:1),
LowerIdent(31:9-31:16),Comma(31:16-31:17),Newline(1:1-1:1),
LowerIdent(32:9-32:16),Comma(32:16-32:17),Newline(1:1-1:1),
LowerIdent(33:9-33:17),Comma(33:17-33:18),Newline(1:1-1:1),
CloseRound(34:5-34:6),Newline(1:1-1:1),
CloseCurly(35:1-35:2),EndOfFile(35:2-35:2),
~~~
# PARSE
~~~clojure
(file @1.1-35.2
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @3.1-3.17 (raw "json.Json"))
		(s-import @4.1-4.48 (raw "http.Client") (alias "Http")
			(exposing
				(exposed-lower-ident (text "get"))
				(exposed-lower-ident (text "post"))))
		(s-import @5.1-5.27 (raw "utils.String") (alias "Str"))
		(s-decl @7.1-35.2
			(p-ident @7.1-7.5 (raw "main"))
			(e-block @7.8-35.2
				(statements
					(s-decl @8.5-8.22
						(p-ident @8.5-8.11 (raw "client"))
						(e-ident @8.14-8.22 (raw "Http.get")))
					(s-decl @9.5-9.23
						(p-ident @9.5-9.11 (raw "parser"))
						(e-ident @9.14-9.23 (raw "Json.utf8")))
					(s-decl @10.5-10.22
						(p-ident @10.5-10.11 (raw "helper"))
						(e-ident @10.14-10.22 (raw "Str.trim")))
					(s-decl @13.5-13.25
						(p-ident @13.5-13.12 (raw "result1"))
						(e-ident @13.15-13.25 (raw "Json.parse")))
					(s-decl @16.5-16.24
						(p-ident @16.5-16.12 (raw "result2"))
						(e-ident @16.15-16.24 (raw "Http.post")))
					(s-decl @19.5-19.18
						(p-ident @19.5-19.12 (raw "result3"))
						(e-ident @19.15-19.18 (raw "get")))
					(s-decl @20.5-20.19
						(p-ident @20.5-20.12 (raw "result4"))
						(e-ident @20.15-20.19 (raw "post")))
					(s-decl @23.5-23.26
						(p-ident @23.5-23.13 (raw "combined"))
						(e-ident @23.16-23.26 (raw "Str.concat")))
					(e-tuple @25.5-34.6
						(e-ident @26.9-26.15 (raw "client"))
						(e-ident @27.9-27.15 (raw "parser"))
						(e-ident @28.9-28.15 (raw "helper"))
						(e-ident @29.9-29.16 (raw "result1"))
						(e-ident @30.9-30.16 (raw "result2"))
						(e-ident @31.9-31.16 (raw "result3"))
						(e-ident @32.9-32.16 (raw "result4"))
						(e-ident @33.9-33.17 (raw "combined"))))))))
~~~
# FORMATTED
~~~roc
module []

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
		(p-assign @7.1-7.5 (ident "main"))
		(e-block @7.8-35.2
			(s-let @8.5-8.22
				(p-assign @8.5-8.11 (ident "client"))
				(e-lookup-external @8.14-8.22 (module-idx 1) (field "get") (target-node-idx 0)))
			(s-let @9.5-9.23
				(p-assign @9.5-9.11 (ident "parser"))
				(e-lookup-external @9.14-9.23 (module-idx 0) (field "utf8") (target-node-idx 0)))
			(s-let @10.5-10.22
				(p-assign @10.5-10.11 (ident "helper"))
				(e-lookup-external @10.14-10.22 (module-idx 2) (field "trim") (target-node-idx 0)))
			(s-let @13.5-13.25
				(p-assign @13.5-13.12 (ident "result1"))
				(e-lookup-external @13.15-13.25 (module-idx 0) (field "parse") (target-node-idx 0)))
			(s-let @16.5-16.24
				(p-assign @16.5-16.12 (ident "result2"))
				(e-lookup-external @16.15-16.24 (module-idx 1) (field "post") (target-node-idx 0)))
			(s-let @19.5-19.18
				(p-assign @19.5-19.12 (ident "result3"))
				(e-lookup-external @19.15-19.18 (module-idx 1) (field "get") (target-node-idx 0)))
			(s-let @20.5-20.19
				(p-assign @20.5-20.12 (ident "result4"))
				(e-lookup-external @20.15-20.19 (module-idx 1) (field "post") (target-node-idx 0)))
			(s-let @23.5-23.26
				(p-assign @23.5-23.13 (ident "combined"))
				(e-lookup-external @23.16-23.26 (module-idx 2) (field "concat") (target-node-idx 0)))
			(e-tuple @25.5-34.6
				(elems
					(e-lookup-local @26.9-26.15
						(pattern @8.5-8.11))
					(e-lookup-local @27.9-27.15
						(pattern @9.5-9.11))
					(e-lookup-local @28.9-28.15
						(pattern @10.5-10.11))
					(e-lookup-local @29.9-29.16
						(pattern @13.5-13.12))
					(e-lookup-local @30.9-30.16
						(pattern @16.5-16.12))
					(e-lookup-local @31.9-31.16
						(pattern @19.5-19.12))
					(e-lookup-local @32.9-32.16
						(pattern @20.5-20.12))
					(e-lookup-local @33.9-33.17
						(pattern @23.5-23.13))))))
	(s-import @3.1-3.17 (module "json.Json") (qualifier "json")
		(exposes))
	(s-import @4.1-4.48 (module "http.Client") (qualifier "http") (alias "Http")
		(exposes
			(exposed (name "get") (wildcard false))
			(exposed (name "post") (wildcard false))))
	(s-import @5.1-5.27 (module "utils.String") (qualifier "utils") (alias "Str")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.5 (type "(*, *, *, *, *, *, *, *)")))
	(expressions
		(expr @7.8-35.2 (type "(*, *, *, *, *, *, *, *)"))))
~~~
