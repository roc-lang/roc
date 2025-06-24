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
(file (1:1-35:2)
	(module (1:1-1:10) (exposes (1:8-1:10)))
	(statements
		(import (3:1-3:17) ".Json" (qualifier "json"))
		(import (4:1-4:48)
			".Client"
			(qualifier "http")
			(alias "Http")
			(exposing
				(exposed_item (lower_ident "get"))
				(exposed_item (lower_ident "post"))))
		(import (5:1-5:27)
			".String"
			(qualifier "utils")
			(alias "Str"))
		(decl (7:1-35:2)
			(ident (7:1-7:5) "main")
			(block (7:8-35:2)
				(statements
					(decl (8:5-8:22)
						(ident (8:5-8:11) "client")
						(ident (8:14-8:22) "Http" ".get"))
					(decl (9:5-9:23)
						(ident (9:5-9:11) "parser")
						(ident (9:14-9:23) "Json" ".utf8"))
					(decl (10:5-10:22)
						(ident (10:5-10:11) "helper")
						(ident (10:14-10:22) "Str" ".trim"))
					(decl (13:5-13:25)
						(ident (13:5-13:12) "result1")
						(ident (13:15-13:25) "Json" ".parse"))
					(decl (16:5-16:24)
						(ident (16:5-16:12) "result2")
						(ident (16:15-16:24) "Http" ".post"))
					(decl (19:5-19:18)
						(ident (19:5-19:12) "result3")
						(ident (19:15-19:18) "" "get"))
					(decl (20:5-20:19)
						(ident (20:5-20:12) "result4")
						(ident (20:15-20:19) "" "post"))
					(decl (23:5-23:26)
						(ident (23:5-23:13) "combined")
						(ident (23:16-23:26) "Str" ".concat"))
					(tuple (25:5-34:6)
						(ident (26:9-26:15) "" "client")
						(ident (27:9-27:15) "" "parser")
						(ident (28:9-28:15) "" "helper")
						(ident (29:9-29:16) "" "result1")
						(ident (30:9-30:16) "" "result2")
						(ident (31:9-31:16) "" "result3")
						(ident (32:9-32:16) "" "result4")
						(ident (33:9-33:17) "" "combined")))))))
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
(can_ir
	(d_let
		(def_pattern
			(p_assign (7:1-7:5)
				(pid 77)
				(ident "main")))
		(def_expr
			(e_block (7:8-35:2)
				(s_let (8:5-8:22)
					(p_assign (8:5-8:11)
						(pid 78)
						(ident "client"))
					(e_lookup_external
						(external_decl (8:14-8:22)
							(qualified_name "http.Client.get")
							(module_name "http.Client")
							(local_name "get")
							(kind "value")
							(type_var 79))))
				(s_let (9:5-9:23)
					(p_assign (9:5-9:11)
						(pid 82)
						(ident "parser"))
					(e_lookup_external
						(external_decl (9:14-9:23)
							(qualified_name "json.Json.utf8")
							(module_name "json.Json")
							(local_name "utf8")
							(kind "value")
							(type_var 83))))
				(s_let (10:5-10:22)
					(p_assign (10:5-10:11)
						(pid 86)
						(ident "helper"))
					(e_lookup_external
						(external_decl (10:14-10:22)
							(qualified_name "utils.String.trim")
							(module_name "utils.String")
							(local_name "trim")
							(kind "value")
							(type_var 87))))
				(s_let (13:5-13:25)
					(p_assign (13:5-13:12)
						(pid 90)
						(ident "result1"))
					(e_lookup_external
						(external_decl (13:15-13:25)
							(qualified_name "json.Json.parse")
							(module_name "json.Json")
							(local_name "parse")
							(kind "value")
							(type_var 91))))
				(s_let (16:5-16:24)
					(p_assign (16:5-16:12)
						(pid 94)
						(ident "result2"))
					(e_lookup_external
						(external_decl (16:15-16:24)
							(qualified_name "http.Client.post")
							(module_name "http.Client")
							(local_name "post")
							(kind "value")
							(type_var 95))))
				(s_let (19:5-19:18)
					(p_assign (19:5-19:12)
						(pid 98)
						(ident "result3"))
					(e_lookup_external
						(external_decl (19:15-19:18)
							(qualified_name "http.Client.get")
							(module_name "http.Client")
							(local_name "get")
							(kind "value")
							(type_var 99))))
				(s_let (20:5-20:19)
					(p_assign (20:5-20:12)
						(pid 102)
						(ident "result4"))
					(e_lookup_external
						(external_decl (20:15-20:19)
							(qualified_name "http.Client.post")
							(module_name "http.Client")
							(local_name "post")
							(kind "value")
							(type_var 103))))
				(s_let (23:5-23:26)
					(p_assign (23:5-23:13)
						(pid 106)
						(ident "combined"))
					(e_lookup_external
						(external_decl (23:16-23:26)
							(qualified_name "utils.String.concat")
							(module_name "utils.String")
							(local_name "concat")
							(kind "value")
							(type_var 107))))
				(e_tuple (25:5-34:6)
					(tuple_var "#118")
					(elems
						(e_lookup_local (26:9-26:15) (pid 78))
						(e_lookup_local (27:9-27:15) (pid 82))
						(e_lookup_local (28:9-28:15) (pid 86))
						(e_lookup_local (29:9-29:16) (pid 90))
						(e_lookup_local (30:9-30:16) (pid 94))
						(e_lookup_local (31:9-31:16) (pid 98))
						(e_lookup_local (32:9-32:16) (pid 102))
						(e_lookup_local (33:9-33:17) (pid 106)))))))
	(s_import (3:1-3:17)
		"json.Json"
		""
		""
		(exposes))
	(s_import (4:1-4:48)
		"http.Client"
		""
		""
		(exposes (exposed_item "get") (exposed_item "post")))
	(s_import (5:1-5:27)
		"utils.String"
		""
		""
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "main" 121 (type "*")))
	(expressions
		(expr (7:8-35:2) 120 (type "*"))))
~~~