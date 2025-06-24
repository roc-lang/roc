# META
~~~ini
description=Import with exposing clause and usage of exposed items
type=file
~~~
# SOURCE
~~~roc
module [main]

import json.Json exposing [decode, encode]

main = {
    data = { name: "Alice", age: 30 }
    encoded = encode(data)
    decoded = decode(encoded)
    decoded
}
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),KwExposing(3:18-3:26),OpenSquare(3:27-3:28),LowerIdent(3:28-3:34),Comma(3:34-3:35),LowerIdent(3:36-3:42),CloseSquare(3:42-3:43),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),OpenCurly(5:8-5:9),Newline(1:1-1:1),
LowerIdent(6:5-6:9),OpAssign(6:10-6:11),OpenCurly(6:12-6:13),LowerIdent(6:14-6:18),OpColon(6:18-6:19),StringStart(6:20-6:21),StringPart(6:21-6:26),StringEnd(6:26-6:27),Comma(6:27-6:28),LowerIdent(6:29-6:32),OpColon(6:32-6:33),Int(6:34-6:36),CloseCurly(6:37-6:38),Newline(1:1-1:1),
LowerIdent(7:5-7:12),OpAssign(7:13-7:14),LowerIdent(7:15-7:21),NoSpaceOpenRound(7:21-7:22),LowerIdent(7:22-7:26),CloseRound(7:26-7:27),Newline(1:1-1:1),
LowerIdent(8:5-8:12),OpAssign(8:13-8:14),LowerIdent(8:15-8:21),NoSpaceOpenRound(8:21-8:22),LowerIdent(8:22-8:29),CloseRound(8:29-8:30),Newline(1:1-1:1),
LowerIdent(9:5-9:12),Newline(1:1-1:1),
CloseCurly(10:1-10:2),EndOfFile(10:2-10:2),
~~~
# PARSE
~~~clojure
(file (1:1-10:2)
	(module (1:1-1:14)
		(exposes (1:8-1:14) (exposed_item (lower_ident "main"))))
	(statements
		(import (3:1-3:43)
			".Json"
			(qualifier "json")
			(exposing
				(exposed_item (lower_ident "decode"))
				(exposed_item (lower_ident "encode"))))
		(decl (5:1-10:2)
			(ident (5:1-5:5) "main")
			(block (5:8-10:2)
				(statements
					(decl (6:5-6:38)
						(ident (6:5-6:9) "data")
						(record (6:12-6:38)
							(field
								"name"
								(string (6:20-6:27) (string_part (6:21-6:26) "Alice")))
							(field "age" (int (6:34-6:36) "30"))))
					(decl (7:5-7:27)
						(ident (7:5-7:12) "encoded")
						(apply (7:15-7:27)
							(ident (7:15-7:21) "" "encode")
							(ident (7:22-7:26) "" "data")))
					(decl (8:5-8:30)
						(ident (8:5-8:12) "decoded")
						(apply (8:15-8:30)
							(ident (8:15-8:21) "" "decode")
							(ident (8:22-8:29) "" "encoded")))
					(ident (9:5-9:12) "" "decoded"))))))
~~~
# FORMATTED
~~~roc
module [main]

import json.Json exposing [decode, encode]

main = {
	data = { name: "Alice", age: 30 }
	encoded = encode(data)
	decoded = decode(encoded)
	decoded
}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (5:1-5:5)
				(pid 75)
				(ident "main")))
		(def_expr
			(e_block (5:8-10:2)
				(s_let (6:5-6:38)
					(p_assign (6:5-6:9)
						(pid 76)
						(ident "data"))
					(e_runtime_error (1:1-1:1) "not_implemented"))
				(s_let (7:5-7:27)
					(p_assign (7:5-7:12)
						(pid 80)
						(ident "encoded"))
					(e_call (7:15-7:27)
						(e_lookup_external
							(external_decl (7:15-7:21)
								(qualified_name "json.Json.encode")
								(module_name "json.Json")
								(local_name "encode")
								(kind "value")
								(type_var 81)))
						(e_lookup_local (7:22-7:26) (pid 76))))
				(s_let (8:5-8:30)
					(p_assign (8:5-8:12)
						(pid 86)
						(ident "decoded"))
					(e_call (8:15-8:30)
						(e_lookup_external
							(external_decl (8:15-8:21)
								(qualified_name "json.Json.decode")
								(module_name "json.Json")
								(local_name "decode")
								(kind "value")
								(type_var 87)))
						(e_lookup_local (8:22-8:29) (pid 80))))
				(e_lookup_local (9:5-9:12) (pid 86)))))
	(s_import (3:1-3:43)
		"json.Json"
		""
		""
		(exposes (exposed_item "decode") (exposed_item "encode"))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "main" 94 (type "*")))
	(expressions
		(expr (5:8-10:2) 93 (type "*"))))
~~~