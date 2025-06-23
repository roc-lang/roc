# META
~~~ini
description=Import with exposing clause using aliases
type=file
~~~
# SOURCE
~~~roc
module [main]

import json.Json exposing [decode as fromJson, encode as toJson]

main = {
	data = { name: "Bob", age: 25 }
	encoded = toJson(data)
	decoded = fromJson(encoded)
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
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),KwExposing(3:18-3:26),OpenSquare(3:27-3:28),LowerIdent(3:28-3:34),KwAs(3:35-3:37),LowerIdent(3:38-3:46),Comma(3:46-3:47),LowerIdent(3:48-3:54),KwAs(3:55-3:57),LowerIdent(3:58-3:64),CloseSquare(3:64-3:65),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),OpenCurly(5:8-5:9),Newline(1:1-1:1),
LowerIdent(6:2-6:6),OpAssign(6:7-6:8),OpenCurly(6:9-6:10),LowerIdent(6:11-6:15),OpColon(6:15-6:16),StringStart(6:17-6:18),StringPart(6:18-6:21),StringEnd(6:21-6:22),Comma(6:22-6:23),LowerIdent(6:24-6:27),OpColon(6:27-6:28),Int(6:29-6:31),CloseCurly(6:32-6:33),Newline(1:1-1:1),
LowerIdent(7:2-7:9),OpAssign(7:10-7:11),LowerIdent(7:12-7:18),NoSpaceOpenRound(7:18-7:19),LowerIdent(7:19-7:23),CloseRound(7:23-7:24),Newline(1:1-1:1),
LowerIdent(8:2-8:9),OpAssign(8:10-8:11),LowerIdent(8:12-8:20),NoSpaceOpenRound(8:20-8:21),LowerIdent(8:21-8:28),CloseRound(8:28-8:29),Newline(1:1-1:1),
LowerIdent(9:2-9:9),Newline(1:1-1:1),
CloseCurly(10:1-10:2),EndOfFile(10:2-10:2),
~~~
# PARSE
~~~clojure
(file (1:1-10:2)
	(module (1:1-1:14)
		(exposes (1:8-1:14) (exposed_item (lower_ident "main"))))
	(statements
		(import (3:1-3:65)
			".Json"
			(qualifier "json")
			(exposing
				(exposed_item (lower_ident "decode" "fromJson"))
				(exposed_item (lower_ident "encode" "toJson"))))
		(decl (5:1-10:2)
			(ident (5:1-5:5) "main")
			(block (5:8-10:2)
				(statements
					(decl (6:2-6:33)
						(ident (6:2-6:6) "data")
						(record (6:9-6:33)
							(field
								"name"
								(string (6:17-6:22) (string_part (6:18-6:21) "Bob")))
							(field "age" (int (6:29-6:31) "25"))))
					(decl (7:2-7:24)
						(ident (7:2-7:9) "encoded")
						(apply (7:12-7:24)
							(ident (7:12-7:18) "" "toJson")
							(ident (7:19-7:23) "" "data")))
					(decl (8:2-8:29)
						(ident (8:2-8:9) "decoded")
						(apply (8:12-8:29)
							(ident (8:12-8:20) "" "fromJson")
							(ident (8:21-8:28) "" "encoded")))
					(ident (9:2-9:9) "" "decoded"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
				(s_let (6:2-6:33)
					(p_assign (6:2-6:6)
						(pid 76)
						(ident "data"))
					(e_runtime_error (1:1-1:1) "not_implemented"))
				(s_let (7:2-7:24)
					(p_assign (7:2-7:9)
						(pid 80)
						(ident "encoded"))
					(e_call (7:12-7:24)
						(e_lookup_external
							(external_decl (7:12-7:18)
								(qualified_name "json.Json.encode")
								(module_name "json.Json")
								(local_name "toJson")
								(kind "value")
								(type_var 81)))
						(e_lookup_local (7:19-7:23) (pid 76))))
				(s_let (8:2-8:29)
					(p_assign (8:2-8:9)
						(pid 86)
						(ident "decoded"))
					(e_call (8:12-8:29)
						(e_lookup_external
							(external_decl (8:12-8:20)
								(qualified_name "json.Json.decode")
								(module_name "json.Json")
								(local_name "fromJson")
								(kind "value")
								(type_var 87)))
						(e_lookup_local (8:21-8:28) (pid 80))))
				(e_lookup_local (9:2-9:9) (pid 86)))))
	(s_import (3:1-3:65)
		"json.Json"
		""
		""
		(exposes
			(exposed_item "decode" "fromJson")
			(exposed_item "encode" "toJson"))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "main" 94 (type "*")))
	(expressions
		(expr (5:8-10:2) 93 (type "*"))))
~~~