# META
~~~ini
description=Import with explicit alias
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json as MyJson

main = MyJson.decode
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),KwAs(3:18-3:20),UpperIdent(3:21-3:27),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),UpperIdent(5:8-5:14),NoSpaceDotLowerIdent(5:14-5:21),EndOfFile(5:21-5:21),
~~~
# PARSE
~~~clojure
(file (1:1-5:21)
	(module (1:1-1:10) (exposes (1:8-1:10)))
	(statements
		(import (3:1-3:27)
			".Json"
			(qualifier "json")
			(alias "MyJson"))
		(decl (5:1-5:21)
			(ident (5:1-5:5) "main")
			(ident (5:8-5:21) "MyJson" ".decode"))))
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
				(pid 73)
				(ident "main")))
		(def_expr
			(e_lookup_external
				(external_decl (5:8-5:21)
					(qualified_name "Json.decode")
					(module_name "Json")
					(local_name "decode")
					(kind "value")
					(type_var 74)))))
	(s_import (3:1-3:27)
		"Json"
		""
		""
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "main" 76 (type "*")))
	(expressions
		(expr (5:8-5:21) 75 (type "*"))))
~~~