# META
~~~ini
description=Import with module-qualified usage
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json

main = Json.utf8
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),UpperIdent(5:8-5:12),NoSpaceDotLowerIdent(5:12-5:17),EndOfFile(5:17-5:17),
~~~
# PARSE
~~~clojure
(file (1:1-5:17)
	(module (1:1-1:10) (exposes (1:8-1:10)))
	(statements
		(import (3:1-3:17) ".Json" (qualifier "json"))
		(decl (5:1-5:17)
			(ident (5:1-5:5) "main")
			(ident (5:8-5:17) "Json" ".utf8"))))
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
				(external_decl (5:8-5:17)
					(qualified_name "Json.utf8")
					(module_name "Json")
					(local_name "utf8")
					(kind "value")
					(type_var 74)))))
	(s_import (3:1-3:17)
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
		(expr (5:8-5:17) 75 (type "*"))))
~~~