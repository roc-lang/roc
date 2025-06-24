# META
~~~ini
description=Import with exposing syntax test
type=file
~~~
# SOURCE
~~~roc
module [main]

import pf.Stdout exposing [line!, write!]

main = 42
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),LowerIdent(3:8-3:10),NoSpaceDotUpperIdent(3:10-3:17),KwExposing(3:18-3:26),OpenSquare(3:27-3:28),LowerIdent(3:28-3:33),Comma(3:33-3:34),LowerIdent(3:35-3:41),CloseSquare(3:41-3:42),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),Int(5:8-5:10),EndOfFile(5:10-5:10),
~~~
# PARSE
~~~clojure
(file (1:1-5:10)
	(module (1:1-1:14)
		(exposes (1:8-1:14) (exposed_item (lower_ident "main"))))
	(statements
		(import (3:1-3:42)
			".Stdout"
			(qualifier "pf")
			(exposing
				(exposed_item (lower_ident "line!"))
				(exposed_item (lower_ident "write!"))))
		(decl (5:1-5:10)
			(ident (5:1-5:5) "main")
			(int (5:8-5:10) "42"))))
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
			(e_int (5:8-5:10)
				(int_var 77)
				(precision_var 76)
				(literal "42")
				(value "TODO")
				(bound "u8"))))
	(s_import (3:1-3:42)
		"pf.Stdout"
		""
		""
		(exposes (exposed_item "line!") (exposed_item "write!"))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "main" 79 (type "Num(Int(*))")))
	(expressions
		(expr (5:8-5:10) 78 (type "Num(Int(*))"))))
~~~