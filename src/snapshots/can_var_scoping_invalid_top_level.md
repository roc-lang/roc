# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# This should cause an error - var not allowed at top level
var topLevelVar_ = 0
~~~
# PROBLEMS
~~~txt
PARSER: var_only_allowed_in_a_body
~~~
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:60),
KwVar(4:1-4:4),LowerIdent(4:5-4:17),OpAssign(4:18-4:19),Int(4:20-4:21),EndOfFile(4:21-4:21),
~~~
# PARSE
~~~clojure
(file (1:1-4:21)
	(module (1:1-1:10) (exposes (1:8-1:10)))
	(statements
		(malformed_stmt (4:1-4:4) "var_only_allowed_in_a_body")
		(decl (4:5-4:21)
			(ident (4:5-4:17) "topLevelVar_")
			(int (4:20-4:21) "0"))))
~~~
# FORMATTED
~~~roc
module []

# This should cause an error - var not allowed at top level
topLevelVar_ = 0
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (4:5-4:17)
				(pid 12)
				(ident "topLevelVar_")))
		(def_expr
			(e_int (4:20-4:21)
				(int_var 14)
				(precision_var 13)
				(literal "0")
				(value "TODO")
				(bound "u8")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "topLevelVar_" 16 (type "Num(Int(*))")))
	(expressions
		(expr (4:20-4:21) 15 (type "Num(Int(*))"))))
~~~