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
**PARSE ERROR**
A parsing error occurred: `var_only_allowed_in_a_body`
This is an unexpected parsing error. Please check your syntax.
Here is the problematic code:
**can_var_scoping_invalid_top_level.md:4:1:4:17:**
```roc
var topLevelVar_ = 0
```


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
		(malformed_stmt (4:1-4:17) "var_only_allowed_in_a_body")
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
				(pid 72)
				(ident "topLevelVar_")))
		(def_expr
			(e_int (4:20-4:21)
				(int_var 74)
				(precision_var 73)
				(literal "0")
				(value "TODO")
				(bound "u8")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "topLevelVar_" 76 (type "Num(Int(*))")))
	(expressions
		(expr (4:20-4:21) 75 (type "Num(Int(*))"))))
~~~