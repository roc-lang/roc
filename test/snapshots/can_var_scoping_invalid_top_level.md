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
# EXPECTED
PARSE ERROR - can_var_scoping_invalid_top_level.md:4:1:4:4
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `var_only_allowed_in_a_body`
This is an unexpected parsing error. Please check your syntax.

**can_var_scoping_invalid_top_level.md:4:1:4:4:**
```roc
var topLevelVar_ = 0
```
^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwVar(4:1-4:4),LowerIdent(4:5-4:17),OpAssign(4:18-4:19),Int(4:20-4:21),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.21
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-malformed @4.1-4.4 (tag "var_only_allowed_in_a_body"))
		(s-decl @4.5-4.21
			(p-ident @4.5-4.17 (raw "topLevelVar_"))
			(e-int @4.20-4.21 (raw "0")))))
~~~
# FORMATTED
~~~roc
module []

# This should cause an error - var not allowed at top level
topLevelVar_ = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.5-4.17 (ident "topLevelVar_"))
		(e-int @4.20-4.21 (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.5-4.17 (type "Num(_size)")))
	(expressions
		(expr @4.20-4.21 (type "Num(_size)"))))
~~~
