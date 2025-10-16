# META
~~~ini
description=Variable scoping with var keyword
type=snippet
~~~
# SOURCE
~~~roc
# This should cause an error - var not allowed at top level
var topLevelVar_ = 0
~~~
# EXPECTED
PARSE ERROR - can_var_scoping_invalid_top_level.md:2:1:2:4
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `var_only_allowed_in_a_body`
This is an unexpected parsing error. Please check your syntax.

**can_var_scoping_invalid_top_level.md:2:1:2:4:**
```roc
var topLevelVar_ = 0
```
^^^


# TOKENS
~~~zig
KwVar,LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "var_only_allowed_in_a_body"))
		(s-decl
			(p-ident (raw "topLevelVar_"))
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
# This should cause an error - var not allowed at top level
topLevelVar_ = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "topLevelVar_"))
		(e-num (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)")))
	(expressions
		(expr (type "Num(_size)"))))
~~~
