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
KwVar(2:1-2:4),LowerIdent(2:5-2:17),OpAssign(2:18-2:19),Int(2:20-2:21),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @2.1-2.21
	(type-module @2.1-2.4)
	(statements
		(s-malformed @2.1-2.4 (tag "var_only_allowed_in_a_body"))
		(s-decl @2.5-2.21
			(p-ident @2.5-2.17 (raw "topLevelVar_"))
			(e-int @2.20-2.21 (raw "0")))))
~~~
# FORMATTED
~~~roc
# This should cause an error - var not allowed at top level
# This should cause an error - var not allowed at top level
topLevelVar_ = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.5-2.17 (ident "topLevelVar_"))
		(e-num @2.20-2.21 (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.5-2.17 (type "Num(_size)")))
	(expressions
		(expr @2.20-2.21 (type "Num(_size)"))))
~~~
