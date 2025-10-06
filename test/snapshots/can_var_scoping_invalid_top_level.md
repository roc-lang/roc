# META
~~~ini
description=Variable scoping with var keyword
type=file:CanVarScopingInvalidTopLevel.roc
~~~
# SOURCE
~~~roc
CanVarScopingInvalidTopLevel := {}

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
UpperIdent(1:1-1:29),OpColonEqual(1:30-1:32),OpenCurly(1:33-1:34),CloseCurly(1:34-1:35),
KwVar(4:1-4:4),LowerIdent(4:5-4:17),OpAssign(4:18-4:19),Int(4:20-4:21),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.21
	(type-module @1.1-1.29)
	(statements
		(s-type-decl @1.1-1.35
			(header @1.1-1.29 (name "CanVarScopingInvalidTopLevel")
				(args))
			(ty-record @1.33-1.35))
		(s-malformed @4.1-4.4 (tag "var_only_allowed_in_a_body"))
		(s-decl @4.5-4.21
			(p-ident @4.5-4.17 (raw "topLevelVar_"))
			(e-int @4.20-4.21 (raw "0")))))
~~~
# FORMATTED
~~~roc
CanVarScopingInvalidTopLevel := {}

# This should cause an error - var not allowed at top level
topLevelVar_ = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.5-4.17 (ident "topLevelVar_"))
		(e-num @4.20-4.21 (value "0")))
	(s-nominal-decl @1.1-1.35
		(ty-header @1.1-1.29 (name "CanVarScopingInvalidTopLevel"))
		(ty-record @1.33-1.35)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.5-4.17 (type "Num(_size)")))
	(type_decls
		(nominal @1.1-1.35 (type "CanVarScopingInvalidTopLevel")
			(ty-header @1.1-1.29 (name "CanVarScopingInvalidTopLevel"))))
	(expressions
		(expr @4.20-4.21 (type "Num(_size)"))))
~~~
