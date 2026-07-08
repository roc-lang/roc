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
VAR OUTSIDE BODY - can_var_scoping_invalid_top_level.md:2:1:2:4
# PROBLEMS

┌──────────────────┐
│ VAR OUTSIDE BODY ├─ I was parsing a statement, and `var` appeared outside ──┐
└┬─────────────────┘  a function or block body.                               │
 │                                                                            │
 │  var topLevelVar_ = 0                                                      │
 │  ‾‾‾                                                                       │
 └────────────────────────────────── can_var_scoping_invalid_top_level.md:2:1 ┘

    Mutable variables are local body statements. Move this `var` into a body,
    or use an ordinary top-level declaration.

    For example:
        main = {
            var count = 0
            count
        }

    I found `var` here.
    That word is reserved by Roc, so it cannot be used as a name in this
    position.

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
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec"))))
~~~
