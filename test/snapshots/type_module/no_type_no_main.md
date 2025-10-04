# META
~~~ini
description=Headerless module with no type and no main! - treated as default-app, reports missing main!
type=file
~~~
# SOURCE
~~~roc
x = 5
~~~
# EXPECTED
TYPE MODULE MISSING MATCHING TYPE - no_type_no_main.md:1:1:1:6
# PROBLEMS
**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `no_type_no_main`.roc, but no top-level type declaration named `no_type_no_main` was found.

Add either:
`no_type_no_main := ...` (nominal type)
or:
`no_type_no_main : ...` (type alias)
**no_type_no_main.md:1:1:1:6:**
```roc
x = 5
```
^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),Int(1:5-1:6),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.6
	(type-module @1.1-1.2)
	(statements
		(s-decl @1.1-1.6
			(p-ident @1.1-1.2 (raw "x"))
			(e-int @1.5-1.6 (raw "5")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.2 (ident "x"))
		(e-num @1.5-1.6 (value "5"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.2 (type "Num(_size)")))
	(expressions
		(expr @1.5-1.6 (type "Num(_size)"))))
~~~
