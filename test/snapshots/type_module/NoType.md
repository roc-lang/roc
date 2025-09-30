# META
~~~ini
description=Invalid type module with no type declaration
type=file
~~~
# SOURCE
~~~roc
x = 5
~~~
# EXPECTED
TYPE MODULE MISSING MATCHING TYPE - NoType.md:1:1:1:6
# PROBLEMS
**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This module is named `NoType`, but no top-level type declaration named `NoType` was found.

Add either:
`NoType := ...` (nominal type)
or:
`NoType : ...` (type alias)
**NoType.md:1:1:1:6:**
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
		(e-int @1.5-1.6 (value "5"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.2 (type "Num(_size)")))
	(expressions
		(expr @1.5-1.6 (type "Num(_size)"))))
~~~
