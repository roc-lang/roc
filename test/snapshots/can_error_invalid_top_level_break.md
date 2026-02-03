# META
~~~ini
description=Test error for break statement at module top level
type=file
~~~
# SOURCE
~~~roc
module [x]

break

x = 1
~~~
# EXPECTED
MODULE HEADER DEPRECATED - can_error_invalid_top_level_break.md:1:1:1:11
INVALID STATEMENT - can_error_invalid_top_level_break.md:3:1:3:6
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_error_invalid_top_level_break.md:1:1:1:11:**
```roc
module [x]
```
^^^^^^^^^^


**INVALID STATEMENT**
The statement `break` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**can_error_invalid_top_level_break.md:3:1:3:6:**
```roc
break
```
^^^^^


# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
KwBreak,
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-lower-ident
				(text "x"))))
	(statements
		(s-break)
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "1")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "1"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
