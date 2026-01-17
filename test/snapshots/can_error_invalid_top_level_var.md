# META
~~~ini
description=Test error for var statement at module top level
type=file
~~~
# SOURCE
~~~roc
module [x]

var x_ = 0
~~~
# EXPECTED
PARSE ERROR - can_error_invalid_top_level_var.md:3:1:3:4
MODULE HEADER DEPRECATED - can_error_invalid_top_level_var.md:1:1:1:11
EXPOSED BUT NOT DEFINED - can_error_invalid_top_level_var.md:1:9:1:10
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `var_only_allowed_in_a_body`
This is an unexpected parsing error. Please check your syntax.

**can_error_invalid_top_level_var.md:3:1:3:4:**
```roc
var x_ = 0
```
^^^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_error_invalid_top_level_var.md:1:1:1:11:**
```roc
module [x]
```
^^^^^^^^^^


**EXPOSED BUT NOT DEFINED**
The module header says that `x` is exposed, but it is not defined anywhere in this module.

**can_error_invalid_top_level_var.md:1:9:1:10:**
```roc
module [x]
```
        ^
You can fix this by either defining `x` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
KwVar,LowerIdent,OpAssign,Int,
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
		(s-malformed (tag "var_only_allowed_in_a_body"))
		(s-decl
			(p-ident (raw "x_"))
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
module [x]

x_ = 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x_"))
		(e-num (value "0"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
