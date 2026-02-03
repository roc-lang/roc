# META
~~~ini
description=Test error for expression statement at module top level
type=file
~~~
# SOURCE
~~~roc
module [x]

42

x = 1
~~~
# EXPECTED
PARSE ERROR - can_error_invalid_top_level_expr.md:3:1:3:3
MODULE HEADER DEPRECATED - can_error_invalid_top_level_expr.md:1:1:1:11
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**can_error_invalid_top_level_expr.md:3:1:3:3:**
```roc
42
```
^^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_error_invalid_top_level_expr.md:1:1:1:11:**
```roc
module [x]
```
^^^^^^^^^^


# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
Int,
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
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "1")))))
~~~
# FORMATTED
~~~roc
module [x]



x = 1
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
