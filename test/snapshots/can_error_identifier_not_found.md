# META
~~~ini
description=Test error for undefined identifier
type=file
~~~
# SOURCE
~~~roc
module [main]

main = undefined_variable
~~~
# EXPECTED
MODULE HEADER DEPRECATED - can_error_identifier_not_found.md:1:1:1:14
UNDEFINED VARIABLE - can_error_identifier_not_found.md:3:8:3:26
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_error_identifier_not_found.md:1:1:1:14:**
```roc
module [main]
```
^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `undefined_variable` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_error_identifier_not_found.md:3:8:3:26:**
```roc
main = undefined_variable
```
       ^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-lower-ident
				(text "main"))))
	(statements
		(s-decl
			(p-ident (raw "main"))
			(e-ident (raw "undefined_variable")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
