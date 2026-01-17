# META
~~~ini
description=Test error when associated value is not found on type
type=file
~~~
# SOURCE
~~~roc
module [MyType, use_nonexistent]

MyType := U64

use_nonexistent = MyType.doesnt_exist
~~~
# EXPECTED
MODULE HEADER DEPRECATED - can_error_nested_value_not_found.md:1:1:1:33
DOES NOT EXIST - can_error_nested_value_not_found.md:5:19:5:38
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_error_nested_value_not_found.md:1:1:1:33:**
```roc
module [MyType, use_nonexistent]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**DOES NOT EXIST**
`MyType.doesnt_exist` does not exist.

`MyType` is in scope, but it has no associated `doesnt_exist`.

It's referenced here:
**can_error_nested_value_not_found.md:5:19:5:38:**
```roc
use_nonexistent = MyType.doesnt_exist
```
                  ^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule,OpenSquare,UpperIdent,Comma,LowerIdent,CloseSquare,
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-upper-ident (text "MyType"))
			(exposed-lower-ident
				(text "use_nonexistent"))))
	(statements
		(s-type-decl
			(header (name "MyType")
				(args))
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "use_nonexistent"))
			(e-ident (raw "MyType.doesnt_exist")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "use_nonexistent"))
		(e-runtime-error (tag "nested_value_not_found")))
	(s-nominal-decl
		(ty-header (name "MyType"))
		(ty-lookup (name "U64") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(type_decls
		(nominal (type "MyType")
			(ty-header (name "MyType"))))
	(expressions
		(expr (type "Error"))))
~~~
