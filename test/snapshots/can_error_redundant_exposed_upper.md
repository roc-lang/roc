# META
~~~ini
description=Test error for duplicate type name in exposed list
type=file
~~~
# SOURCE
~~~roc
module [Foo, Foo]

Foo : U64
~~~
# EXPECTED
MODULE HEADER DEPRECATED - can_error_redundant_exposed_upper.md:1:1:1:18
REDUNDANT EXPOSED - can_error_redundant_exposed_upper.md:1:14:1:17
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_error_redundant_exposed_upper.md:1:1:1:18:**
```roc
module [Foo, Foo]
```
^^^^^^^^^^^^^^^^^


**REDUNDANT EXPOSED**
The identifier `Foo` is exposed multiple times in the module header.

**can_error_redundant_exposed_upper.md:1:14:1:17:**
```roc
module [Foo, Foo]
```
             ^^^
You can remove the duplicate entry to fix this warning.

# TOKENS
~~~zig
KwModule,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
UpperIdent,OpColon,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-upper-ident (text "Foo"))
			(exposed-upper-ident (text "Foo"))))
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty (name "U64")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Foo"))
		(ty-lookup (name "U64") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Foo")
			(ty-header (name "Foo"))))
	(expressions))
~~~
