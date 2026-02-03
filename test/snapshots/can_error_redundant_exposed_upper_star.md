# META
~~~ini
description=Test error for duplicate type with constructors in exposed list
type=file
~~~
# SOURCE
~~~roc
module [Color.*, Color.*]

Color : [Red, Green, Blue]
~~~
# EXPECTED
MODULE HEADER DEPRECATED - can_error_redundant_exposed_upper_star.md:1:1:1:26
REDUNDANT EXPOSED - can_error_redundant_exposed_upper_star.md:1:18:1:25
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_error_redundant_exposed_upper_star.md:1:1:1:26:**
```roc
module [Color.*, Color.*]
```
^^^^^^^^^^^^^^^^^^^^^^^^^


**REDUNDANT EXPOSED**
The identifier `Color` is exposed multiple times in the module header.

**can_error_redundant_exposed_upper_star.md:1:18:1:25:**
```roc
module [Color.*, Color.*]
```
                 ^^^^^^^
You can remove the duplicate entry to fix this warning.

# TOKENS
~~~zig
KwModule,OpenSquare,UpperIdent,DotStar,Comma,UpperIdent,DotStar,CloseSquare,
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-upper-ident-star (text "Color"))
			(exposed-upper-ident-star (text "Color"))))
	(statements
		(s-type-decl
			(header (name "Color")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Red"))
					(ty (name "Green"))
					(ty (name "Blue")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Color"))
		(ty-tag-union
			(ty-tag-name (name "Red"))
			(ty-tag-name (name "Green"))
			(ty-tag-name (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Color")
			(ty-header (name "Color"))))
	(expressions))
~~~
