# META
~~~ini
description=Type redeclaration in same scope should produce error
type=file
~~~
# SOURCE
~~~roc
module [Maybe]

Maybe(a) : [Some(a), None]
Maybe(a) : [Ok(a), Err]
~~~
# EXPECTED
MODULE HEADER DEPRECATED - type_redeclaration_same_scope.md:1:1:1:15
TYPE REDECLARED - type_redeclaration_same_scope.md:4:1:4:24
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**type_redeclaration_same_scope.md:1:1:1:15:**
```roc
module [Maybe]
```
^^^^^^^^^^^^^^


**TYPE REDECLARED**
The type _Maybe_ is being redeclared.

The redeclaration is here:
**type_redeclaration_same_scope.md:4:1:4:24:**
```roc
Maybe(a) : [Ok(a), Err]
```
^^^^^^^^^^^^^^^^^^^^^^^

But _Maybe_ was already declared here:
**type_redeclaration_same_scope.md:3:1:3:27:**
```roc
Maybe(a) : [Some(a), None]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:14),CloseSquare(1:14-1:15),
UpperIdent(3:1-3:6),NoSpaceOpenRound(3:6-3:7),LowerIdent(3:7-3:8),CloseRound(3:8-3:9),OpColon(3:10-3:11),OpenSquare(3:12-3:13),UpperIdent(3:13-3:17),NoSpaceOpenRound(3:17-3:18),LowerIdent(3:18-3:19),CloseRound(3:19-3:20),Comma(3:20-3:21),UpperIdent(3:22-3:26),CloseSquare(3:26-3:27),
UpperIdent(4:1-4:6),NoSpaceOpenRound(4:6-4:7),LowerIdent(4:7-4:8),CloseRound(4:8-4:9),OpColon(4:10-4:11),OpenSquare(4:12-4:13),UpperIdent(4:13-4:15),NoSpaceOpenRound(4:15-4:16),LowerIdent(4:16-4:17),CloseRound(4:17-4:18),Comma(4:18-4:19),UpperIdent(4:20-4:23),CloseSquare(4:23-4:24),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.24
	(module @1.1-1.15
		(exposes @1.8-1.15
			(exposed-upper-ident @1.9-1.14 (text "Maybe"))))
	(statements
		(s-type-decl @3.1-3.27
			(header @3.1-3.9 (name "Maybe")
				(args
					(ty-var @3.7-3.8 (raw "a"))))
			(ty-tag-union @3.12-3.27
				(tags
					(ty-apply @3.13-3.20
						(ty @3.13-3.17 (name "Some"))
						(ty-var @3.18-3.19 (raw "a")))
					(ty @3.22-3.26 (name "None")))))
		(s-type-decl @4.1-4.24
			(header @4.1-4.9 (name "Maybe")
				(args
					(ty-var @4.7-4.8 (raw "a"))))
			(ty-tag-union @4.12-4.24
				(tags
					(ty-apply @4.13-4.18
						(ty @4.13-4.15 (name "Ok"))
						(ty-var @4.16-4.17 (raw "a")))
					(ty @4.20-4.23 (name "Err")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-3.27
		(ty-header @3.1-3.9 (name "Maybe")
			(ty-args
				(ty-rigid-var @3.7-3.8 (name "a"))))
		(ty-tag-union @3.12-3.27
			(ty-tag-name @3.13-3.20 (name "Some")
				(ty-rigid-var-lookup (ty-rigid-var @3.7-3.8 (name "a"))))
			(ty-tag-name @3.22-3.26 (name "None"))))
	(s-alias-decl @4.1-4.24
		(ty-header @4.1-4.9 (name "Maybe")
			(ty-args
				(ty-rigid-var @4.7-4.8 (name "a"))))
		(ty-tag-union @4.12-4.24
			(ty-tag-name @4.13-4.18 (name "Ok")
				(ty-rigid-var-lookup (ty-rigid-var @4.7-4.8 (name "a"))))
			(ty-tag-name @4.20-4.23 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @3.1-3.27 (type "Maybe(a)")
			(ty-header @3.1-3.9 (name "Maybe")
				(ty-args
					(ty-rigid-var @3.7-3.8 (name "a")))))
		(alias @4.1-4.24 (type "Maybe(a)")
			(ty-header @4.1-4.9 (name "Maybe")
				(ty-args
					(ty-rigid-var @4.7-4.8 (name "a"))))))
	(expressions))
~~~
