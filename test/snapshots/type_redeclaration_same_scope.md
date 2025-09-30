# META
~~~ini
description=Type redeclaration in same scope should produce error
type=file
~~~
# SOURCE
~~~roc
Maybe(a) : [Some(a), None]
Maybe(a) : [Ok(a), Err]
~~~
# EXPECTED
TYPE REDECLARED - type_redeclaration_same_scope.md:2:1:2:24
TYPE MODULE MISSING MATCHING TYPE - type_redeclaration_same_scope.md:1:1:2:24
# PROBLEMS
**TYPE REDECLARED**
The type _Maybe_ is being redeclared.

The redeclaration is here:
**type_redeclaration_same_scope.md:2:1:2:24:**
```roc
Maybe(a) : [Ok(a), Err]
```
^^^^^^^^^^^^^^^^^^^^^^^

But _Maybe_ was already declared here:
**type_redeclaration_same_scope.md:1:1:1:27:**
```roc
Maybe(a) : [Some(a), None]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `type_redeclaration_same_scope.roc`, but no top-level type declaration named `type_redeclaration_same_scope` was found.

Add either:
`type_redeclaration_same_scope := ...` (nominal type)
or:
`type_redeclaration_same_scope : ...` (type alias)
**type_redeclaration_same_scope.md:1:1:2:24:**
```roc
Maybe(a) : [Some(a), None]
Maybe(a) : [Ok(a), Err]
```


# TOKENS
~~~zig
UpperIdent(1:1-1:6),NoSpaceOpenRound(1:6-1:7),LowerIdent(1:7-1:8),CloseRound(1:8-1:9),OpColon(1:10-1:11),OpenSquare(1:12-1:13),UpperIdent(1:13-1:17),NoSpaceOpenRound(1:17-1:18),LowerIdent(1:18-1:19),CloseRound(1:19-1:20),Comma(1:20-1:21),UpperIdent(1:22-1:26),CloseSquare(1:26-1:27),
UpperIdent(2:1-2:6),NoSpaceOpenRound(2:6-2:7),LowerIdent(2:7-2:8),CloseRound(2:8-2:9),OpColon(2:10-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:15),NoSpaceOpenRound(2:15-2:16),LowerIdent(2:16-2:17),CloseRound(2:17-2:18),Comma(2:18-2:19),UpperIdent(2:20-2:23),CloseSquare(2:23-2:24),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.24
	(type-module @1.1-1.6)
	(statements
		(s-type-decl @1.1-1.27
			(header @1.1-1.9 (name "Maybe")
				(args
					(ty-var @1.7-1.8 (raw "a"))))
			(ty-tag-union @1.12-1.27
				(tags
					(ty-apply @1.13-1.20
						(ty @1.13-1.17 (name "Some"))
						(ty-var @1.18-1.19 (raw "a")))
					(ty @1.22-1.26 (name "None")))))
		(s-type-decl @2.1-2.24
			(header @2.1-2.9 (name "Maybe")
				(args
					(ty-var @2.7-2.8 (raw "a"))))
			(ty-tag-union @2.12-2.24
				(tags
					(ty-apply @2.13-2.18
						(ty @2.13-2.15 (name "Ok"))
						(ty-var @2.16-2.17 (raw "a")))
					(ty @2.20-2.23 (name "Err")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.1-1.27
		(ty-header @1.1-1.9 (name "Maybe")
			(ty-args
				(ty-var @1.7-1.8 (name "a"))))
		(ty-tag-union @1.12-1.27
			(ty-apply @1.13-1.20 (symbol "Some")
				(ty-var @1.18-1.19 (name "a")))
			(ty @1.22-1.26 (name "None"))))
	(s-alias-decl @2.1-2.24
		(ty-header @2.1-2.9 (name "Maybe")
			(ty-args
				(ty-var @2.7-2.8 (name "a"))))
		(ty-tag-union @2.12-2.24
			(ty-apply @2.13-2.18 (symbol "Ok")
				(ty-var @2.16-2.17 (name "a")))
			(ty @2.20-2.23 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.1-1.27 (type "Maybe(a)")
			(ty-header @1.1-1.9 (name "Maybe")
				(ty-args
					(ty-var @1.7-1.8 (name "a")))))
		(alias @2.1-2.24 (type "Maybe(a)")
			(ty-header @2.1-2.9 (name "Maybe")
				(ty-args
					(ty-var @2.7-2.8 (name "a"))))))
	(expressions))
~~~
