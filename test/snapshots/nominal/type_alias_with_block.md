# META
~~~ini
description=Type alias with block produces error
type=file
~~~
# SOURCE
~~~roc
Foo : [A, B, C].{ x = 5 }
~~~
# EXPECTED
TYPE ALIAS WITH ASSOCIATED ITEMS - type_alias_with_block.md:1:16:1:17
TYPE MODULE MISSING MATCHING TYPE - type_alias_with_block.md:1:1:1:26
# PROBLEMS
**TYPE ALIAS WITH ASSOCIATED ITEMS**
Type aliases cannot have associated items (such as types or methods).

Only nominal types (defined with **:=**) can have associated items. Type aliases (defined with **:**) only define names for other types.

**type_alias_with_block.md:1:16:1:17:**
```roc
Foo : [A, B, C].{ x = 5 }
```
               ^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `type_alias_with_block.roc`, but no top-level type declaration named `type_alias_with_block` was found.

Add either:
`type_alias_with_block := ...` (nominal type)
or:
`type_alias_with_block : ...` (type alias)
**type_alias_with_block.md:1:1:1:26:**
```roc
Foo : [A, B, C].{ x = 5 }
```
^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColon(1:5-1:6),OpenSquare(1:7-1:8),UpperIdent(1:8-1:9),Comma(1:9-1:10),UpperIdent(1:11-1:12),Comma(1:12-1:13),UpperIdent(1:14-1:15),CloseSquare(1:15-1:16),Dot(1:16-1:17),OpenCurly(1:17-1:18),LowerIdent(1:19-1:20),OpAssign(1:21-1:22),Int(1:23-1:24),CloseCurly(1:25-1:26),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.26
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-1.26
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.7-1.16
				(tags
					(ty @1.8-1.9 (name "A"))
					(ty @1.11-1.12 (name "B"))
					(ty @1.14-1.15 (name "C")))))))
~~~
# FORMATTED
~~~roc
Foo : [A, B, C]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.1-1.26
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.7-1.16
			(ty @1.8-1.9 (name "A"))
			(ty @1.11-1.12 (name "B"))
			(ty @1.14-1.15 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.1-1.26 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo"))))
	(expressions))
~~~
