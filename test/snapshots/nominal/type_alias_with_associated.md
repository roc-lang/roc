# META
~~~ini
description=Type alias with associated items produces error
type=file
~~~
# SOURCE
~~~roc
module []

Foo : [A, B, C].{ x = 5 }
~~~
# EXPECTED
TYPE ALIAS WITH ASSOCIATED ITEMS - type_alias_with_associated.md:3:16:3:17
# PROBLEMS
**TYPE ALIAS WITH ASSOCIATED ITEMS**
Type aliases cannot have associated items (such as types or methods).

Only nominal types (defined with **:=**) can have associated items. Type aliases (defined with **:**) only define names for other types.

**type_alias_with_associated.md:3:16:3:17:**
```roc
Foo : [A, B, C].{ x = 5 }
```
               ^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:4),OpColon(3:5-3:6),OpenSquare(3:7-3:8),UpperIdent(3:8-3:9),Comma(3:9-3:10),UpperIdent(3:11-3:12),Comma(3:12-3:13),UpperIdent(3:14-3:15),CloseSquare(3:15-3:16),Dot(3:16-3:17),OpenCurly(3:17-3:18),LowerIdent(3:19-3:20),OpAssign(3:21-3:22),Int(3:23-3:24),CloseCurly(3:25-3:26),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.26
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.26
			(header @3.1-3.4 (name "Foo")
				(args))
			(ty-tag-union @3.7-3.16
				(tags
					(ty @3.8-3.9 (name "A"))
					(ty @3.11-3.12 (name "B"))
					(ty @3.14-3.15 (name "C")))))))
~~~
# FORMATTED
~~~roc
module []

Foo : [A, B, C].{
	x = 5
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-3.26
		(ty-header @3.1-3.4 (name "Foo"))
		(ty-tag-union @3.7-3.16
			(ty-tag-name @3.8-3.9 (name "A"))
			(ty-tag-name @3.11-3.12 (name "B"))
			(ty-tag-name @3.14-3.15 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @3.1-3.26 (type "Foo")
			(ty-header @3.1-3.4 (name "Foo"))))
	(expressions))
~~~
