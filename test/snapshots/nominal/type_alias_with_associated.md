# META
~~~ini
description=Type alias with associated items produces error
type=snippet
~~~
# SOURCE
~~~roc
Foo : [A, B, C].{ x = 5 }
~~~
# EXPECTED
TYPE ALIAS WITH ASSOCIATED ITEMS - type_alias_with_associated.md:1:16:1:17
UNUSED VARIABLE - type_alias_with_associated.md:1:19:1:24
# PROBLEMS
**TYPE ALIAS WITH ASSOCIATED ITEMS**
Type aliases cannot have associated items (such as types or methods).

Only nominal types (defined with **:=**) can have associated items. Type aliases (defined with **:**) only define names for other types.

**type_alias_with_associated.md:1:16:1:17:**
```roc
Foo : [A, B, C].{ x = 5 }
```
               ^


**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**type_alias_with_associated.md:1:19:1:24:**
```roc
Foo : [A, B, C].{ x = 5 }
```
                  ^^^^^


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
					(ty @1.14-1.15 (name "C"))))
			(associated @1.17-1.26
				(s-decl @1.19-1.24
					(p-ident @1.19-1.20 (raw "x"))
					(e-int @1.23-1.24 (raw "5")))))))
~~~
# FORMATTED
~~~roc
Foo : [A, B, C].{
	x = 5
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.19-1.24 (ident "Foo.x"))
		(e-num @1.23-1.24 (value "5")))
	(s-alias-decl @1.1-1.26
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.7-1.16
			(ty-tag-name @1.8-1.9 (name "A"))
			(ty-tag-name @1.11-1.12 (name "B"))
			(ty-tag-name @1.14-1.15 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.19-1.24 (type "Num(_size)")))
	(type_decls
		(alias @1.1-1.26 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo"))))
	(expressions
		(expr @1.23-1.24 (type "Num(_size)"))))
~~~
