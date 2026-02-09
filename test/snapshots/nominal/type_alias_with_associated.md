# META
~~~ini
description=Type alias with associated items produces error
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo : [A, B, C].{ x = 5 }
~~~
# EXPECTED
TYPE ALIAS WITH ASSOCIATED ITEMS - type_alias_with_associated.md:1:16:1:17
TYPE MODULE REQUIRES NOMINAL TYPE - type_alias_with_associated.md:1:1:1:26
# PROBLEMS
**TYPE ALIAS WITH ASSOCIATED ITEMS**
Type aliases cannot have associated items (such as types or methods).

Only nominal types (defined with **:=**) can have associated items. Type aliases (defined with **:**) only define names for other types.

**type_alias_with_associated.md:1:16:1:17:**
```roc
Foo : [A, B, C].{ x = 5 }
```
               ^


**TYPE MODULE REQUIRES NOMINAL TYPE**
This file is named `Foo`.roc, and contains a type alias `Foo`.

Type modules must use nominal types (`:=` or `::`), not type aliases (`:`).

Nominal types must be records or tag unions:

# Record example:
`Foo := { data: List(U8) }.{}`

# Tag union example:
`Foo := [ State(List(U8)) ].{}`

Tip: Nominal types have their own identity and can have associated functions. Type aliases (`:`) are just shorthand for another type and cannot define modules.
**type_alias_with_associated.md:1:1:1:26:**
```roc
Foo : [A, B, C].{ x = 5 }
```
^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,LowerIdent,OpAssign,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))
					(ty (name "B"))
					(ty (name "C"))))
			(associated
				(s-decl
					(p-ident (raw "x"))
					(e-int (raw "5")))))))
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
		(p-assign (ident "Foo.x"))
		(e-num (value "5")))
	(s-alias-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "A"))
			(ty-tag-name (name "B"))
			(ty-tag-name (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec")))
	(type_decls
		(alias (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Dec"))))
~~~
