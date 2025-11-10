# META
~~~ini
description=Nominal type with multi-statement associated items
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [A, B, C].{
    x = 5
    y = 10
    z = 15
}
~~~
# EXPECTED
UNUSED VARIABLE - nominal_type_with_associated_multi_statement.md:2:5:2:6
UNUSED VARIABLE - nominal_type_with_associated_multi_statement.md:3:5:3:6
UNUSED VARIABLE - nominal_type_with_associated_multi_statement.md:4:5:4:6
# PROBLEMS
**UNUSED VARIABLE**
Variable `Foo.x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Foo.x` to suppress this warning.
The unused variable is declared here:
**nominal_type_with_associated_multi_statement.md:2:5:2:6:**
```roc
    x = 5
```
    ^


**UNUSED VARIABLE**
Variable `Foo.y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Foo.y` to suppress this warning.
The unused variable is declared here:
**nominal_type_with_associated_multi_statement.md:3:5:3:6:**
```roc
    y = 10
```
    ^


**UNUSED VARIABLE**
Variable `Foo.z` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Foo.z` to suppress this warning.
The unused variable is declared here:
**nominal_type_with_associated_multi_statement.md:4:5:4:6:**
```roc
    z = 15
```
    ^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
CloseCurly,
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
					(e-int (raw "5")))
				(s-decl
					(p-ident (raw "y"))
					(e-int (raw "10")))
				(s-decl
					(p-ident (raw "z"))
					(e-int (raw "15")))))))
~~~
# FORMATTED
~~~roc
Foo := [A, B, C].{
	x = 5
	y = 10
	z = 15
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.x"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "Foo.y"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "Foo.z"))
		(e-num (value "15")))
	(s-nominal-decl
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
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))))
~~~
