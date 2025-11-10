# META
~~~ini
description=Nested types in associated blocks
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something].{
        y = 6
    }
    x = 5
}
~~~
# EXPECTED
UNUSED VARIABLE - nominal_nested_types.md:3:9:3:10
UNUSED VARIABLE - nominal_nested_types.md:3:9:3:10
UNUSED VARIABLE - nominal_nested_types.md:5:5:5:6
# PROBLEMS
**UNUSED VARIABLE**
Variable `Bar.y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Bar.y` to suppress this warning.
The unused variable is declared here:
**nominal_nested_types.md:3:9:3:10:**
```roc
        y = 6
```
        ^


**UNUSED VARIABLE**
Variable `Bar.y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Bar.y` to suppress this warning.
The unused variable is declared here:
**nominal_nested_types.md:3:9:3:10:**
```roc
        y = 6
```
        ^


**UNUSED VARIABLE**
Variable `Foo.x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Foo.x` to suppress this warning.
The unused variable is declared here:
**nominal_nested_types.md:5:5:5:6:**
```roc
    x = 5
```
    ^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
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
					(ty (name "Whatever"))))
			(associated
				(s-type-decl
					(header (name "Bar")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Something"))))
					(associated
						(s-decl
							(p-ident (raw "y"))
							(e-int (raw "6")))))
				(s-decl
					(p-ident (raw "x"))
					(e-int (raw "5")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [Something].{
		y = 6
	}
	x = 5
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.Bar.y"))
		(e-num (value "6")))
	(d-let
		(p-assign (ident "Foo.x"))
		(e-num (value "5")))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "Something")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar"))))
	(expressions
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))))
~~~
