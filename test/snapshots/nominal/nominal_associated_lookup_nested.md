# META
~~~ini
description=Referencing deeply nested items from associated blocks
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something].{
        baz = 5
    }
}

myType : Foo.Bar
myType = Something

myNum : U64
myNum = Foo.Bar.baz
~~~
# EXPECTED
TYPE MISMATCH - nominal_associated_lookup_nested.md:8:10:8:19
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_lookup_nested.md:8:10:8:19:**
```roc
myType = Something
```
         ^^^^^^^^^

It has the type:
    _[Something]_others_

But the type annotation says it should have the type:
    _Foo.Bar_

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
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
							(p-ident (raw "baz"))
							(e-int (raw "5")))))))
		(s-type-anno (name "myType")
			(ty (name "Foo.Bar")))
		(s-decl
			(p-ident (raw "myType"))
			(e-tag (raw "Something")))
		(s-type-anno (name "myNum")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "myNum"))
			(e-ident (raw "Foo.Bar.baz")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [Something].{
		baz = 5
	}
}

myType : Foo.Bar
myType = Something

myNum : U64
myNum = Foo.Bar.baz
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "myType"))
		(e-tag (name "Something"))
		(annotation
			(declared-type
				(ty-lookup (name "Foo.Bar") (local)))))
	(d-let
		(p-assign (ident "myNum"))
		(e-lookup-local
			(p-assign (ident "Foo.Bar.baz")))
		(annotation
			(declared-type
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "Foo.Bar.baz"))
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
		(patt (type "Error"))
		(patt (type "Num(Int(Unsigned64))"))
		(patt (type "Num(Int(Unsigned64))")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar"))))
	(expressions
		(expr (type "Error"))
		(expr (type "Num(Int(Unsigned64))"))
		(expr (type "Num(Int(Unsigned64))"))))
~~~
