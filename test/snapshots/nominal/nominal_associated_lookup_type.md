# META
~~~ini
description=Type annotation referencing nested type in associated block
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something]
}

myBar : Foo.Bar
myBar = Something
~~~
# EXPECTED
TYPE MISMATCH - nominal_associated_lookup_type.md:6:9:6:18
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_lookup_type.md:6:9:6:18:**
```roc
myBar = Something
```
        ^^^^^^^^^

It has the type:
    _[Something]_others_

But the type annotation says it should have the type:
    _Foo.Bar_

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,
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
							(ty (name "Something")))))))
		(s-type-anno (name "myBar")
			(ty (name "Foo.Bar")))
		(s-decl
			(p-ident (raw "myBar"))
			(e-tag (raw "Something")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [Something]
}

myBar : Foo.Bar
myBar = Something
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "myBar"))
		(e-tag (name "Something"))
		(annotation
			(declared-type
				(ty-lookup (name "Foo.Bar") (local)))))
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
		(patt (type "Error")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar"))))
	(expressions
		(expr (type "Error"))))
~~~
