# META
~~~ini
description=Qualified types in tuples and as type arguments
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [X, Y]
    Baz := [Z]
}

pair : (Foo.Bar, Foo.Baz)
pair = (X, Z)

Box : a -> [Box(a)]

boxed : Box(Foo.Bar)
boxed = Box(X)
~~~
# EXPECTED
SHADOWING - nominal_associated_in_tuples.md:9:1:9:4
TYPE MISMATCH - nominal_associated_in_tuples.md:12:9:12:15
# PROBLEMS
**SHADOWING**
The name `Box` is already defined in this scope.

Choose a different name for this identifier.

**nominal_associated_in_tuples.md:9:1:9:4:**
```roc
Box : a -> [Box(a)]
```
^^^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_in_tuples.md:12:9:12:15:**
```roc
boxed = Box(X)
```
        ^^^^^^

It has the type:
    _[Box([X]_others)]_others2_

But the type annotation says it should have the type:
    _Box(Foo.Bar)_

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,
CloseCurly,
LowerIdent,OpColon,OpenRound,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
UpperIdent,OpColon,LowerIdent,OpArrow,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
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
							(ty (name "X"))
							(ty (name "Y")))))
				(s-type-decl
					(header (name "Baz")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Z")))))))
		(s-type-anno (name "pair")
			(ty-tuple
				(ty (name "Foo.Bar"))
				(ty (name "Foo.Baz"))))
		(s-decl
			(p-ident (raw "pair"))
			(e-tuple
				(e-tag (raw "X"))
				(e-tag (raw "Z"))))
		(s-type-decl
			(header (name "Box")
				(args))
			(ty-fn
				(ty-var (raw "a"))
				(ty-tag-union
					(tags
						(ty-apply
							(ty (name "Box"))
							(ty-var (raw "a")))))))
		(s-type-anno (name "boxed")
			(ty-apply
				(ty (name "Box"))
				(ty (name "Foo.Bar"))))
		(s-decl
			(p-ident (raw "boxed"))
			(e-apply
				(e-tag (raw "Box"))
				(e-tag (raw "X"))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [X, Y]
	Baz := [Z]
}

pair : (Foo.Bar, Foo.Baz)
pair = (X, Z)

Box : a -> [Box(a)]

boxed : Box(Foo.Bar)
boxed = Box(X)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "pair"))
		(e-tuple
			(elems
				(e-tag (name "X"))
				(e-tag (name "Z"))))
		(annotation
			(ty-tuple
				(ty-lookup (name "Foo.Bar") (local))
				(ty-lookup (name "Foo.Baz") (local)))))
	(d-let
		(p-assign (ident "boxed"))
		(e-tag (name "Box")
			(args
				(e-tag (name "X"))))
		(annotation
			(ty-apply (name "Box") (builtin)
				(ty-lookup (name "Foo.Bar") (local)))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "X"))
			(ty-tag-name (name "Y"))))
	(s-nominal-decl
		(ty-header (name "Foo.Baz"))
		(ty-tag-union
			(ty-tag-name (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Foo.Bar, Foo.Baz)"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar")))
		(nominal (type "Foo.Baz")
			(ty-header (name "Foo.Baz"))))
	(expressions
		(expr (type "(Foo.Bar, Foo.Baz)"))
		(expr (type "Error"))))
~~~
