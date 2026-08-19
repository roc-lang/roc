# META
~~~ini
description=A forward reference prepares a nested type declaration as a placeholder; when its owner is then redeclared, the owner's associated block is skipped and the placeholder survives to end of mod unfilled. The defaulted-fields passes and the checker's decl generation must treat it as declaring nothing (its `.placeholder` anno used to be read as a TypeAnno and crash).
type=snippet
~~~
# SOURCE
~~~roc
f : Foo.Baz -> U8
f = |_| 1

g : Foo.Qux -> U8
g = |_| 2

Foo := [A].{
    Bar := { x : U8 }
}

Foo := [B].{
    Baz := { y : U8 }
    Qux : { z : U8 }
}
~~~
# EXPECTED
TYPE REDECLARED - record_default_forward_placeholder_survives.md:11:1:14:2
# PROBLEMS
── ✗ type redeclared ─────── record_default_forward_placeholder_survives.md:11:1

The type Foo is being redeclared.

Foo := [B].{
    Baz := { y : U8 }
    Qux : { z : U8 }
}

But Foo was already declared in record_default_forward_placeholder_survives.md:7:1:

Foo := [A].{
    Bar := { x : U8 }
}

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,Int,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "f")
			(ty-fn
				(ty (name "Foo.Baz"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-underscore))
				(e-int (raw "1"))))
		(s-type-anno (name "g")
			(ty-fn
				(ty (name "Foo.Qux"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "g"))
			(e-lambda
				(args
					(p-underscore))
				(e-int (raw "2"))))
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))))
			(associated
				(s-type-decl
					(header (name "Bar")
						(args))
					(ty-record
						(anno-record-field (name "x")
							(ty (name "U8")))))))
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "B"))))
			(associated
				(s-type-decl
					(header (name "Baz")
						(args))
					(ty-record
						(anno-record-field (name "y")
							(ty (name "U8")))))
				(s-type-decl
					(header (name "Qux")
						(args))
					(ty-record
						(anno-record-field (name "z")
							(ty (name "U8")))))))))
~~~
# FORMATTED
~~~roc
f : Foo.Baz -> U8
f = |_| 1

g : Foo.Qux -> U8
g = |_| 2

Foo := [A].{
	Bar := { x : U8 }
}

Foo := [B].{
	Baz := { y : U8 }
	Qux : { z : U8 }
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Foo.Baz") (local))
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "g"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Foo.Qux") (local))
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "A"))))
	(s-nominal-decl
		(ty-header (name "record_default_forward_placeholder_survives.Foo.Bar"))
		(ty-record
			(field (field "x")
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "B")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> U8"))
		(patt (type "Error -> U8")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "record_default_forward_placeholder_survives.Foo.Bar")))
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Error -> U8"))
		(expr (type "Error -> U8"))))
~~~
