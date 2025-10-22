# META
~~~ini
description=Very deep nesting of associated items (4 levels)
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Level1 := [A].{
        Level2 := [B].{
            Level3 := [C].{
                value = 42
            }
        }
    }
}

deepValue : U64
deepValue = Foo.Level1.Level2.Level3.value

deepType : Foo.Level1.Level2.Level3
deepType = C
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
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
					(header (name "Level1")
						(args))
					(ty-tag-union
						(tags
							(ty (name "A"))))
					(associated
						(s-type-decl
							(header (name "Level2")
								(args))
							(ty-tag-union
								(tags
									(ty (name "B"))))
							(associated
								(s-type-decl
									(header (name "Level3")
										(args))
									(ty-tag-union
										(tags
											(ty (name "C"))))
									(associated
										(s-decl
											(p-ident (raw "value"))
											(e-int (raw "42")))))))))))
		(s-type-anno (name "deepValue")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "deepValue"))
			(e-ident (raw "Foo.Level1.Level2.Level3.value")))
		(s-type-anno (name "deepType")
			(ty (name "Foo.Level1.Level2.Level3")))
		(s-decl
			(p-ident (raw "deepType"))
			(e-tag (raw "C")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Level1 := [A].{
		Level2 := [B].{
			Level3 := [C].{
				value = 42
			}
		}
	}
}

deepValue : U64
deepValue = Foo.Level1.Level2.Level3.value

deepType : Foo.Level1.Level2.Level3
deepType = C
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "deepValue"))
		(e-lookup-local
			(p-assign (ident "Foo.Level1.Level2.Level3.value")))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "deepType"))
		(e-tag (name "C"))
		(annotation
			(ty-lookup (name "Foo.Level1.Level2.Level3") (local))))
	(d-let
		(p-assign (ident "Foo.Level1.Level2.Level3.value"))
		(e-num (value "42")))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Foo.Level1"))
		(ty-tag-union
			(ty-tag-name (name "A"))))
	(s-nominal-decl
		(ty-header (name "Foo.Level1.Level2"))
		(ty-tag-union
			(ty-tag-name (name "B"))))
	(s-nominal-decl
		(ty-header (name "Foo.Level1.Level2.Level3"))
		(ty-tag-union
			(ty-tag-name (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(Unsigned64))"))
		(patt (type "Foo.Level1.Level2.Level3"))
		(patt (type "Num(Int(Unsigned64))")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Level1")
			(ty-header (name "Foo.Level1")))
		(nominal (type "Foo.Level1.Level2")
			(ty-header (name "Foo.Level1.Level2")))
		(nominal (type "Foo.Level1.Level2.Level3")
			(ty-header (name "Foo.Level1.Level2.Level3"))))
	(expressions
		(expr (type "Num(Int(Unsigned64))"))
		(expr (type "Foo.Level1.Level2.Level3"))
		(expr (type "Num(Int(Unsigned64))"))))
~~~
