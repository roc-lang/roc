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
TYPE MISMATCH - nominal_associated_deep_nesting.md:15:12:15:13
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_deep_nesting.md:15:12:15:13:**
```roc
deepType = C
```
           ^

It has the type:
    _[C]_others_

But the type annotation says it should have the type:
    _Foo.Level1.Level2.Level3_

# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:11),OpColonEqual(2:12-2:14),OpenSquare(2:15-2:16),UpperIdent(2:16-2:17),CloseSquare(2:17-2:18),Dot(2:18-2:19),OpenCurly(2:19-2:20),
UpperIdent(3:9-3:15),OpColonEqual(3:16-3:18),OpenSquare(3:19-3:20),UpperIdent(3:20-3:21),CloseSquare(3:21-3:22),Dot(3:22-3:23),OpenCurly(3:23-3:24),
UpperIdent(4:13-4:19),OpColonEqual(4:20-4:22),OpenSquare(4:23-4:24),UpperIdent(4:24-4:25),CloseSquare(4:25-4:26),Dot(4:26-4:27),OpenCurly(4:27-4:28),
LowerIdent(5:17-5:22),OpAssign(5:23-5:24),Int(5:25-5:27),
CloseCurly(6:13-6:14),
CloseCurly(7:9-7:10),
CloseCurly(8:5-8:6),
CloseCurly(9:1-9:2),
LowerIdent(11:1-11:10),OpColon(11:11-11:12),UpperIdent(11:13-11:16),
LowerIdent(12:1-12:10),OpAssign(12:11-12:12),UpperIdent(12:13-12:16),NoSpaceDotUpperIdent(12:16-12:23),NoSpaceDotUpperIdent(12:23-12:30),NoSpaceDotUpperIdent(12:30-12:37),NoSpaceDotLowerIdent(12:37-12:43),
LowerIdent(14:1-14:9),OpColon(14:10-14:11),UpperIdent(14:12-14:15),NoSpaceDotUpperIdent(14:15-14:22),NoSpaceDotUpperIdent(14:22-14:29),NoSpaceDotUpperIdent(14:29-14:36),
LowerIdent(15:1-15:9),OpAssign(15:10-15:11),UpperIdent(15:12-15:13),
EndOfFile(16:1-16:1),
~~~
# PARSE
~~~clojure
(file @1.1-15.13
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-9.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-9.2
				(s-type-decl @2.5-8.6
					(header @2.5-2.11 (name "Level1")
						(args))
					(ty-tag-union @2.15-2.18
						(tags
							(ty @2.16-2.17 (name "A"))))
					(associated @2.19-8.6
						(s-type-decl @3.9-7.10
							(header @3.9-3.15 (name "Level2")
								(args))
							(ty-tag-union @3.19-3.22
								(tags
									(ty @3.20-3.21 (name "B"))))
							(associated @3.23-7.10
								(s-type-decl @4.13-6.14
									(header @4.13-4.19 (name "Level3")
										(args))
									(ty-tag-union @4.23-4.26
										(tags
											(ty @4.24-4.25 (name "C"))))
									(associated @4.27-6.14
										(s-decl @5.17-5.27
											(p-ident @5.17-5.22 (raw "value"))
											(e-int @5.25-5.27 (raw "42")))))))))))
		(s-type-anno @11.1-11.16 (name "deepValue")
			(ty @11.13-11.16 (name "U64")))
		(s-decl @12.1-12.43
			(p-ident @12.1-12.10 (raw "deepValue"))
			(e-ident @12.13-12.43 (raw "Foo.Level1.Level2.Level3.value")))
		(s-type-anno @14.1-14.36 (name "deepType")
			(ty @14.12-14.36 (name "Foo.Level1.Level2.Level3")))
		(s-decl @15.1-15.13
			(p-ident @15.1-15.9 (raw "deepType"))
			(e-tag @15.12-15.13 (raw "C")))))
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
		(p-assign @12.1-12.10 (ident "deepValue"))
		(e-lookup-local @12.13-12.43
			(p-assign @5.17-5.27 (ident "Foo.Level1.Level2.Level3.value")))
		(annotation @12.1-12.10
			(declared-type
				(ty-lookup @11.13-11.16 (name "U64") (builtin)))))
	(d-let
		(p-assign @15.1-15.9 (ident "deepType"))
		(e-tag @15.12-15.13 (name "C"))
		(annotation @15.1-15.9
			(declared-type
				(ty-lookup @14.12-14.36 (name "Foo.Level1.Level2.Level3") (local)))))
	(d-let
		(p-assign @5.17-5.27 (ident "Foo.Level1.Level2.Level3.value"))
		(e-num @5.25-5.27 (value "42")))
	(s-nominal-decl @1.1-9.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-8.6
		(ty-header @2.5-8.6 (name "Foo.Level1"))
		(ty-tag-union @2.15-2.18
			(ty-tag-name @2.16-2.17 (name "A"))))
	(s-nominal-decl @3.9-7.10
		(ty-header @3.9-7.10 (name "Foo.Level1.Level2"))
		(ty-tag-union @3.19-3.22
			(ty-tag-name @3.20-3.21 (name "B"))))
	(s-nominal-decl @4.13-6.14
		(ty-header @4.13-6.14 (name "Foo.Level1.Level2.Level3"))
		(ty-tag-union @4.23-4.26
			(ty-tag-name @4.24-4.25 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @12.1-12.10 (type "Num(_size)"))
		(patt @15.1-15.9 (type "Error"))
		(patt @5.17-5.27 (type "Num(_size)")))
	(type_decls
		(nominal @1.1-9.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-8.6 (type "Foo.Level1")
			(ty-header @2.5-8.6 (name "Foo.Level1")))
		(nominal @3.9-7.10 (type "Foo.Level1.Level2")
			(ty-header @3.9-7.10 (name "Foo.Level1.Level2")))
		(nominal @4.13-6.14 (type "Foo.Level1.Level2.Level3")
			(ty-header @4.13-6.14 (name "Foo.Level1.Level2.Level3"))))
	(expressions
		(expr @12.13-12.43 (type "Num(_size)"))
		(expr @15.12-15.13 (type "Error"))
		(expr @5.25-5.27 (type "Num(_size)"))))
~~~
