# META
~~~ini
description=Deeply nested types (3+ levels) in associated blocks
type=file
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something].{
        Baz := [Else].{
            Qux := [Deep].{
                w = 1
            }
            z = 2
        }
        y = 3
    }
    x = 4
}
~~~
# EXPECTED
TYPE MODULE MISSING MATCHING TYPE - nominal_deeply_nested_types.md:1:1:12:2
# PROBLEMS
**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `nominal_deeply_nested_types`.roc, but no top-level type declaration named `nominal_deeply_nested_types` was found.

Add either:
`nominal_deeply_nested_types := ...` (nominal type)
or:
`nominal_deeply_nested_types : ...` (type alias)
**nominal_deeply_nested_types.md:1:1:12:2:**
```roc
Foo := [Whatever].{
    Bar := [Something].{
        Baz := [Else].{
            Qux := [Deep].{
                w = 1
            }
            z = 2
        }
        y = 3
    }
    x = 4
}
```


# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:22),CloseSquare(2:22-2:23),Dot(2:23-2:24),OpenCurly(2:24-2:25),
UpperIdent(3:9-3:12),OpColonEqual(3:13-3:15),OpenSquare(3:16-3:17),UpperIdent(3:17-3:21),CloseSquare(3:21-3:22),Dot(3:22-3:23),OpenCurly(3:23-3:24),
UpperIdent(4:13-4:16),OpColonEqual(4:17-4:19),OpenSquare(4:20-4:21),UpperIdent(4:21-4:25),CloseSquare(4:25-4:26),Dot(4:26-4:27),OpenCurly(4:27-4:28),
LowerIdent(5:17-5:18),OpAssign(5:19-5:20),Int(5:21-5:22),
CloseCurly(6:13-6:14),
LowerIdent(7:13-7:14),OpAssign(7:15-7:16),Int(7:17-7:18),
CloseCurly(8:9-8:10),
LowerIdent(9:9-9:10),OpAssign(9:11-9:12),Int(9:13-9:14),
CloseCurly(10:5-10:6),
LowerIdent(11:5-11:6),OpAssign(11:7-11:8),Int(11:9-11:10),
CloseCurly(12:1-12:2),
EndOfFile(13:1-13:1),
~~~
# PARSE
~~~clojure
(file @1.1-12.2
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-12.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-12.2
				(s-type-decl @2.5-10.6
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-tag-union @2.12-2.23
						(tags
							(ty @2.13-2.22 (name "Something"))))
					(associated @2.24-10.6
						(s-type-decl @3.9-8.10
							(header @3.9-3.12 (name "Baz")
								(args))
							(ty-tag-union @3.16-3.22
								(tags
									(ty @3.17-3.21 (name "Else"))))
							(associated @3.23-8.10
								(s-type-decl @4.13-6.14
									(header @4.13-4.16 (name "Qux")
										(args))
									(ty-tag-union @4.20-4.26
										(tags
											(ty @4.21-4.25 (name "Deep"))))
									(associated @4.27-6.14
										(s-decl @5.17-5.22
											(p-ident @5.17-5.18 (raw "w"))
											(e-int @5.21-5.22 (raw "1")))))
								(s-decl @7.13-7.18
									(p-ident @7.13-7.14 (raw "z"))
									(e-int @7.17-7.18 (raw "2")))))
						(s-decl @9.9-9.14
							(p-ident @9.9-9.10 (raw "y"))
							(e-int @9.13-9.14 (raw "3")))))
				(s-decl @11.5-11.10
					(p-ident @11.5-11.6 (raw "x"))
					(e-int @11.9-11.10 (raw "4")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [Something].{
		Baz := [Else].{
			Qux := [Deep].{
				w = 1
			}
			z = 2
		}
		y = 3
	}
	x = 4
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-12.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-12.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo"))))
	(expressions))
~~~
