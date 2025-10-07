# META
~~~ini
description=Nominal type with declarations in associated blocks
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something].{
        baz = 5
    }
    blah = 6
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:22),CloseSquare(2:22-2:23),Dot(2:23-2:24),OpenCurly(2:24-2:25),
LowerIdent(3:9-3:12),OpAssign(3:13-3:14),Int(3:15-3:16),
CloseCurly(4:5-4:6),
LowerIdent(5:5-5:9),OpAssign(5:10-5:11),Int(5:12-5:13),
CloseCurly(6:1-6:2),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.2
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-6.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-6.2
				(s-type-decl @2.5-4.6
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-tag-union @2.12-2.23
						(tags
							(ty @2.13-2.22 (name "Something"))))
					(associated @2.24-4.6
						(s-decl @3.9-3.16
							(p-ident @3.9-3.12 (raw "baz"))
							(e-int @3.15-3.16 (raw "5")))))
				(s-decl @5.5-5.13
					(p-ident @5.5-5.9 (raw "blah"))
					(e-int @5.12-5.13 (raw "6")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [Something].{
		baz = 5
	}
	blah = 6
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-6.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-4.6
		(ty-header @2.5-2.8 (name "Bar"))
		(ty-tag-union @2.12-2.23
			(ty-tag-name @2.13-2.22 (name "Something")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-6.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-4.6 (type "Bar")
			(ty-header @2.5-2.8 (name "Bar"))))
	(expressions))
~~~
